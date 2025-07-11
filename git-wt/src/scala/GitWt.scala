import java.nio.file.Paths

import zio.*
import zio.cli.*
import zio.cli.HelpDoc.Span.text
import zio.process.Command as ZCommand


object GitWt extends ZIOCliDefault {
  sealed trait Subcommand extends Product with Serializable
  private object Subcommand {
    final case class New(branch: String)         extends Subcommand
    final case class Add(branches: List[String]) extends Subcommand
    final case class Path(branch: String)        extends Subcommand
    case object Prune                            extends Subcommand
  }

  private def slugify(s: String): String =
    s.foldLeft(List.empty[Char]) {
      case (acc, '/') if !acc.endsWith("-")                   => acc :+ '-'
      case (acc, ch) if !acc.endsWith("-") && ch.isWhitespace => acc :+ '-'
      case (acc, ch @ ('.' | '_'))                            => acc :+ ch
      case (acc, ch) if ch.isLetterOrDigit                    => acc :+ ch
      case (acc, _)                                           => acc
    }.mkString.stripPrefix("-").stripSuffix("-")

  private def exec(
    command: String,
    args: String*
  ): ZIO[Any, Throwable, String] = ZCommand(command, args*).string

  override val cliApp =
    CliApp.make(
      name = "git-wt",
      version = "0.1.0",
      summary = text("A lightweight worktree manager"),
      command = Command("git-wt", Options.none)
        .subcommands(
          Command("new", Args.text("branch")).map(Subcommand.New.apply),
          Command("add", Args.text("branch-or-pr").*).map(Subcommand.Add.apply),
          Command("path", Args.text("branch")).map(Subcommand.Path.apply),
          Command("prune").map(_ => Subcommand.Prune)
        )
    ) {
      case Subcommand.New(branch)   =>
        for {
          gitRoot      <- exec("git", "rev-parse", "--show-toplevel")
          repoName      = Paths.get(gitRoot).getFileName.toString
          dirHash      <- exec("git", "hash-object", "--stdin", gitRoot)
          worktreesRoot = Paths.get(
                            sys.props("user.home"),
                            ".worktrees",
                            s"$repoName-${dirHash.take(7)}"
                          )
          branchSlug    = slugify(branch)
          worktreePath  = worktreesRoot.resolve(branchSlug)
          _            <- ZIO.succeed(os.makeDir.all(os.Path(worktreesRoot.toFile)))
          _            <- exec("git", "branch", branch, "HEAD")
          _            <- exec("git", "worktree", "add", worktreePath.toString, branch)
          _            <- Console.printLine(worktreePath.toString)
        } yield ()
      case Subcommand.Add(branches) =>
        for {
          gitRoot      <- exec("git", "rev-parse", "--show-toplevel")
          repoName      = Paths.get(gitRoot).getFileName.toString
          dirHash      <- exec("git", "hash-object", "--stdin", gitRoot)
          worktreesRoot = Paths.get(
                            sys.props("user.home"),
                            ".worktrees",
                            s"$repoName-${dirHash.take(7)}"
                          )
          _            <- ZIO.succeed(os.makeDir.all(os.Path(worktreesRoot.toFile)))
          _            <- ZIO.foreach(branches) { arg =>
                            val isPr         = arg.forall(_.isDigit)
                            val fetchAndDest = if (isPr) {
                              val prId     = arg
                              val prBranch = s"pr-$prId"
                              exec("git", "fetch", "origin", s"pull/$prId/head:$prBranch")
                                .orElse(
                                  exec(
                                    "git",
                                    "fetch",
                                    "origin",
                                    s"merge-requests/$prId/head:$prBranch"
                                  )
                                )
                                .as(prBranch)
                                .tapError { e =>
                                  Console
                                    .printLine(s"Failed to fetch PR $prId: ${e.getMessage}")
                                    .ignore
                                }
                            } else {
                              exec("git", "fetch", "origin", arg)
                                .tapError { e =>
                                  Console
                                    .printLine(s"Failed to fetch branch $arg: ${e.getMessage}")
                                    .ignore
                                }
                                .as(arg)
                            }

                            fetchAndDest.flatMap { dest =>
                              val branchSlug   = slugify(dest)
                              val worktreePath = worktreesRoot.resolve(branchSlug)
                              exec("git", "worktree", "add", worktreePath.toString, dest)
                                .map { _ =>
                                  Console.printLine(worktreePath.toString)
                                }
                                .tapError { e =>
                                  Console
                                    .printLine(
                                      s"Failed to add worktree for $dest: ${e.getMessage}"
                                    )
                                    .ignore
                                }
                            }.ignore // Ignore errors for individual branches to continue processing others
                          }
        } yield ()
      case Subcommand.Path(branch)  =>
        for {
          output <- exec("git", "worktree", "list", "--porcelain")
          lines   = output.split("\n")
          path   <- ZIO.succeed(lines.sliding(2).collectFirst {
                      case Array(pathLine, branchLine)
                          if pathLine.startsWith("worktree ") && branchLine.startsWith(
                            "branch "
                          ) && branchLine.stripPrefix("branch ") == branch =>
                        pathLine.stripPrefix("worktree ")
                    })
          _      <- path match {
                      case Some(p) => Console.printLine(p)
                      case None    =>
                        Console.printLine(s"No worktree found for branch: $branch")
                    }
        } yield ()
      case Subcommand.Prune         =>
        for {
          output   <- exec("git", "worktree", "list", "--porcelain")
          worktrees = output
                        .split("\n")
                        .grouped(2)
                        .collect {
                          case Array(pathLine, branchLine)
                              if pathLine.startsWith("worktree ") && branchLine.startsWith(
                                "branch "
                              ) =>
                            (
                              pathLine.stripPrefix("worktree "),
                              branchLine.stripPrefix("branch ")
                            )
                        }
                        .toList
          _        <- ZIO.foreach(worktrees) { case (wtPath, branch) =>
                        val isStale = for {
                          merged          <- exec("git", "-C", wtPath, "branch", "--merged")
                                               .map(_.contains(branch))
                                               .orElse(ZIO.succeed(false))
                          missingUpstream <- exec(
                                               "git",
                                               "-C",
                                               wtPath,
                                               "rev-parse",
                                               "--symbolic-full-name",
                                               "--remotes",
                                               branch
                                             ).map(_ => false).orElse(ZIO.succeed(true))
                        } yield merged || missingUpstream

                        isStale.flatMap { stale =>
                          if (stale) {
                            for {
                              diffQuiet       <- exec("git", "-C", wtPath, "diff", "--quiet")
                                                   .map(_ => true)
                                                   .orElse(ZIO.succeed(false))
                              diffCachedQuiet <- exec(
                                                   "git",
                                                   "-C",
                                                   wtPath,
                                                   "diff",
                                                   "--cached",
                                                   "--quiet"
                                                 ).map(_ => true).orElse(ZIO.succeed(false))
                              _               <-
                                if (diffQuiet && diffCachedQuiet) {
                                  ZIO
                                    .succeed(os.remove.all(os.Path(wtPath)))
                                    .flatMap(_ => Console.printLine(s"Deleted $wtPath"))
                                } else {
                                  Console.printLine(
                                    s"Skipped (uncommitted changes): $branch at $wtPath"
                                  )
                                }
                            } yield ()
                          } else {
                            ZIO.unit
                          }
                        }
                      }
        } yield ()
    }
}
