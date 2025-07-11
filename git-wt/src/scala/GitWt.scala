import java.nio.file.Paths
import java.security.MessageDigest

import zio.*
import zio.cli.*


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

  private def sha1Hash(s: String): String = {
    val md = MessageDigest.getInstance("SHA-1")
    md.digest(s.getBytes("UTF-8")).map("%02x".format(_)).mkString
  }

  def impl: ((Git, Subcommand)) => Task[Unit] = {
    case git -> Subcommand.New(branch)   =>
      for {
        gitRoot      <- git.repository.root
        repoName      = gitRoot.getFileName.toString
        dirHash       = sha1Hash(gitRoot.toString)
        worktreesRoot = Paths.get(sys.props("user.home"), ".worktrees", s"$repoName-${dirHash.take(7)}")
        branchSlug    = slugify(branch)
        worktreePath  = worktreesRoot.resolve(branchSlug)
        _            <- ZIO.attempt(os.makeDir.all(os.Path(worktreePath)))
        _            <- git.branches.create(branch)
        worktree     <- git.worktrees.add(worktreePath.toString, branch)
        _            <- Console.printLine(worktree.path.toString)
      } yield ()
    case git -> Subcommand.Add(branches) =>
      for {
        gitRoot      <- git.repository.root
        repoName      = gitRoot.getFileName.toString
        dirHash       = sha1Hash(gitRoot.toString)
        worktreesRoot = Paths.get(sys.props("user.home"), ".worktrees", s"$repoName-${dirHash.take(7)}")
        _            <- ZIO.succeed(os.makeDir.all(os.Path(worktreesRoot.toFile)))
        _            <- ZIO.foreachDiscard(branches) { arg =>
                          val isPr         = arg.forall(_.isDigit)
                          val fetchAndDest =
                            if (isPr) {
                              val prId     = arg
                              val prBranch = s"pr-$prId"
                              git.remotes.fetchPullRequest(prId, prBranch)
                                .tapError { e =>
                                  Console
                                    .printLine(s"Failed to fetch PR $prId: ${e.getMessage}")
                                    .ignore
                                }
                            } else
                              git.remotes.fetch(arg)
                                .tapError { e =>
                                  Console
                                    .printLine(s"Failed to fetch branch $arg: ${e.getMessage}")
                                    .ignore
                                }
                                .as(Git.Branch(arg))

                          fetchAndDest.flatMap { branch =>
                            val branchSlug   = slugify(branch.name)
                            val worktreePath = worktreesRoot.resolve(branchSlug)

                            (git.worktrees.add(worktreePath.toString, branch.name) *>
                              Console.printLine(worktreePath.toString))
                              .tapError { e =>
                                Console.printLine(s"Failed to add worktree for ${branch.name}: ${e.getMessage}")
                                  .ignore
                              }
                          }.ignore
                        }
      } yield ()
    case git -> Subcommand.Path(branch)  =>
      for {
        worktrees <- git.worktrees.list
        worktree   = worktrees.find(_.branch.name == branch)
        _         <- worktree match {
                       case Some(w) => Console.printLine(w.path.toString)
                       case None    => Console.printLine(s"No worktree found for branch: $branch")
                     }
      } yield ()
    case git -> Subcommand.Prune         =>
      for {
        worktrees <- git.worktrees.list
        _         <- ZIO.foreachDiscard(worktrees) { worktree =>
                       for {
                         merged          <- git.branches.listMerged(
                                              worktree.path.toString
                                            ).map(_.exists(_.name == worktree.branch.name)).orElseSucceed(false)
                         missingUpstream <- Git(worktree.path)
                                              .remotes
                                              .getSymbolicFullName(worktree.branch.name)
                                              .isFailure
                         _               <- ZIO.when(merged || missingUpstream) {
                                              Git(worktree.path).status.isClean
                                                .flatMap { isClean =>
                                                  if (isClean)
                                                    ZIO.attempt(os.remove.all(os.Path(worktree.path))) *>
                                                      Console.printLine(s"Deleted ${worktree.path}")
                                                  else
                                                    Console.printLine(
                                                      s"Skipped (uncommitted changes):" +
                                                        s" ${worktree.branch.name} at ${worktree.path}"
                                                    )
                                                }
                                            }

                       } yield ()
                     }
      } yield ()
  }

  override val cliApp =
    CliApp.make(
      name = "git-wt",
      version = "0.1.0",
      summary = HelpDoc.Span.text("A lightweight worktree manager"),
      command =
        Command("git-wt", Options.directory("cwd", Exists.Yes).withDefault(Paths.get(".")) ?? "Working directory")
          .withHelp("git-wt")
          .map(new Git(_))
          .subcommands(
            Command("new", Args.text("branch") ?? "New branch").map(Subcommand.New.apply)
              .withHelp("git-wt new BRANCH") |
              Command("add", Args.text("branch-or-pr").* ?? "Existing branch, PR, or MR").map(Subcommand.Add.apply)
                .withHelp("git-wt add BRANCH...") |
              Command("path", Args.text("branch") ?? "Existing branch").map(Subcommand.Path.apply) |
              Command("prune").as(Subcommand.Prune)
          )
    )(impl(_).catchAll { e =>
      Console.printLine(e.getMessage) *> exit(ExitCode.failure)
    })
}
