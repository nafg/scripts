import java.nio.file.Path

import Git.GitError
import zio.*
import zio.process.{Command, CommandError}


object Git {

  /** Error types for Git operations */
  sealed trait GitError extends Throwable
  object GitError {
    final case class CommandFailed(command: String, args: Seq[String], cause: CommandError, err: Option[String])
        extends GitError {
      override def getMessage: String  = s"Git command failed: $command ${args.mkString(" ")}"
      override def getCause: Throwable = cause
    }

    final case class BranchNotFound(branch: String)    extends GitError
    final case class WorktreeNotFound(path: String)    extends GitError
    final case class RemoteNotFound(remote: String)    extends GitError
    final case class InvalidRefspec(refspec: String)   extends GitError
    final case class PullRequestNotFound(prId: String) extends GitError
  }

  final case class Branch(name: String)
  final case class Remote(name: String)
  final case class Worktree(path: Path, branch: Branch)
  final case class PullRequest(id: String, branch: Branch)

}

/** Git API for interacting with git repositories */
final case class Git(cwd: Path) {

  /** Executes a git command and returns the result as a string */
  private def exec(command: String, args: String*): ZIO[Any, GitError, String] = {
    println(s"$command ${args.mkString(" ")}")

    Command(command, args*)
      .workingDirectory(cwd.toFile)
      .run
      .mapError(_ -> None)
      .flatMap { p =>
        (p.successfulExitCode *> p.stdout.string)
          .flatMapError(e => p.stderr.string.fold(_ => e -> None, e -> Some(_)))
      }
      .mapError {
        case (ce, se) => GitError.CommandFailed(command, args, ce, se)
      }
  }

  /** Repository operations */
  object repository {

    /** Gets the root directory of the git repository */
    def root: ZIO[Any, GitError, Path] =
      exec("git", "rev-parse", "--show-toplevel")
        .map(path => Path.of(path.trim))
  }

  /** Branch operations */
  object branches {

    /** Creates a new branch from the specified start point */
    def create(branchName: String, startPoint: String = "HEAD"): ZIO[Any, GitError, Git.Branch] =
      exec("git", "branch", branchName, startPoint)
        .as(Git.Branch(branchName))

    /** Lists branches that have been merged into the specified branch */
    def listMerged(branch: String): ZIO[Any, GitError, List[Git.Branch]] =
      exec("git", "branch", "--merged", branch)
        .map(_.linesIterator.map(_.trim).filter(_.nonEmpty).map(Git.Branch.apply).toList)

    /** Checks if a branch exists */
    def exists(branchName: String): ZIO[Any, GitError, Boolean] =
      exec("git", "rev-parse", "--verify", branchName)
        .as(true)
        .catchSome {
          case GitError.CommandFailed(_, _, _, _) => ZIO.succeed(false)
        }
  }

  /** Worktree operations */
  object worktrees {

    /** Adds a new worktree at the specified path for the given branch */
    def add(path: String, branch: String): ZIO[Any, GitError, Git.Worktree] =
      exec("git", "worktree", "add", path, branch)
        .as(Git.Worktree(Path.of(path), Git.Branch(branch)))

    /** Lists all worktrees */
    def list: ZIO[Any, GitError, List[Git.Worktree]] =
      exec("git", "worktree", "list", "--porcelain")
        .map { output =>
          output
            .split("\n")
            .grouped(2)
            .collect {
              case Array(pathLine, branchLine)
                  if pathLine.startsWith("worktree ") && branchLine.startsWith("branch ") =>
                Git.Worktree(
                  Path.of(pathLine.stripPrefix("worktree ").trim),
                  Git.Branch(branchLine.stripPrefix("branch ").trim)
                )
            }
            .toList
        }

    /** Finds a worktree for a specific branch */
    def findByBranch(branch: String): ZIO[Any, GitError, Option[Git.Worktree]] =
      list.map(_.find(_.branch.name == branch))
  }

  /** Remote operations */
  object remotes {

    /** Fetches from the origin remote using the specified refspec */
    def fetch(refspec: String, remote: String = "origin"): ZIO[Any, GitError, Unit] =
      exec("git", "fetch", remote, refspec).unit

    /** Fetches a pull request and creates a local branch for it */
    def fetchPullRequest(prId: String, branchName: String = ""): ZIO[Any, GitError, Git.Branch] = {
      val targetBranch = if (branchName.isEmpty) s"pr-$prId" else branchName

      exec("git", "fetch", "origin", s"pull/$prId/head:$targetBranch")
        .catchSome {
          case GitError.CommandFailed(_, _, _, _) =>
            exec("git", "fetch", "origin", s"merge-requests/$prId/head:$targetBranch")
        }
        .as(Git.Branch(targetBranch))
        .catchAll(_ => ZIO.fail(GitError.PullRequestNotFound(prId)))
    }

    /** Gets the full symbolic name of a branch in the remotes */
    def getSymbolicFullName(branch: String): ZIO[Any, GitError, String] =
      exec("git", "rev-parse", "--symbolic-full-name", "--remotes", branch)
        .map(_.trim)
  }

  /** Working directory status operations */
  object status {

    /** Checks if there are any uncommitted changes in the working directory */
    def hasUncommittedChanges: ZIO[Any, Nothing, Boolean] =
      exec("git", "diff", "--quiet").as(false)
        .catchAll(_ => ZIO.succeed(true))

    /** Checks if there are any staged but uncommitted changes */
    def hasStagedUncommittedChanges: ZIO[Any, Nothing, Boolean] =
      exec("git", "diff", "--cached", "--quiet").as(false)
        .catchAll(_ => ZIO.succeed(true))

    /** Checks if the working directory is clean (no uncommitted or staged changes) */
    def isClean: ZIO[Any, Nothing, Boolean] =
      (hasUncommittedChanges <&> hasStagedUncommittedChanges).map { case (uncommitted, staged) =>
        !uncommitted && !staged
      }
  }
}
