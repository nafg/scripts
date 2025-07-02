import scala.sys.process._
import scala.util.{Try, Failure, Success}

object GitAutoReword {
  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      System.err.println("Usage: git-auto-reword <commit-sha>")
      sys.exit(1)
    }
    val commit = args(0)
    println("Current commit message:")
    Try(os.proc("git", "show", commit).call().out.trim()) match {
      case Success(out) => println(out)
      case Failure(ex)  =>
        System.err.println(s"ERROR: git show failed: ${ex.getMessage}")
        sys.exit(1)
    }

    val diff = Try(
      os.proc("git", "show", commit, "--format=").call().out.trim()
    ) match {
      case Success(d)  => d
      case Failure(ex) =>
        System.err.println(s"ERROR: git show diff failed: ${ex.getMessage}")
        sys.exit(1)
    }

    val msg = Try {
      os.proc(
        "npx",
        "--yes",
        "@google/gemini-cli",
        "-p",
        "describe this commit. See instructions in .github/git-commit-instructions.md",
        "-y"
      ).call(stdin = diff)
        .out
        .trim()
    } match {
      case Success(out) => out
      case Failure(ex)  =>
        System.err.println(s"ERROR: gemini-cli failed: ${ex.getMessage}")
        sys.exit(1)
    }

    println("Revising to:")
    println(msg)

    val paragraphs = msg.split("\n\n+").map(_.trim).filter(_.nonEmpty)
    val cmd = Seq("git", "revise", "--no-index", commit) ++
      paragraphs.flatMap(p => Seq("-m", p))
    sys.exit(Process(cmd).!)
  }
}
