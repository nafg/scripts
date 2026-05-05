import java.nio.file.Path

import cats.implicits.catsSyntaxTuple2Semigroupal
import com.monovore.decline.{CommandApp, Opts}


private def exec(parts: os.Shellable*): String =
  try os.proc(parts*).call().out.trim()
  catch
    case e: Throwable =>
      System.err.println(
        s"Error running ${parts.flatMap(_.value).mkString(" ")}: " + e.getMessage
      )
      sys.exit(1)

object GitAutoReword
    extends CommandApp(
      name = "git-auto-reword",
      header = "Auto reword git commit messages",
      main =
        (
          Opts
            .option[Path](
              "dir",
              "The git directory. Defaults to the current directory."
            )
            .withDefault(Path.of(".")),
          Opts.argument[String]("commit").withDefault("HEAD")
        ).mapN { (dir, commit) =>
          os.dynamicPwd.withValue(os.FilePath(dir).resolveFrom(os.pwd)) {
            println("Current commit message:")
            println(exec("git", "show", "--color=always", commit, "--"))

            val diff          = exec("git", "show", commit, "--format=")
            val commitMessage = CommitMessageGenerator.generate(
              repo = CommitMessageGenerator.repoDescription,
              request = "Generate a commit message for this diff:",
              diff = diff
            )

            println()

            println("Revising to:")
            println(
              commitMessage.subject.linesIterator.map("  " + _).mkString("\n")
            )
            println()
            println(
              commitMessage.body.linesIterator.map("  " + _).mkString("\n")
            )

            println()

            // Split by paragraphs and apply to git revise
            val paragraphs = Seq(commitMessage.subject) ++ commitMessage.body
              .split("\n\n+")
              .map(_.trim)
              .filter(_.nonEmpty)

            val _ =
              os.proc(
                "git",
                "revise",
                "--no-index",
                commit,
                paragraphs.flatMap(p => Seq("-m", p))
              ).call(stdout = os.Inherit)
          }
        }
    )
