import java.nio.file.Path

import cats.implicits.catsSyntaxTuple3Semigroupal
import com.monovore.decline.{CommandApp, Opts}
import upickle.default.*


private case class CommitMessage(subject: String, body: String) derives Reader

// Always use the built-in prompt to enforce JSON output. Any optional
// instructions file is added as a SECOND system message, so it cannot
// override the required output format.
private def systemPrompt: String =
  """
    |You are an expert at writing Git commit messages.
    |Analyze the diff and create a clear, concise commit message that explains:
    |1. WHAT was changed (in the subject line)
    |2. WHY it was changed (in the body)
    |
    |Output a JSON object with the following structure:
    |{
    |  "subject": "<type>[(scope)]: <short summary>",
    |  "body": "<detailed explanation>"
    |}
    |
    |The subject line should follow conventional commits format (under 72 chars).
    |The body should be wrapped at 72 characters if necessary.
    |
    |ONLY output the JSON object.
    """.stripMargin

private def extraInstructionsMessage: OpenAI.ChatMessage = {
  val file = os.pwd / ".github" / "git-commit-instructions.md"
  if (os.exists(file))
    OpenAI.ChatMessage(
      role = "system",
      content =
        "Additional commit message guidelines (do NOT change the required JSON output format):\n\n" +
          os.read(file)
    )
  else
    OpenAI.ChatMessage(role = "system", content = "")
}

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
          Opts
            .option[String](
              "provider",
              "Message provider: codex or openai. Defaults to codex."
            )
            .withDefault(sys.env.getOrElse("GIT_AUTO_REWORD_PROVIDER", "codex")),
          Opts.argument[String]("commit").withDefault("HEAD")
        ).mapN { (dir, provider, commit) =>
          os.dynamicPwd.withValue(os.FilePath(dir).resolveFrom(os.pwd)) {
            val normalizedProvider = provider.toLowerCase

            println("Current commit message:")
            println(exec("git", "show", "--color=always", commit, "--"))

            val diff = exec("git", "show", commit, "--format=")

            // Get repo info for context
            val repoNameStr =
              try
                "Repository: " +
                  os.proc("git", "config", "get", "remote.origin.url")
                    .call()
                    .out
                    .trim()
                    .split("/")
                    .last
                    .stripSuffix(".git")
              catch
                case os.SubprocessException(_) =>
                  "Directory: " +
                    os.pwd.last

            val messages = Seq(
              OpenAI.ChatMessage(role = "system", content = systemPrompt),
              extraInstructionsMessage,
              OpenAI.ChatMessage(
                role = "user",
                content = s"""$repoNameStr
                     |
                     |Generate a commit message for this diff:
                     |
                     |$diff""".stripMargin
              )
            )

            val msgJson =
              normalizedProvider match
                case "openai" =>
                  val apiKey = sys.env.getOrElse(
                    "OPENAI_API_KEY", {
                      System.err.println(
                        "Error: OPENAI_API_KEY environment variable not set"
                      )
                      sys.exit(1)
                    }
                  )
                  OpenAI.callOpenAIChatCompletion(apiKey)(messages*)
                case "codex" =>
                  Codex.generateCommitMessage(messages*)
                case other =>
                  System.err.println(
                    s"Error: unknown provider '$other'. Expected openai or codex."
                  )
                  sys.exit(1)

            val commitMessage = read[CommitMessage](msgJson)

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
