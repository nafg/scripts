import scala.util.Try

import upickle.default.*


case class CommitMessage(subject: String, body: String) derives Reader

object CommitMessageGenerator {
  private val systemPrompt: String =
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

  def repoDescription: String =
    Try(
      "Repository: " +
        os.proc("git", "config", "get", "remote.origin.url")
          .call()
          .out
          .trim()
          .split("/")
          .last
          .stripSuffix(".git")
    ).getOrElse("Directory: " + os.pwd.last)

  def generate(
    repo: String,
    request: String,
    diff: String
  ): CommitMessage = {
    val settings = Config.load()
    val inputs   = PromptInputs(
      system = systemPrompt,
      extraInstructions = extraInstructions,
      repo = repo,
      request = request,
      diff = diff
    )
    read[CommitMessage](settings.provider.generate(inputs, settings.model))
  }

  def render(message: CommitMessage): String =
    s"${message.subject}\n\n${message.body}"

  private def extraInstructions: Option[String] = {
    val file = os.pwd / ".github" / "git-commit-instructions.md"
    Option.when(os.exists(file))(
      "Additional commit message guidelines (do NOT change the required JSON output format):\n\n" +
        os.read(file)
    )
  }
}
