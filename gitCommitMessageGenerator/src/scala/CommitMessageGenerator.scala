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
    provider: String,
    repo: String,
    request: String,
    diff: String
  ): CommitMessage = {
    val prompt = commitMessagePrompt(repo, request, diff)
    val msgJson =
      provider.toLowerCase match
        case "codex" =>
          Codex.generateCommitMessage(prompt.asCodexPrompt)
        case "openai" =>
          val apiKey = sys.env.getOrElse(
            "OPENAI_API_KEY", {
              System.err.println("Error: OPENAI_API_KEY environment variable not set")
              sys.exit(1)
            }
          )
          OpenAI.callOpenAIChatCompletion(apiKey)(prompt.asOpenAIMessages*)
        case other =>
          System.err.println(
            s"Error: unknown provider '$other'. Expected codex or openai."
          )
          sys.exit(1)

    read[CommitMessage](msgJson)
  }

  def render(message: CommitMessage): String =
    s"${message.subject}\n\n${message.body}"

  private def commitMessagePrompt(
    repo: String,
    request: String,
    diff: String
  ): CommitMessagePrompt =
    CommitMessagePrompt(
      system = systemPrompt,
      extraInstructions = extraInstructions,
      repo = repo,
      request = request,
      diff = diff
    )

  private def extraInstructions: Option[String] = {
    val file = os.pwd / ".github" / "git-commit-instructions.md"
    Option.when(os.exists(file))(
      "Additional commit message guidelines (do NOT change the required JSON output format):\n\n" +
        os.read(file)
    )
  }
}

private case class CommitMessagePrompt(
  system: String,
  extraInstructions: Option[String],
  repo: String,
  request: String,
  diff: String
) {
  def asCodexPrompt: String =
    (Seq(s"system:\n$system") ++
      extraInstructions.map(instructions => s"system:\n$instructions") ++
      Seq(
        s"""user:
           |$repo
           |
           |$request
           |
           |$diff""".stripMargin
      )).mkString("\n\n")

  def asOpenAIMessages: Seq[OpenAI.ChatMessage] =
    Seq(OpenAI.ChatMessage(role = "system", content = system)) ++
      extraInstructions.map(instructions =>
        OpenAI.ChatMessage(role = "system", content = instructions)
      ) ++
      Seq(
        OpenAI.ChatMessage(
          role = "user",
          content = s"""$repo
               |
               |$request
               |
               |$diff""".stripMargin
        )
      )
}
