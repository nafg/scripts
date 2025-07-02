import scala.sys.process._

import requests.RequestAuth
import upickle.default._

object GitAutoReword {
  // Case classes for JSON parsing
  case class ChatMessage(role: String, content: String)
  case class ResponseChoice(message: ChatMessage, index: Int)
  case class ChatCompletionResponse(id: String, choices: Seq[ResponseChoice])

  // JSON readers for parsing OpenAI response
  implicit val chatMessageRW: ReadWriter[ChatMessage] = macroRW
  implicit val responseChoiceRW: ReadWriter[ResponseChoice] = macroRW
  implicit val chatCompletionResponseRW: ReadWriter[ChatCompletionResponse] =
    macroRW

  private val defaultSystemPrompt = """
    |You are an expert at writing Git commit messages following conventional commits format.
    |Analyze the diff and create a clear, concise commit message that explains:
    |1. WHAT was changed (in the summary line)
    |2. WHY it was changed (in the body)
    |3. Any significant implementation details if relevant
    |
    |Format:
    |- First line: <type>[(scope)]: <short summary> (under 72 chars)
    |  * Types: feat, fix, docs, style, refactor, test, chore
    |  * Scope is optional, e.g. (api), (cli), (config)
    |  * Use lowercase and present tense: "add" not "added" or "adds"
    |- Blank line
    |- Body text (wrapped at 72 chars)
    |- Blank line if needed
    |- Footer (e.g., breaking changes, references to issues)
    |
    |ONLY output the commit message, nothing else.
    |Do NOT include explanations of your thought process or inability to find files.
    |Start directly with the commit message content.
    """.stripMargin

  private def getSystemPrompt: String = {
    val file = os.pwd / ".github" / "git-commit-instructions.md"
    if (os.exists(file)) os.read(file)
    else defaultSystemPrompt
  }

  private def exec(parts: os.Shellable*): String =
    try os.proc(parts*).call().out.trim()
    catch
      case e: Throwable =>
        System.err.println(
          s"Error running ${parts.flatMap(_.value).mkString(" ")}: " + e.getMessage
        )
        sys.exit(1)

  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      System.err.println("Usage: git-auto-reword <commit-sha>")
      sys.exit(1)
    }

    val commit = args(0)

    // Get API key from the environment
    val apiKey = sys.env.getOrElse(
      "OPENAI_API_KEY", {
        System.err.println(
          "Error: OPENAI_API_KEY environment variable not set"
        )
        sys.exit(1)
      }
    )

    println("Current commit message:")
    println(exec("git", "show", "--color=always", commit))

    val diff = exec("git", "show", commit, "--format=")

    // Get repo info for context
    val repoName =
      exec("git", "config", "--get", "remote.origin.url")
        .split("/")
        .last
        .stripSuffix(".git")

    // Request commit message from OpenAI API
    def callOpenAIChatCompletion(
        apiKey: String,
        messages: ujson.Arr,
        model: String = "gpt-4",
        temperature: Double = 0.5,
        responseFormat: String = "text"
    ): String = {
      val requestBody = ujson.Obj(
        "model" -> model,
        "messages" -> messages,
        "temperature" -> temperature,
        "response_format" -> ujson.Obj("type" -> responseFormat)
      )
      val response =
        requests.post(
          "https://api.openai.com/v1/chat/completions",
          auth = RequestAuth.Bearer(apiKey),
          headers = Map("Content-Type" -> "application/json"),
          data = requestBody,
          readTimeout = 30_000
        )
      read[ChatCompletionResponse](response.text()).choices.head.message.content.trim
    }

    val msg = {
      val messages = ujson.Arr(
        ujson.Obj("role" -> "system", "content" -> getSystemPrompt),
        ujson.Obj(
          "role" -> "user",
          "content" -> s"Repository: $repoName\n\nGenerate a commit message for this diff:\n\n$diff"
        )
      )
      callOpenAIChatCompletion(apiKey, messages)
    }

    println("Revising to:")
    println(msg)

    // Split by paragraphs and apply to git revise
    val paragraphs = msg.split("\n\n+").map(_.trim).filter(_.nonEmpty)
    val cmd = Seq("git", "revise", "--no-index", commit) ++
      paragraphs.flatMap(p => Seq("-m", p))
    sys.exit(Process(cmd).!)
  }
}
