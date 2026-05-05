import requests.RequestAuth
import upickle.default.*


object OpenAI extends Provider {
  val name: String = "openai"

  private case class ChatMessage(role: String, content: String) derives ReadWriter
  private case class ResponseChoice(message: ChatMessage, index: Int) derives Reader
  private case class ChatCompletionResponse(
    id: String,
    choices: Seq[ResponseChoice]) derives Reader

  def generate(inputs: PromptInputs): String = {
    val apiKey = sys.env.getOrElse(
      "OPENAI_API_KEY", {
        System.err.println("Error: OPENAI_API_KEY environment variable not set")
        sys.exit(1)
      }
    )
    val messages =
      Seq(ChatMessage(role = "system", content = inputs.system)) ++
        inputs.extraInstructions.map(i => ChatMessage(role = "system", content = i)) ++
        Seq(
          ChatMessage(
            role = "user",
            content = s"""${inputs.repo}
                         |
                         |${inputs.request}
                         |
                         |${inputs.diff}""".stripMargin
          )
        )

    callChatCompletion(apiKey, messages)
  }

  private def callChatCompletion(
    apiKey: String,
    messages: Seq[ChatMessage],
    model: String = "gpt-4.1-mini",
    temperature: Double = 0.5
  ): String = {
    val requestBody = ujson.Obj(
      "model"           -> model,
      "messages"        -> writeJs(messages),
      "temperature"     -> temperature,
      "response_format" -> ujson.Obj("type" -> "json_object")
    )
    val response    =
      requests.post(
        "https://api.openai.com/v1/chat/completions",
        auth = RequestAuth.Bearer(apiKey),
        headers = Map("Content-Type" -> "application/json"),
        data = requestBody,
        readTimeout = 60_000
      )
    read[ChatCompletionResponse](response.text())
      .choices
      .head
      .message
      .content
      .trim
  }
}
