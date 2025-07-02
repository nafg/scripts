import requests.RequestAuth
import upickle.default.*

object OpenAI {
  private case class ChatMessage(role: String, content: String) derives Reader
  private case class ResponseChoice(message: ChatMessage, index: Int)
      derives Reader
  private case class ChatCompletionResponse(
      id: String,
      choices: Seq[ResponseChoice]
  ) derives Reader

  // Request commit message from OpenAI API
  def callOpenAIChatCompletion(
      apiKey: String,
      messages: ujson.Arr,
      model: String = "gpt-4-turbo",
      temperature: Double = 0.5
  ): String = {
    val requestBody = ujson.Obj(
      "model" -> model,
      "messages" -> messages,
      "temperature" -> temperature,
      "response_format" -> ujson.Obj("type" -> "json_object")
    )
    val response =
      requests.post(
        "https://api.openai.com/v1/chat/completions",
        auth = RequestAuth.Bearer(apiKey),
        headers = Map("Content-Type" -> "application/json"),
        data = requestBody,
        readTimeout = 60_000
      )
    read[ChatCompletionResponse](
      response.text()
    ).choices.head.message.content.trim
  }
}
