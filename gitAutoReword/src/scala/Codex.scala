import upickle.default.*


object Codex {
  private val responseSchema = ujson.Obj(
    "type"                 -> "object",
    "additionalProperties" -> false,
    "required"             -> ujson.Arr("subject", "body"),
    "properties" -> ujson.Obj(
      "subject" -> ujson.Obj("type" -> "string"),
      "body"    -> ujson.Obj("type" -> "string")
    )
  )

  def generateCommitMessage(messages: OpenAI.ChatMessage*): String = {
    val prompt =
      messages
        .filter(_.content.trim.nonEmpty)
        .map(message => s"${message.role}:\n${message.content}")
        .mkString("\n\n")

    val schemaFile = os.temp(prefix = "git-auto-reword-schema-")
    val outputFile = os.temp(prefix = "git-auto-reword-output-")

    try {
      os.write.over(schemaFile, write(responseSchema))

      val _ = os.proc(
        "codex",
        "exec",
        "--ephemeral",
        "--sandbox",
        "read-only",
        "--cd",
        os.pwd.toString,
        "--output-schema",
        schemaFile.toString,
        "--output-last-message",
        outputFile.toString,
        "-"
      ).call(stdin = prompt)

      os.read(outputFile).trim
    } finally {
      os.remove(schemaFile): Unit
      os.remove(outputFile): Unit
    }
  }
}
