object Claude extends Provider {
  val name: String                        = "claude"
  val defaultModel: String                = "sonnet"
  override val exampleModels: Seq[String] = Seq("sonnet", "opus", "haiku")

  def generate(inputs: PromptInputs, model: String): String = {
    val systemPrompt = inputs.extraInstructions match
      case Some(extra) => s"${inputs.system}\n\n$extra"
      case None        => inputs.system

    val userContent =
      s"""${inputs.repo}
         |
         |${inputs.request}
         |
         |${inputs.diff}""".stripMargin

    val output =
      os.proc(
        "env",
        "-u",
        "ANTHROPIC_API_KEY",
        "claude",
        "-p",
        "--model",
        model,
        "--system-prompt",
        systemPrompt,
        "--strict-mcp-config",
        "--setting-sources",
        "",
        "--disable-slash-commands"
      ).call(stdin = userContent)
        .out
        .trim()
    stripFences(output)
  }

  private def stripFences(s: String): String = {
    val trimmed = s.trim
    if (trimmed.startsWith("```")) {
      val lines        = trimmed.linesIterator.toList
      val withoutFirst = lines.tail
      val withoutLast  =
        if withoutFirst.lastOption.exists(_.trim.startsWith("```"))
        then withoutFirst.init
        else withoutFirst
      withoutLast.mkString("\n")
    } else trimmed
  }
}
