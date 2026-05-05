object Claude extends Provider {
  val name: String = "claude"

  def generate(inputs: PromptInputs): String = {
    val prompt =
      (Seq(s"system:\n${inputs.system}") ++
        inputs.extraInstructions.map(i => s"system:\n$i") ++
        Seq(
          s"""user:
             |${inputs.repo}
             |
             |${inputs.request}
             |
             |${inputs.diff}""".stripMargin
        )).mkString("\n\n")

    val output =
      os.proc("env", "-u", "ANTHROPIC_API_KEY", "claude", "-p")
        .call(stdin = prompt)
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
