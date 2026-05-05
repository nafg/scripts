case class PromptInputs(
  system: String,
  extraInstructions: Option[String],
  repo: String,
  request: String,
  diff: String
)


trait Provider {
  def name: String
  def generate(inputs: PromptInputs): String
}

object Provider {
  val all: Seq[Provider]                    = Seq(Codex, OpenAI, Claude)
  def fromName(s: String): Option[Provider] = all.find(_.name.equalsIgnoreCase(s))
}
