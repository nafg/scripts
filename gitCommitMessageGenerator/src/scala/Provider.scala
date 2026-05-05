case class PromptInputs(
  system: String,
  extraInstructions: Option[String],
  repo: String,
  request: String,
  diff: String
)


trait Provider {
  def name: String
  def defaultModel: String
  def exampleModels: Seq[String] = Seq(defaultModel)
  def generate(inputs: PromptInputs, model: String): String
}

object Provider {
  val all: Seq[Provider]                    = Seq(Codex, OpenAI, Claude)
  def fromName(s: String): Option[Provider] = all.find(_.name.equalsIgnoreCase(s))
}
