import com.typesafe.config.ConfigFactory


object Config {
  case class Settings(provider: Provider)

  private val ConfigFile: os.Path =
    sys.env
      .get("MISE_TASK_DIR")
      .filter(_.nonEmpty)
      .map(os.Path(_) / ".git-commit-message.conf")
      .getOrElse {
        System.err.println(
          "Error: MISE_TASK_DIR not set. Run via `mise run git-commit-message` " +
            "or `mise run git-auto-reword` so the config file location is known."
        )
        sys.exit(1)
      }

  private def defaultContent: String = {
    val providerLines = Provider.all.zipWithIndex
      .map((p, i) => if i == 0 then s"provider = ${p.name}" else s"# provider = ${p.name}")
      .mkString("\n")
    s"""# Configuration for git-commit-message and git-auto-reword.
       |# HOCON format: key = value, # or // for comments.
       |
       |# LLM provider. Uncomment exactly one:
       |$providerLines
       |""".stripMargin
  }

  def load(): Settings = {
    if (!os.exists(ConfigFile))
      os.write(ConfigFile, defaultContent)
    val config = ConfigFactory.parseFile(ConfigFile.toIO)
    val name   =
      if config.hasPath("provider") then config.getString("provider")
      else Provider.all.head.name
    val provider = Provider.fromName(name).getOrElse {
      System.err.println(
        s"Error: unknown provider '$name' in $ConfigFile. " +
          s"Expected one of: ${Provider.all.map(_.name).mkString(", ")}"
      )
      sys.exit(1)
    }
    Settings(provider = provider)
  }
}
