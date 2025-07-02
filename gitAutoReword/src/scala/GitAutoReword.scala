import upickle.default._

object GitAutoReword {
  private case class CommitMessage(subject: String, body: String) derives Reader

  // Always use the built-in prompt to enforce JSON output. Any optional
  // instructions file is added as a SECOND system message, so it cannot
  // override the required output format.
  private def systemPrompt: String =
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

  private def extraInstructionsMessage: Option[ujson.Obj] = {
    val file = os.pwd / ".github" / "git-commit-instructions.md"
    if (os.exists(file))
      Some(
        ujson.Obj(
          "role" -> "system",
          "content" ->
            ("Additional commit message guidelines (do NOT change the required JSON output format):\n\n" +
              os.read(file))
        )
      )
    else None
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

    val msgJson = {
      val messagesSeq: Seq[ujson.Value] =
        Seq(ujson.Obj("role" -> "system", "content" -> systemPrompt)) ++
          extraInstructionsMessage.toSeq ++
          Seq(
            ujson.Obj(
              "role" -> "user",
              "content" -> s"Repository: $repoName\n\nGenerate a commit message for this diff:\n\n$diff"
            )
          )
      val messages = ujson.Arr(messagesSeq*)
      OpenAI.callOpenAIChatCompletion(apiKey, messages)
    }

    val commitMessage = read[CommitMessage](msgJson)

    println("Revising to:")
    println(commitMessage.subject.linesIterator.map("  " + _).mkString("\n"))
    println()
    println(commitMessage.body.linesIterator.map("  " + _).mkString("\n"))

    println()

    // Split by paragraphs and apply to git revise
    val paragraphs = Seq(commitMessage.subject) ++ commitMessage.body
      .split("\n\n+")
      .map(_.trim)
      .filter(_.nonEmpty)

    val _ =
      os.proc(
        "git",
        "revise",
        "--no-index",
        commit,
        paragraphs.flatMap(p => Seq("-m", p))
      ).call(stdout = os.Inherit)
  }
}
