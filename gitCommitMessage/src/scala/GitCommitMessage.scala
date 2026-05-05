import java.nio.file.Path

import com.monovore.decline.{CommandApp, Opts}


object GitCommitMessage
    extends CommandApp(
      name = "git-commit-message",
      header = "Generate a commit message for staged changes",
      main =
        Opts
          .option[Path](
            "dir",
            "The git directory. Defaults to the current directory."
          )
          .withDefault(Path.of("."))
          .map { dir =>
            os.dynamicPwd.withValue(os.FilePath(dir).resolveFrom(os.pwd)) {
              val diff = os.proc("git", "diff", "--cached").call().out.trim()
              if (diff.isEmpty) {
                System.err.println("Error: no staged changes")
                sys.exit(1)
              }

              println(
                CommitMessageGenerator.render(
                  CommitMessageGenerator.generate(
                    repo = CommitMessageGenerator.repoDescription,
                    request = "Generate a commit message for these staged changes:",
                    diff = diff
                  )
                )
              )
            }
          }
    )
