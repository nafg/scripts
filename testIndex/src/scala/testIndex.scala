import java.io.PrintStream
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

//@main def testIndex =
//  val sessionName = "mySession"
//  try
//    val program1 = "program1"
//    val program2 = "program2"
//
//    os.proc("tmux", "new-session", "-d", "-s", sessionName).call()
//    os.proc("tmux", "split-window", "-v", "-t", sessionName).call()
//    os.proc("tmux", "send-keys", "-t", s"$sessionName:0.0", program1, "C-m").call()
//    os.proc("tmux", "send-keys", "-t", s"$sessionName:0.1", program2, "C-m").call()
//    os.proc("tmux", "attach", "-t", sessionName).call(stdout = os.Inherit, stdin = os.Inherit)
//  finally
//    os.proc("tmux", "kill-session", "-t", sessionName).call()


def md5(s: String): String =
  val md = java.security.MessageDigest.getInstance("MD5")
  md.digest(s.getBytes).map("%02x".format(_)).mkString

@main
def testIndex(): Unit =
  val srcDir = os.pwd
  val name = srcDir.last + "-" + md5(srcDir.toString).take(4)
  val tmpDir = os.root / "var" / "tmp"
  val checkoutDir = tmpDir / (name + "-checkout")
  val testingDir = tmpDir / (name + "-testing")
  val task = "test"

  val checkoutOutput = tmpDir / (checkoutDir.last + ".log")
  val testingOutput = tmpDir / (testingDir.last + ".log")

  println(srcDir)
//  println(checkoutOutput)
//  println(testingOutput)

  val checkoutProcessOutput = os.ProcessOutput.Readlines(line => os.write.append(checkoutOutput, line + "\n"))
  val testingProcessOutput = os.ProcessOutput.Readlines(line => os.write.append(testingOutput, line + "\n"))

  val checkoutOutputStream = os.write.append.outputStream(checkoutOutput)
  val checkoutPrintStream = PrintStream(checkoutOutputStream, true, "UTF-8")

  def doSync(): Unit =
    Console.withOut(checkoutOutputStream):
      Console.withErr(checkoutOutputStream):
        while (true)
          try
            print("Waiting...\r")
            Thread.sleep(2000)
            print("          \n")
            os.remove.all(checkoutDir)
            os.proc("git", "checkout-index", "-a", "-f", s"--prefix=$checkoutDir/")
              .call(stdout = checkoutProcessOutput, mergeErrIntoOut = true, check = false)
            os.proc(
              "rsync",
              "-rci",
              "--delete",
              "--exclude", "target*",
              "--exclude", "project/project",
              "--exclude", "*.jvm",
              "--exclude", "*.js",
              checkoutDir.toString + "/",
              testingDir.toString + "/"
            ).call(stdout = checkoutProcessOutput, mergeErrIntoOut = true, check = false)
          catch case e: Throwable =>
            e.printStackTrace(checkoutPrintStream)
            Thread.sleep(2000)
        end while

  def test(): Unit =
    os.makeDir.all(testingDir)
    Thread.sleep(2000)
    os.proc("sbt", "--color=always", "set Global / onChangedBuildSource := ReloadOnSourceChanges", task)
      .call(cwd = testingDir, stdout = testingProcessOutput, stderr = testingProcessOutput)

  Future:
    doSync()
  Future:
    test()

  os.proc("tmux", "new-session", s"tail -F $checkoutOutput", ";", "split", "-p", "80", s"tail -F $testingOutput")
    .call(stdin = os.Inherit, stdout = os.Inherit)

//  sys.env.foreach(println)
