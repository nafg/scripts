import com.googlecode.lanterna.{SGR, Symbols, TerminalPosition}
import com.googlecode.lanterna.graphics.TextGraphics
import com.googlecode.lanterna.input.KeyType
import com.googlecode.lanterna.terminal.DefaultTerminalFactory
import java.util.concurrent.LinkedTransferQueue

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.boundary
import scala.util.boundary.break

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

class Buffer:
  private val q = new LinkedTransferQueue[String]()
  private var buf = Seq.empty[String]

  def println(s: String): Unit = q.put(s)

  def run(): Unit =
    Iterator.continually(q.take())
      .foreach { line =>
        buf = buf :+ line
        Thread.`yield`()
      }

  def buffer: Seq[String] = buf


@main
def testIndex(command: String*): Unit =
  val srcDir = os.pwd
  val name = srcDir.last + "-" + md5(srcDir.toString).take(4)
  val tmpDir = os.root / "var" / "tmp" / "testIndex"
  val checkoutDir = tmpDir / (name + "-checkout")
  val testingDir = tmpDir / (name + "-testing")
  val checkoutOutput = tmpDir / (checkoutDir.last + ".log")
  val testingOutput = tmpDir / (testingDir.last + ".log")

  os.remove.all(checkoutOutput)
  os.remove.all(testingOutput)

  val checkoutOutputQueue, testingOutputQueue = new Buffer

  Future:
    checkoutOutputQueue.run()

  Future:
    testingOutputQueue.run()


  //  println(srcDir)
  //  println(checkoutOutput)
  //  println(testingOutput)

  val checkoutProcessOutput = os.ProcessOutput.Readlines { line =>
    os.write.append(checkoutOutput, line + "\n")
    checkoutOutputQueue.println(line)
  }
  val testingProcessOutput = os.ProcessOutput.Readlines { line =>
    val str = line.replace(testingDir.toString, srcDir.toString)
    os.write.append(testingOutput, str + "\n")
    testingOutputQueue.println(str)
  }


  def doSync(): Unit =
    while (true)
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
        "--exclude", "out",
        "--exclude", "*.jvm",
        "--exclude", "*.js",
        "--exclude", ".bleep",
        checkoutDir.toString + "/",
        testingDir.toString + "/"
      ).call(stdout = checkoutProcessOutput, mergeErrIntoOut = true, check = false)
    end while

  var testProcess: Option[os.SubProcess] = None

  def test(): Unit =
    os.makeDir.all(testingDir)
    while (true)
      Thread.sleep(2000)
      val p =
        os.proc(command)
          .spawn(
            cwd = testingDir,
            stdout = testingProcessOutput,
            stderr = testingProcessOutput,
            stdin = os.Pipe,
            env = Map("JAVA_OPTS" -> "-Xmx3g")
          )
      testProcess = Some(p)
      p.join()
      testingOutputQueue.println("QUIT")

  Future:
    doSync()
  Future:
    test()

  val terminalFactory = new DefaultTerminalFactory()
  terminalFactory.setForceTextTerminal(true)

  //  Future:
  //    for (i <- 1 to 1000000)
  //      Thread.sleep(100)
  //      val str =
  //        f"$i%10s  " + AnsiColor.RED + Iterator.fill(50)(Random.nextPrintableChar()).mkString + AnsiColor.RESET
  //      checkoutOutputQueue.println(str)


  def renderPane(tg: TextGraphics, queue: Buffer, scrollUp: Int = 0): Unit =
    val buf = queue.buffer
    tg.fill(' ')
    for ((line, n) <- buf.dropRight(scrollUp).takeRight(tg.getSize.getRows).zipWithIndex)
      tg.putCSIStyledString(0, n, line)

  def useLanterna(): Unit = {
    val screen = terminalFactory.createScreen()
    try
      screen.startScreen()
      val tg = screen.newTextGraphics()
      //    screen.getTerminal.addResizeListener { (_, newSize) =>
      //      tg.putString(0, 0, s"Resized to $newSize", SGR.BOLD)
      //      screen.refresh()
      //    }
      var scrollUp = 0
      boundary:
        Iterator.continually(Option(screen.pollInput()))
          .foreach { input =>
            input.foreach { key =>
              val character = Option(key.getCharacter).map(_.charValue())
              if key.getKeyType == KeyType.EOF || character.contains('q') then
                testProcess.foreach(_.destroy())
                testProcess = None
                break()
              else if key.getKeyType == KeyType.ArrowUp then
                scrollUp = scrollUp + 1 min testingOutputQueue.buffer.length - screen.getTerminalSize.getRows
              else if key.getKeyType == KeyType.ArrowDown then
                scrollUp = scrollUp - 1 max 0
              else if character.contains('r') then
                testProcess.foreach(_.destroy())
                testProcess = None
            }

            screen.doResizeIfNecessary()
            val size = screen.getTerminalSize
            val rows = size.getRows - 1
            val i = 20
            val tg1 =
              tg.newTextGraphics(TerminalPosition.TOP_LEFT_CORNER, tg.getSize.withRows(i - 1))
            tg.drawLine(0, i, size.getColumns, i + 2, Symbols.SINGLE_LINE_HORIZONTAL)
            val tg2 =
              tg.newTextGraphics(TerminalPosition.TOP_LEFT_CORNER.withRelativeRow(i), tg.getSize.withRows(rows - i))
            renderPane(tg1, checkoutOutputQueue, 0)
            renderPane(tg2, testingOutputQueue, scrollUp)
            tg.putString(size.getColumns - 10, 0, s"($scrollUp)", SGR.BOLD)
            screen.refresh()
            Thread.`yield`()
          }
    finally
      screen.stopScreen()
  }

  useLanterna()
