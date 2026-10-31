import java.io.File
import scala.sys.process.*
import scala.util.control.NonFatal

case class ShellResult(
                        exitCode: Int,
                        stdout: String,
                        stderr: String,
                        startTime: String,
                        endTime: String,
                        durationMillis: Long
                      )

// Интеграция с внешним миром: запуск команд и проверка каталогов.
// Исключения процесса превращаются в ShellError.
object Shell:
  def run(
           command: String,
           directory: String,
           shell: String,
           clock: Clock
         ): Either[ShellError, ShellResult] =
    try
      val stdout = new StringBuilder
      val stderr = new StringBuilder
      val startTime = clock.now()
      val startedAt = System.currentTimeMillis()

      val exitCode = Process(
        Seq(shell, "-c", command),
        new File(directory)
      ).!(ProcessLogger(
        line => { stdout.append(line).append("\n"); () },
        line => { stderr.append(line).append("\n"); () }
      ))

      val duration = System.currentTimeMillis() - startedAt
      Right(ShellResult(
        exitCode = exitCode,
        stdout = stdout.toString.trim,
        stderr = stderr.toString.trim,
        startTime = startTime,
        endTime = clock.now(),
        durationMillis = duration
      ))
    catch
      case NonFatal(e) => Left(ShellError.ExecutionFailed(reason(e)))

  def resolveDirectory(currentDir: String, target: String): Either[ShellError, String] =
    try
      val home = System.getProperty("user.home")
      val targetPath =
        if target == "~" then home
        else if target.startsWith("~/") then home + "/" + target.drop(2)
        else if target.startsWith("/") then target
        else currentDir + "/" + target

      val file = new File(targetPath).getCanonicalFile
      if file.isDirectory then Right(file.getAbsolutePath)
      else Left(ShellError.DirectoryNotFound(target))
    catch
      case NonFatal(_) => Left(ShellError.DirectoryNotFound(target))

  private def reason(e: Throwable): String =
    Option(e.getMessage).filter(_.nonEmpty).getOrElse(e.getClass.getSimpleName)
