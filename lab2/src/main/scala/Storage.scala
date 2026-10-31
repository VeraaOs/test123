import upickle.default.*
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import scala.util.Try
import scala.util.control.NonFatal

// Зеркала JSON-структур для чтения config.json.
case class RuleJson(
                     id: String,
                     command: String,
                     priority: Int,
                     descriptionTemplate: String,
                     showCommand: Boolean,
                     outputMode: String,
                     maximumLines: Option[Int] = None,
                     enabled: Boolean
                   ) derives ReadWriter

case class ConfigJson(
                       shell: String,
                       reportDirectory: String,
                       sessionDirectory: String,
                       logFile: String,
                       defaultProfile: String,
                       rules: Seq[RuleJson]
                     ) derives ReadWriter

// Слой хранения. Исключения Java API ловятся здесь и превращаются
// в типизированные StorageError.
object Storage:
  def loadConfig(path: Path): Either[StorageError, AppConfig] =
    if !Files.exists(path) then Left(StorageError.FileNotFound(path.toString))
    else
      try
        val text = Files.readString(path, StandardCharsets.UTF_8)
        Try(read[ConfigJson](text)).toEither
          .left
          .map(e => StorageError.InvalidConfig(path.toString, reason(e)))
          .map(toConfig)
      catch
        case NonFatal(e) => Left(StorageError.ReadFailed(path.toString, reason(e)))

  def saveReport(
                  reportDirectory: String,
                  fileName: String,
                  text: String
                ): Either[StorageError, Path] =
    try
      val directory = Paths.get(reportDirectory)
      Files.createDirectories(directory)
      val file = directory.resolve(fileName)
      Files.writeString(
        file,
        text,
        StandardCharsets.UTF_8,
        StandardOpenOption.CREATE,
        StandardOpenOption.TRUNCATE_EXISTING,
        StandardOpenOption.WRITE
      )
      Right(file)
    catch
      case NonFatal(e) =>
        Left(StorageError.WriteFailed(Paths.get(reportDirectory).toString, reason(e)))

  private def toConfig(json: ConfigJson): AppConfig =
    AppConfig(
      shell = json.shell,
      reportDirectory = json.reportDirectory,
      sessionDirectory = json.sessionDirectory,
      logFile = json.logFile,
      defaultProfile = json.defaultProfile,
      rules = json.rules.map { r =>
        Rule(
          id = r.id,
          command = r.command,
          priority = r.priority,
          descriptionTemplate = r.descriptionTemplate,
          showCommand = r.showCommand,
          outputMode = r.outputMode,
          maximumLines = r.maximumLines,
          enabled = r.enabled
        )
      }
    )

  private def reason(e: Throwable): String =
    Option(e.getMessage).filter(_.nonEmpty).getOrElse(e.getClass.getSimpleName)
