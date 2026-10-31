import java.nio.file.{Path, Paths}
import scala.annotation.tailrec
import scala.util.control.NonFatal

// Точка входа: связывает разбор команды, бизнес-логику,
// выполнение команд и консольный интерфейс.

@main
def main(): Unit =
  val ui = ConsoleUi
  val clock = SystemClock
  val config = loadConfig(ui)

  ui.println("report-shell запущен. Введите 'help' для списка команд.")
  runLoop(AppState(config, None, currentDirectory(), skipNext = false), ui, clock)

private def runLoop(initial: AppState, ui: Ui, clock: Clock): Unit =
  @tailrec
  def loop(state: AppState): Unit =
    ui.print("report-shell> ")
    ui.readLine("") match
      case None => ()
      case Some(line) =>
        val trimmed = line.trim
        if trimmed.isEmpty then loop(state)
        else
          CommandParser.parse(trimmed) match
            case Left(error) =>
              ui.println(Messages.commandError(error))
              loop(state)
            case Right(command) =>
              val (next, continue) = handle(command, state, ui, clock)
              if continue then loop(next)

  loop(initial)

private def handle(
                    command: Command,
                    state: AppState,
                    ui: Ui,
                    clock: Clock
                  ): (AppState, Boolean) =
  command match
    case Command.StartSession(name) =>
      state.session match
        case None =>
          (startNewSession(state, name, ui, clock), true)
        case Some(active) =>
          ui.println(s"Уже есть активная сессия: ${active.name}")
          ui.choose("Ваш выбор: ", SessionMenu.options) match
            case Some(StartAction.ContinueCurrent) =>
              ui.println("Продолжаем текущую сессию.")
              (state, true)
            case Some(StartAction.FinishCurrentAndStartNew) =>
              val ended = endSession(state, ui, clock)
              (startNewSession(ended, name, ui, clock), true)
            case Some(StartAction.DiscardCurrentAndStartNew) =>
              ui.println("Незавершённая сессия отменена.")
              (startNewSession(state.copy(session = None), name, ui, clock), true)
            case None =>
              ui.println("Неизвестный выбор. Оставлена текущая сессия.")
              (state, true)

    case Command.EndSession =>
      (endSession(state, ui, clock), true)

    case Command.AddStep(title) =>
      SessionLogic.addStep(state.session, title) match
        case Left(SessionError.NoActiveSession) =>
          ui.println("Сначала начните сессию: starts \"Название\"")
          (state, true)
        case Right(updated) =>
          val index = updated.steps.size - 1
          ui.println(s"Новый шаг: ${SessionLogic.stepDisplayName(updated.steps.last, index)}")
          (state.copy(session = Some(updated)), true)

    case Command.AddNote(text) =>
      SessionLogic.addNote(state.session, text) match
        case Left(error) =>
          ui.println(Messages.sessionError(error))
          (state, true)
        case Right(updated) =>
          ui.println("Заметка добавлена.")
          (state.copy(session = Some(updated)), true)

    case Command.SkipNextCommand =>
      ui.println("Следующая команда будет выполнена, но не попадёт в отчёт.")
      (state.copy(skipNext = true), true)

    case Command.ShowStatus =>
      SessionLogic.status(state.session, state.currentDir) match
        case Left(error) =>
          ui.println(Messages.sessionError(error))
          (state, true)
        case Right(status) =>
          ui.println(View.status(status).mkString("\n"))
          (state, true)

    case Command.SetRootDirectory(path) =>
      state.session match
        case None =>
          ui.println("Нет активной сессии.")
          (state, true)
        case Some(_) =>
          val requested = path.getOrElse(".")
          val resolved =
            if requested == "." then Right(state.currentDir)
            else Shell.resolveDirectory(state.currentDir, requested)
          resolved match
            case Left(error) =>
              ui.println(Messages.shellError(error))
              (state, true)
            case Right(target) =>
              SessionLogic.setRoot(state.session, target) match
                case Left(error) =>
                  ui.println(Messages.sessionError(error))
                  (state, true)
                case Right(updated) =>
                  ui.println(s"Корневой каталог установлен: $target")
                  (state.copy(session = Some(updated)), true)

    case Command.PreviewReport =>
      state.session match
        case None =>
          ui.println("Нет активной сессии.")
          (state, true)
        case Some(active) =>
          SessionLogic.preview(Some(active), state.config.rules, clock.now()) match
            case Left(error) =>
              ui.println(Messages.sessionError(error))
              (state, true)
            case Right(report) =>
              val fileName = "draft-" + SessionLogic.slug(active.name) + ".md"
              persistReport(
                state.config.reportDirectory,
                fileName,
                report,
                path => s"Предварительный отчёт создан: ${path.toString}",
                ui
              )
              (state, true)

    case Command.ChangeDirectory(target, text) =>
      val requested = target.getOrElse("~")
      Shell.resolveDirectory(state.currentDir, requested) match
        case Left(error) =>
          ui.println(Messages.shellError(error))
          (state, true)
        case Right(newDir) =>
          val now = clock.now()
          val record = CommandRecord(
            text = text,
            directoryBefore = state.currentDir,
            directoryAfter = newDir,
            exitCode = 0,
            stdout = "",
            stderr = "",
            startTime = now,
            endTime = now,
            durationMillis = 0,
            includeInReport = true
          )
          val moved = state.copy(currentDir = newDir)
          val recorded = SessionLogic.record(state.session, record, state.skipNext) match
            case Right(updated) => moved.copy(session = Some(updated), skipNext = false)
            case Left(_) => moved
          warnIfOutsideRoot(recorded, ui)
          (recorded, true)

    case Command.RunShell(text) =>
      Shell.run(text, state.currentDir, state.config.shell, clock) match
        case Left(error) =>
          ui.println(Messages.shellError(error))
          (state, true)
        case Right(result) =>
          if result.stdout.nonEmpty then ui.println(result.stdout)
          if result.stderr.nonEmpty then ui.println(result.stderr)
          if result.exitCode != 0 then
            ui.println(s"Команда завершилась с кодом ${result.exitCode}.")

          val record = CommandRecord(
            text = text,
            directoryBefore = state.currentDir,
            directoryAfter = state.currentDir,
            exitCode = result.exitCode,
            stdout = result.stdout,
            stderr = result.stderr,
            startTime = result.startTime,
            endTime = result.endTime,
            durationMillis = result.durationMillis,
            includeInReport = true
          )

          SessionLogic.record(state.session, record, state.skipNext) match
            case Right(updated) => (state.copy(session = Some(updated), skipNext = false), true)
            case Left(_) => (state, true)

    case Command.Help =>
      ui.println(View.help.mkString("\n"))
      (state, true)

    case Command.Exit =>
      (state, false)

private def startNewSession(
                             state: AppState,
                             name: Option[String],
                             ui: Ui,
                             clock: Clock
                           ): AppState =
  val session = SessionLogic.newSession(
    name = name.getOrElse(""),
    startTime = clock.now(),
    rootDir = state.currentDir,
    shell = state.config.shell,
    profile = state.config.defaultProfile
  )
  ui.println(s"Сессия начата: ${session.name}")
  state.copy(session = Some(session))

private def endSession(state: AppState, ui: Ui, clock: Clock): AppState =
  SessionLogic.finish(state.session, state.config.rules, clock.now()) match
    case Left(error) =>
      ui.println(Messages.sessionError(error))
      state
    case Right(finished) =>
      finished.warnings.foreach(w => ui.println(s"Предупреждение: $w"))
      ui.println("Сессия завершена.")
      val fileName = SessionLogic.slug(finished.session.name) + ".md"
      persistReport(
        state.config.reportDirectory,
        fileName,
        finished.text,
        path => s"Отчёт сохранён: ${path.toString}",
        ui
      )
      state.copy(session = None)

private def persistReport(
                           directory: String,
                           fileName: String,
                           text: String,
                           successMessage: Path => String,
                           ui: Ui
                         ): Unit =
  Storage.saveReport(directory, fileName, text)
    .left
    .map(AppError.Storage.apply)
    .fold(
      error => ui.println(Messages.appError(error)),
      path => ui.println(successMessage(path))
    )

private def warnIfOutsideRoot(state: AppState, ui: Ui): Unit =
  state.session
    .filter(session => !SessionLogic.isInsideRoot(session, state.currentDir))
    .foreach { _ =>
      ui.println("Предупреждение: текущий каталог находится вне корневого каталога сессии.")
      ui.println("В отчёте будет использован абсолютный путь.")
    }

private def loadConfig(ui: Ui): AppConfig =
  Storage.loadConfig(Paths.get("config.json")) match
    case Right(config) =>
      ui.println(s"Конфигурация загружена: ${config.rules.size} правил, профиль '${config.defaultProfile}'.")
      config
    case Left(error) =>
      ui.println(s"Внимание: ${Messages.storageError(error)}")
      ui.println("Будут использованы настройки по умолчанию.")
      AppConfig.default

private def currentDirectory(): String =
  try Paths.get(".").toRealPath().toString
  catch
    case NonFatal(_) => Paths.get(".").toAbsolutePath.normalize().toString
