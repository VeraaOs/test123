// Чистое представление данных для консоли.
// Здесь только формирование строк, без чтения ввода и записи файлов.

object View:
  val help: Seq[String] = Seq(
    "Служебные команды:",
    "  starts \"Название\"  — начать сессию",
    "  ends                — завершить сессию и сохранить отчёт",
    "  prootd <путь>       — установить корневой каталог",
    "  mstep \"Название\"    — начать новый шаг",
    "  mnote \"Текст\"       — добавить заметку к шагу",
    "  mskip               — исключить следующую команду из отчёта",
    "  mstatus             — показать состояние сессии",
    "  mpreview            — создать предварительный отчёт",
    "  exit                — выйти из программы"
  )

  def status(status: SessionStatus): Seq[String] = Seq(
    s"Сессия: ${status.name}",
    s"Время начала: ${status.startTime}",
    s"Корневой каталог: ${status.rootDir}",
    s"Текущий каталог: ${status.currentDir}",
    s"Текущий шаг: ${status.stepName.getOrElse("(шага нет)")}",
    s"Записано команд: ${status.totalCommands}",
    s"Исключено команд: ${status.excludedCommands}"
  )
