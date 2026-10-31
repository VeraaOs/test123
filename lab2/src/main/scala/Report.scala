import scala.annotation.tailrec

// Чистая сборка Markdown-отчёта. Никаких файлов и печати.

object Report:
  def generate(
                session: Session,
                rules: Seq[Rule],
                warnings: Seq[String],
                finishedAt: String
              ): String =
    val header = Seq(
      s"# ${session.name}",
      "",
      "## Сведения о сессии",
      "",
      s"- Начало: ${session.startTime}",
      s"- Завершение: $finishedAt",
      s"- Корневой каталог: `${session.rootDir}`",
      s"- Командный интерпретатор: `${session.shell}`",
      s"- Профиль отчёта: `${session.profile}`",
      s"- Количество шагов: ${session.steps.count(st => st.commands.nonEmpty || st.notes.nonEmpty)}",
      ""
    )

    val stepBlocks = session.steps.zipWithIndex.flatMap { case (step, index) =>
      stepBlock(step, index, session, rules)
    }

    val all = session.steps.flatMap(_.commands)
    val failed = all.filter(_.exitCode != 0)
    val problems =
      if warnings.nonEmpty || failed.nonEmpty then
        Seq("## Ошибки и предупреждения", "") ++
          warnings.map(w => s"- Предупреждение: $w") ++
          failed.map(c => s"- Команда `${c.text}` завершилась с кодом ${c.exitCode}.") ++
          Seq("")
      else Seq()

    val totals = Seq(
      "## Итоги",
      "",
      s"- Выполнено команд: ${all.size}",
      s"- Успешно: ${all.count(_.exitCode == 0)}",
      s"- Завершилось с ошибкой: ${all.count(_.exitCode != 0)}",
      s"- Исключено из отчёта: ${all.count(c => !c.includeInReport)}"
    )

    (header ++ stepBlocks ++ problems ++ totals).mkString("\n") + "\n"

  private def stepBlock(
                         step: Step,
                         index: Int,
                         session: Session,
                         rules: Seq[Rule]
                       ): Seq[String] =
    val shown = step.commands.filter(_.includeInReport)
    if shown.isEmpty && step.notes.isEmpty then Seq()
    else
      val heading = step.title match
        case Some(title) if title.nonEmpty => s"## Шаг ${index + 1}. $title"
        case _ => s"## Шаг ${index + 1}"

      val notes = step.notes.flatMap(note => Seq(escapeInline(note), ""))

      val commands = shown.flatMap { record =>
        val rule = findRule(rules, commandName(record.text))
        Seq(commandToMarkdown(record, rule, session, step.title.getOrElse("")), "")
      }

      Seq(heading, "") ++ notes ++ commands

  def escapeInline(s: String): String =
    s.replace("\\", "\\\\").replace("`", "\\`").replace("*", "\\*").replace("_", "\\_")

  def fenceFor(content: String): String =
    @tailrec
    def widen(fence: String): String =
      if content.contains(fence) then widen(fence + "`") else fence
    widen("```")

  def formatDuration(ms: Long): String =
    if ms < 1000 then s"$ms мс" else f"${ms / 1000.0}%.1f с"

  def relativePath(session: Session, dir: String): String =
    if SessionLogic.isInsideRoot(session, dir) then
      dir.stripPrefix(session.rootDir).stripPrefix("/")
    else dir

  def fillTemplate(template: String, values: Map[String, String]): String =
    values.foldLeft(template) { case (result, (key, value)) =>
      result.replace("{" + key + "}", value)
    }

  def findRule(rules: Seq[Rule], commandName: String): Option[Rule] =
    rules
      .filter(r => r.enabled && r.command == commandName)
      .sortBy(r => -r.priority)
      .headOption

  // Режимы включения вывода.
  def outputBlock(record: CommandRecord, mode: String, maxLines: Option[Int]): String =
    val limit = maxLines.getOrElse(10)
    val lines = if record.stdout.isEmpty then Seq.empty[String] else record.stdout.split("\n").toSeq

    def whole: String =
      val fence = fenceFor(record.stdout)
      s"\n$fence\n${record.stdout}\n$fence\n"

    def truncated(shown: Seq[String], label: String): String =
      val fence = fenceFor(shown.mkString("\n"))
      s"\n$fence\n${shown.mkString("\n")}\n$fence\n\n*Показаны $label ${shown.size} строк из ${lines.size}.*\n"

    mode match
      case "full" => if lines.isEmpty then "" else whole
      case "errors-only" =>
        if record.stderr.nonEmpty then
          val fence = fenceFor(record.stderr)
          s"\n$fence\n${record.stderr}\n$fence\n"
        else ""
      case "none" => ""
      case "first-lines" | "limited" =>
        if lines.size <= limit then whole else truncated(lines.take(limit), "первые")
      case "last-lines" =>
        if lines.size <= limit then whole else truncated(lines.takeRight(limit), "последние")
      case "summary" =>
        s"\nВывод: ${lines.size} строк, код завершения: `${record.exitCode}`.\n"
      case _ => ""

  def commandToMarkdown(
                         record: CommandRecord,
                         rule: Option[Rule],
                         session: Session,
                         stepTitle: String
                       ): String =
    val parts = record.text.split(" ").toSeq
    val values = Map(
      "command" -> record.text,
      "commandName" -> parts.headOption.getOrElse(""),
      "arguments" -> parts.drop(1).mkString(" "),
      "argument1" -> parts.lift(1).getOrElse(""),
      "argument2" -> parts.lift(2).getOrElse(""),
      "currentDirectory" -> record.directoryAfter,
      "relativeDirectory" -> relativePath(session, record.directoryAfter),
      "exitCode" -> record.exitCode.toString,
      "duration" -> formatDuration(record.durationMillis),
      "stepTitle" -> stepTitle
    )

    rule match
      case Some(r) =>
        val description = fillTemplate(r.descriptionTemplate, values)
        val commandBlock =
          if r.showCommand then
            val fence = fenceFor(record.text)
            s"\nКоманда:\n$fence bash\n${record.text}\n$fence\n"
          else ""
        description + commandBlock + outputBlock(record, r.outputMode, r.maximumLines)
      case None =>
        val fence = fenceFor(record.text)
        val base = s"Выполним команду:\n$fence bash\n${record.text}\n$fence\n"
        val out =
          if record.stdout.nonEmpty then
            val f2 = fenceFor(record.stdout)
            s"\nРезультат:\n$f2\n${record.stdout}\n$f2\n"
          else ""
        val err =
          if record.exitCode != 0 then
            val errBlock =
              if record.stderr.nonEmpty then
                val f3 = fenceFor(record.stderr)
                s"\n$f3\n${record.stderr}\n$f3\n"
              else ""
            s"\nКод завершения: `${record.exitCode}`." + errBlock + "\n"
          else ""
        base + out + err

  private def commandName(text: String): String =
    text.split(" ").headOption.getOrElse("")
