// Бизнес-логика сессии: чистые преобразования значений.
// Здесь нет println, чтения ввода и работы с файлами.

object SessionLogic:
  def newSession(
                  name: String,
                  startTime: String,
                  rootDir: String,
                  shell: String,
                  profile: String
                ): Session =
    val safeName = if name.trim.isEmpty then "Без названия" else name
    Session(
      name = safeName,
      startTime = startTime,
      rootDir = rootDir,
      currentDir = rootDir,
      shell = shell,
      profile = profile,
      steps = Seq()
    )

  def addStep(session: Option[Session], title: Option[String]): Either[SessionError, Session] =
    session.toRight(SessionError.NoActiveSession).map { s =>
      s.copy(steps = s.steps :+ Step(title, Seq(), Seq()))
    }

  def addNote(session: Option[Session], text: String): Either[SessionError, Session] =
    session.toRight(SessionError.NoActiveSession).map { s =>
      val withStep = ensureStep(s)
      val last = withStep.steps.last
      val updated = last.copy(notes = last.notes :+ text)
      withStep.copy(steps = withStep.steps.updated(withStep.steps.size - 1, updated))
    }

  def setRoot(session: Option[Session], rootDir: String): Either[SessionError, Session] =
    session.toRight(SessionError.NoActiveSession).map(_.copy(rootDir = rootDir))

  def record(
              session: Option[Session],
              record: CommandRecord,
              skipNext: Boolean
            ): Either[SessionError, Session] =
    session.toRight(SessionError.NoActiveSession).map { s =>
      val withStep = ensureStep(s)
      val last = withStep.steps.last
      val marked = record.copy(includeInReport = !skipNext)
      val updated = last.copy(commands = last.commands :+ marked)
      withStep.copy(
        steps = withStep.steps.updated(withStep.steps.size - 1, updated),
        currentDir = marked.directoryAfter
      )
    }

  def finish(
              session: Option[Session],
              rules: Seq[Rule],
              finishedAt: String
            ): Either[SessionError, FinishedReport] =
    session.toRight(SessionError.NoActiveSession).map { s =>
      val warnings = collectWarnings(s, rules)
      FinishedReport(s, Report.generate(s, rules, warnings, finishedAt), warnings)
    }

  def preview(
               session: Option[Session],
               rules: Seq[Rule],
               generatedAt: String
             ): Either[SessionError, String] =
    session.toRight(SessionError.NoActiveSession)
      .map(s => Report.generate(s, rules, collectWarnings(s, rules), generatedAt))

  def status(session: Option[Session], currentDir: String): Either[SessionError, SessionStatus] =
    session.toRight(SessionError.NoActiveSession).map { s =>
      val all = allCommands(s)
      SessionStatus(
        name = s.name,
        startTime = s.startTime,
        rootDir = s.rootDir,
        currentDir = currentDir,
        stepName = s.steps.lastOption.map(step => stepDisplayName(step, s.steps.size - 1)),
        totalCommands = all.size,
        excludedCommands = all.count(c => !c.includeInReport)
      )
    }

  def collectWarnings(session: Session, rules: Seq[Rule]): Seq[String] =
    val emptyWarnings =
      if session.steps.isEmpty then Seq("в сессии нет ни одного шага")
      else if session.steps.forall(st => st.commands.isEmpty) then Seq("в сессии нет записанных команд")
      else Seq()

    val conflicts =
      rules
        .filter(_.enabled)
        .groupBy(r => (r.command, r.priority))
        .toSeq
        .sortBy { case ((command, priority), _) => (command, priority) }
        .collect {
          case ((command, priority), group) if group.size > 1 =>
            s"конфликт правил для '$command' с приоритетом $priority: ${group.map(_.id).mkString(", ")}"
        }

    emptyWarnings ++ conflicts

  def allCommands(session: Session): Seq[CommandRecord] =
    session.steps.flatMap(_.commands)

  // Проверяет, что путь лежит внутри корневого каталога сессии.
  // Сравниваем по границе каталога, а не по простому префиксу строки:
  // /tmp/project-other не должен считаться частью /tmp/project.
  def isInsideRoot(session: Session, dir: String): Boolean =
    val root = session.rootDir
    val prefix = if root.endsWith("/") then root else root + "/"
    dir == root || dir.startsWith(prefix)

  def stepDisplayName(step: Step, index: Int): String =
    step.title.filter(_.nonEmpty).getOrElse(s"Шаг ${index + 1}")

  // Превращает название сессии в безопасное имя файла.
  def slug(name: String): String =
    val s = name.toLowerCase.replaceAll("[^a-z0-9а-яё]+", "-").stripPrefix("-").stripSuffix("-")
    if s.isEmpty then "report" else s

  private def ensureStep(session: Session): Session =
    if session.steps.isEmpty then session.copy(steps = Seq(Step(None, Seq(), Seq())))
    else session
