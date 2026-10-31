import java.nio.charset.StandardCharsets
import java.nio.file.Files

class Lab2Suite extends munit.FunSuite:

  test("parse: starts with quoted name") {
    assertEquals(
      CommandParser.parse("starts \"Моя сессия\""),
      Right(Command.StartSession(Some("Моя сессия")))
    )
  }

  test("parse: starts without a name") {
    assertEquals(CommandParser.parse("starts"), Right(Command.StartSession(None)))
    assertEquals(CommandParser.parse("starts   "), Right(Command.StartSession(None)))
  }

  test("parse: unterminated quote is a typed error") {
    assertEquals(
      CommandParser.parse("starts \"Моя сессия"),
      Left(CommandError.UnterminatedQuote("starts"))
    )
  }

  test("parse: known commands") {
    assertEquals(CommandParser.parse("ends"), Right(Command.EndSession))
    assertEquals(CommandParser.parse("mskip"), Right(Command.SkipNextCommand))
    assertEquals(CommandParser.parse("mstatus"), Right(Command.ShowStatus))
    assertEquals(CommandParser.parse("help"), Right(Command.Help))
    assertEquals(CommandParser.parse("quit"), Right(Command.Exit))
  }

  test("parse: ordinary command keeps its text") {
    assertEquals(CommandParser.parse("ls -la"), Right(Command.RunShell("ls -la")))
    assertEquals(CommandParser.parse("  echo hi  "), Right(Command.RunShell("echo hi")))
  }

  test("parse: cd without argument and with quoted path") {
    assertEquals(CommandParser.parse("cd"), Right(Command.ChangeDirectory(None, "cd")))
    assertEquals(
      CommandParser.parse("cd \"my dir\""),
      Right(Command.ChangeDirectory(Some("my dir"), "cd \"my dir\""))
    )
  }

  test("parse: optional text arguments") {
    assertEquals(CommandParser.parse("mnote"), Right(Command.AddNote("")))
    assertEquals(CommandParser.parse("mstep"), Right(Command.AddStep(None)))
    assertEquals(CommandParser.parse("mstep \"a b\""), Right(Command.AddStep(Some("a b"))))
    assertEquals(CommandParser.parse("prootd"), Right(Command.SetRootDirectory(None)))
  }

  test("session: empty name becomes default") {
    val session = SessionLogic.newSession("", "10:00", "/tmp", "/bin/bash", "linux-lab")
    assertEquals(session.name, "Без названия")
    assertEquals(session.steps, Seq())
  }

  test("session: operations require an active session") {
    assertEquals(SessionLogic.addStep(None, None), Left(SessionError.NoActiveSession))
    assertEquals(SessionLogic.addNote(None, "note"), Left(SessionError.NoActiveSession))
    assertEquals(SessionLogic.setRoot(None, "/tmp"), Left(SessionError.NoActiveSession))
  }

  test("session: adding a note creates the first step") {
    val session = SessionLogic.newSession("S", "10:00", "/tmp", "/bin/bash", "p")
    SessionLogic.addNote(Some(session), "первая заметка") match
      case Left(error) => fail(s"unexpected error: $error")
      case Right(updated) =>
        assertEquals(updated.steps.size, 1)
        assertEquals(updated.steps.head.notes, Seq("первая заметка"))
        assertEquals(updated.steps.head.title, None)
  }

  test("session: record respects skipNext and updates current dir") {
    val session = SessionLogic.newSession("S", "10:00", "/tmp", "/bin/bash", "p")
    val record = CommandRecord(
      "echo hi", "/tmp", "/tmp/sub", 0, "hi", "", "10:01", "10:02", 1000, includeInReport = true
    )
    SessionLogic.record(Some(session), record, skipNext = true) match
      case Left(error) => fail(s"unexpected error: $error")
      case Right(updated) =>
        assertEquals(updated.steps.head.commands.head.includeInReport, false)
        assertEquals(updated.currentDir, "/tmp/sub")
  }

  test("session: status counts all commands and excluded ones") {
    val record = (included: Boolean) => CommandRecord(
      "echo", "/tmp", "/tmp", 0, "", "", "10:00", "10:00", 0, included
    )
    val session = Session(
      "S", "10:00", "/tmp", "/tmp", "/bin/bash", "p",
      Seq(Step(Some("Шаг"), Seq(), Seq(record(true), record(false))))
    )
    SessionLogic.status(Some(session), "/tmp") match
      case Left(error) => fail(s"unexpected error: $error")
      case Right(status) =>
        assertEquals(status.totalCommands, 2)
        assertEquals(status.excludedCommands, 1)
        assertEquals(status.stepName, Some("Шаг"))
  }

  test("session: warnings include empty session and rule conflicts") {
    val session = Session("S", "10:00", "/tmp", "/tmp", "/bin/bash", "p", Seq())
    val rules = Seq(
      Rule("a", "ls", 10, "", false, "none", None, true),
      Rule("b", "ls", 10, "", false, "none", None, true)
    )
    val warnings = SessionLogic.collectWarnings(session, rules)
    assert(warnings.exists(_.contains("нет ни одного шага")))
    assert(warnings.exists(_.contains("конфликт правил")))
  }

  test("report: contains session, step, command and totals") {
    val record = CommandRecord(
      "echo hi", "/tmp", "/tmp", 0, "hi", "", "10:00", "10:01", 1000, includeInReport = true
    )
    val session = Session(
      "Демо", "10:00", "/tmp", "/tmp", "/bin/bash", "p",
      Seq(Step(Some("Первый шаг"), Seq("заметка"), Seq(record)))
    )
    val report = Report.generate(session, Seq(), Seq(), "10:02")
    assert(report.contains("# Демо"))
    assert(report.contains("## Шаг 1. Первый шаг"))
    assert(report.contains("заметка"))
    assert(report.contains("echo hi"))
    assert(report.contains("- Выполнено команд: 1"))
  }

  test("report: skipped command is counted but not shown") {
    val record = CommandRecord(
      "secret", "/tmp", "/tmp", 0, "", "", "10:00", "10:00", 0, includeInReport = false
    )
    val session = Session(
      "S", "10:00", "/tmp", "/tmp", "/bin/bash", "p",
      Seq(Step(None, Seq(), Seq(record)))
    )
    val report = Report.generate(session, Seq(), Seq(), "10:02")
    assert(!report.contains("secret"))
    assert(report.contains("- Исключено из отчёта: 1"))
  }

  test("report: output modes") {
    val record = CommandRecord(
      "seq 1 3", "/tmp", "/tmp", 1, "a\nb\nc", "err", "10:00", "10:00", 0, includeInReport = true
    )
    assertEquals(Report.outputBlock(record, "none", None), "")
    assert(Report.outputBlock(record, "full", None).contains("a\nb\nc"))
    assert(Report.outputBlock(record, "errors-only", None).contains("err"))
    assert(Report.outputBlock(record, "first-lines", Some(2)).contains("Показаны первые 2 строк из 3"))
    assert(Report.outputBlock(record, "last-lines", Some(1)).contains("Показаны последние 1 строк из 3"))
    assert(Report.outputBlock(record, "summary", None).contains("Вывод: 3 строк"))
  }

  test("report: rule with greater priority wins, disabled rule is ignored") {
    val rules = Seq(
      Rule("low", "echo", 1, "low", false, "none", None, true),
      Rule("high", "echo", 10, "high", false, "none", None, true),
      Rule("off", "echo", 100, "off", false, "none", None, false)
    )
    assertEquals(Report.findRule(rules, "echo").map(_.id), Some("high"))
    assertEquals(Report.findRule(rules, "cat"), None)
  }

  test("session: preview builds the same text and finish returns warnings") {
    val session = Session("S", "10:00", "/tmp", "/tmp", "/bin/bash", "p", Seq())
    val expected = Report.generate(session, Seq(), Seq("в сессии нет ни одного шага"), "10:02")
    assertEquals(
      SessionLogic.preview(Some(session), Seq(), "10:02"),
      Right(expected)
    )
    SessionLogic.finish(Some(session), Seq(), "10:02") match
      case Left(error) => fail(s"unexpected error: $error")
      case Right(finished) =>
        assertEquals(finished.warnings, Seq("в сессии нет ни одного шага"))
        assertEquals(finished.text, expected)
  }

  test("storage: reads config and reports invalid json") {
    val valid = Files.createTempFile("config", ".json")
    Files.writeString(
      valid,
      """{"shell":"/bin/bash","reportDirectory":"r","sessionDirectory":"s",
        |"logFile":"l","defaultProfile":"p","rules":[]}""".stripMargin,
      StandardCharsets.UTF_8
    )
    Storage.loadConfig(valid) match
      case Left(error) => fail(s"unexpected error: $error")
      case Right(config) =>
        assertEquals(config.shell, "/bin/bash")
        assertEquals(config.defaultProfile, "p")

    val invalid = Files.createTempFile("config-bad", ".json")
    Files.writeString(invalid, "{ not json", StandardCharsets.UTF_8)
    assert(Storage.loadConfig(invalid).isLeft)
  }

  test("storage: saves a report and creates the directory") {
    val temp = Files.createTempDirectory("lab2-reports")
    val directory = temp.resolve("nested")
    Storage.saveReport(directory.toString, "report.md", "# Report") match
      case Left(error) => fail(s"unexpected error: $error")
      case Right(path) =>
        assertEquals(Files.readString(path, StandardCharsets.UTF_8), "# Report")
  }

  test("shell: resolves existing and missing directories") {
    val temp = Files.createTempDirectory("lab2")
    assertEquals(
      Shell.resolveDirectory(temp.toString, temp.toString),
      Right(temp.toRealPath().toString)
    )
    assert(Shell.resolveDirectory(temp.toString, "no-such-dir").isLeft)
  }

  test("shell: runs a command and captures output") {
    Shell.run("printf hello", "/tmp", "/bin/bash", SystemClock) match
      case Left(error) => fail(s"unexpected error: $error")
      case Right(result) =>
        assertEquals(result.exitCode, 0)
        assertEquals(result.stdout, "hello")
  }

  test("session: root containment respects directory boundaries") {
    val session = Session(
      "S", "10:00", "/tmp/project", "/tmp/project", "/bin/bash", "p", Seq()
    )
    assert(SessionLogic.isInsideRoot(session, "/tmp/project"))
    assert(SessionLogic.isInsideRoot(session, "/tmp/project/sub"))
    assert(!SessionLogic.isInsideRoot(session, "/tmp/project-other"))
    assertEquals(Report.relativePath(session, "/tmp/project/sub"), "sub")
    assertEquals(Report.relativePath(session, "/tmp/project-other"), "/tmp/project-other")
  }

  test("slug: makes a safe file name") {
    assertEquals(SessionLogic.slug("Моя сессия / тест"), "моя-сессия-тест")
    assertEquals(SessionLogic.slug("!!!"), "report")
  }
