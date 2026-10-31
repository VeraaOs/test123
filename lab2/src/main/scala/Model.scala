// Доменные данные приложения.
// В этом файле нет ни ввода-вывода, ни печати: только значения.

case class CommandRecord(
                          text: String,
                          directoryBefore: String,
                          directoryAfter: String,
                          exitCode: Int,
                          stdout: String,
                          stderr: String,
                          startTime: String,
                          endTime: String,
                          durationMillis: Long,
                          includeInReport: Boolean
                        )

case class Step(
                 title: Option[String],
                 notes: Seq[String],
                 commands: Seq[CommandRecord]
               )

case class Session(
                    name: String,
                    startTime: String,
                    rootDir: String,
                    currentDir: String,
                    shell: String,
                    profile: String,
                    steps: Seq[Step]
                  )

// Правило генерации отчёта, читается из config.json.
case class Rule(
                 id: String,
                 command: String,
                 priority: Int,
                 descriptionTemplate: String,
                 showCommand: Boolean,
                 outputMode: String,
                 maximumLines: Option[Int],
                 enabled: Boolean
               )

case class AppConfig(
                      shell: String,
                      reportDirectory: String,
                      sessionDirectory: String,
                      logFile: String,
                      defaultProfile: String,
                      rules: Seq[Rule]
                    )

object AppConfig:
  // Настройки по умолчанию: используются, если config.json не прочитан.
  val default: AppConfig = AppConfig(
    shell = "/bin/bash",
    reportDirectory = "reports",
    sessionDirectory = "sessions",
    logFile = "report-shell.log",
    defaultProfile = "linux-lab",
    rules = Seq()
  )

// Всё изменяемое состояние приложения собрано в одном значении.
case class AppState(
                     config: AppConfig,
                     session: Option[Session],
                     currentDir: String,
                     skipNext: Boolean
                   )

// Готовый отчёт: чистая функция сформировала текст и предупреждения.
case class FinishedReport(
                           session: Session,
                           text: String,
                           warnings: Seq[String]
                         )

// Представление статуса для слоя вывода.
case class SessionStatus(
                          name: String,
                          startTime: String,
                          rootDir: String,
                          currentDir: String,
                          stepName: Option[String],
                          totalCommands: Int,
                          excludedCommands: Int
                        )
