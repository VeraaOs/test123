// Преобразование типизированных ошибок в текст.
// Сами ошибки остаются данными, а не строками.

object Messages:
  def commandError(error: CommandError): String =
    error match
      case CommandError.EmptyInput => "Пустая команда."
      case CommandError.UnterminatedQuote(command) =>
        s"Незакрытая кавычка в команде '$command'."

  def sessionError(error: SessionError): String =
    error match
      case SessionError.NoActiveSession => "Нет активной сессии."

  def storageError(error: StorageError): String =
    error match
      case StorageError.FileNotFound(path) =>
        s"Файл не найден: $path"
      case StorageError.ReadFailed(path, reason) =>
        s"Не удалось прочитать $path: $reason"
      case StorageError.InvalidConfig(path, reason) =>
        s"Некорректный конфигурационный файл $path: $reason"
      case StorageError.WriteFailed(path, reason) =>
        s"Не удалось записать $path: $reason"

  def shellError(error: ShellError): String =
    error match
      case ShellError.DirectoryNotFound(path) => s"Каталог не существует: $path"
      case ShellError.ExecutionFailed(reason) => s"Не удалось выполнить команду: $reason"

  def appError(error: AppError): String =
    error match
      case AppError.Storage(inner) => storageError(inner)
