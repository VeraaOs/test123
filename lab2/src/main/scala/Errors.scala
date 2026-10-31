// Ожидаемые ошибки разных уровней.
// Ни один вариант не кодируется через null, -1, пустую строку или просто false.

// Ошибки разбора пользовательской строки.
enum CommandError:
  case EmptyInput
  case UnterminatedQuote(command: String)

// Ошибки операций над сессией.
enum SessionError:
  case NoActiveSession

// Ошибки слоя хранения (config.json, файлы отчётов).
enum StorageError:
  case FileNotFound(path: String)
  case ReadFailed(path: String, reason: String)
  case InvalidConfig(path: String, reason: String)
  case WriteFailed(path: String, reason: String)

// Ошибки выполнения внешних команд и работы с каталогами.
enum ShellError:
  case DirectoryNotFound(path: String)
  case ExecutionFailed(reason: String)

// Ошибка уровня приложения: сюда поднимается только то,
// что не может быть осмысленно исправлено на нижнем уровне.
enum AppError:
  case Storage(error: StorageError)
