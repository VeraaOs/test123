// Действия диалога при уже активной сессии.
enum StartAction:
  case ContinueCurrent
  case FinishCurrentAndStartNew
  case DiscardCurrentAndStartNew

object SessionMenu:
  // Текст пункта и его действие лежат рядом, поэтому рассогласовать их нельзя.
  val options: Seq[MenuOption[StartAction]] = Seq(
    MenuOption("Продолжить её", StartAction.ContinueCurrent),
    MenuOption("Завершить и сформировать отчёт", StartAction.FinishCurrentAndStartNew),
    MenuOption("Отменить незавершённую сессию", StartAction.DiscardCurrentAndStartNew)
  )
