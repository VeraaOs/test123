import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

// Время передаётся в бизнес-логику снаружи, поэтому её можно тестировать.
trait Clock:
  def now(): String

object SystemClock extends Clock:
  private val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")

  def now(): String =
    LocalDateTime.now().format(formatter)
