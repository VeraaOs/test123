import scala.io.StdIn

// Пункт меню хранит значение, которое нужно вернуть при выборе.
// Номер пункта не хранится: он печатается по порядку.
case class MenuOption[A](text: String, value: A)

// Весь ввод-вывод приложения спрятан за этим интерфейсом.
// В тестах можно подставить реализацию без консоли.
trait Ui:
  def print(text: String): Unit
  def println(text: String): Unit
  def readLine(prompt: String): Option[String]
  def choose[A](prompt: String, options: Seq[MenuOption[A]]): Option[A]

object ConsoleUi extends Ui:
  def print(text: String): Unit =
    Predef.print(text)

  def println(text: String): Unit =
    Predef.println(text)

  def readLine(prompt: String): Option[String] =
    Predef.print(prompt)
    Option(StdIn.readLine())

  def choose[A](prompt: String, options: Seq[MenuOption[A]]): Option[A] =
    options.zipWithIndex.foreach { case (option, index) =>
      Predef.println(s"${index + 1}) ${option.text}")
    }
    readLine(prompt).flatMap { raw =>
      raw.trim.toIntOption
        .flatMap(number => options.lift(number - 1))
        .map(_.value)
    }
