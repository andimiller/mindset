package net.andimiller.mindset

import cats._, cats.syntax._, cats.implicits._
import com.monovore.decline._
import fastparse.all._
import org.bykn.fastparse_cats.StringInstances._

import scala.language.implicitConversions

object AST {
  sealed trait Block
  case class Text(s: String) extends Block
  case class Heading(index: Int) extends Block
  case class Bullet() extends Block
  case class Bold(value: Seq[Block]) extends Block
  case object Newline extends Block

}

object Parser {
  import AST._

  def enclosed[A, B, C](open: Parser[A], contents: Parser[B], close: Parser[C]): Parser[(A, Seq[B], C)] = open ~/ (!close ~ contents).rep(min=1) ~ close

  val heading = P("#").map(_ => 1).rep(min = 1, max = 6).map{ xs => Heading(xs.length) }
  val newline: Parser[Newline.type] = P("\n").map(_ => Newline).log()
  val onechar: Parser[Text] = AnyChars(1).!.map(x => Text(x)).log()
  lazy val strong: Parser[Bold] = enclosed(P("b"), onechar, P("b")).map{case (_, x, _) => Bold(x)}.log()
  lazy val expr: Parser[Block] = (newline | strong | onechar).log()
}

object CLI {
  val file = Opts.arguments[String]("file")
  val run = Opts.subcommand("run", "run something")(file.orEmpty).map { _ =>
    val r = Parser.strong.parse("baab")
    println(r)
  }

}

object Main extends CommandApp(
  name = "mindset",
  header = "typesets things from your mind",
  main = {
    CLI.run
  }

)