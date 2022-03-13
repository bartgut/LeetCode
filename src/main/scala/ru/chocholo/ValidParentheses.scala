package ru.chocholo

import cats.implicits._
import cats.{Foldable, Traverse}
import magnolia1._

import scala.language.experimental.macros

trait Opening[F] {
  def opening(x: F): Option[F]
}

object Opening {
  implicit def charOpening: Opening[Char] = {
    case ')' => Some('(')
    case '}' => Some('{')
    case ']' => Some('[')
    case _ => None
  }
  implicit def wrappedValue[F[_]: Traverse, A: Opening]: Opening[F[A]] =
    x => x.traverse(implicitly[Opening[A]].opening)

  type Typeclass[T] = Opening[T]

  def join[T](caseClass: CaseClass[Typeclass, T]): Typeclass[T] = x => {
    caseClass.parameters.traverse { param =>
      param.typeclass.opening(param.dereference(x))
    }.map(caseClass.rawConstruct)
  }

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]
}

case class Test(a: Char, b: Char)

object ValidParentheses {
  def isValid[F[_]: Foldable, A](s: F[A])(implicit opening: Opening[A]): Boolean =
    s.foldLeft(Right(List.empty[A]).withLeft[String]) { (acc, c) =>
      acc.flatMap { stack =>
        opening.opening(c)
          .map(opening => if (stack.headOption.contains(opening)) Right(stack.tail) else Left(stack.toString()))
          .getOrElse(Right(c :: stack))
      }
    }.map(_.lengthCompare(0) == 0).getOrElse(false)


  def main(args: Array[String]): Unit = {
    println(isValid("{".toList)) // false
    println(isValid("}".toList)) // false
    println(isValid("{{}}".toList)) // true
    println(isValid("{{]}".toList)) // false
    println(isValid(List(Option('{'), Option('{'), Option('}'), Option('}')))) //true
    println(isValid(List(Test('{','{'), Test('{','{'), Test('}','}'), Test('}','}')))) // true
  }

}
