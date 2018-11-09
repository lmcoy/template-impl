package io_intro

import scala.annotation.tailrec

object Imp extends App {

  // Referential Transparency
  //
  // We can replace any function call with its result w/o changing the program.
  //


  // Pure Functions
  //
  // * return value is **only** determined by input values
  //        (no variation via static variables, global variables, IO...)
  // * any input value has a defined return value
  // * no (visible) side effects
  //        (no change of any static variables, global variables, IO...)


  // Benefits:
  // you can argue about a small function itself. don't have think about side effects.

  // -------------------------------------------------------------

  // impure functions:

  // no return value for negative `x`
  def sqrt(x: Double): Double = {
    if (x > 0.0) Math.sqrt(x)
    else throw new Exception("error")
  }

  // negative numbers have a defined return value of `None`
  def sqrt_pure(x: Double) : Option[Double] = {
    if (x > 0.0) Some(Math.sqrt(x))
    else None
  }

  // ----------------------------------------------------------------

  // Print has a visible side effect. (Is printing a visible side effect? it depends...)
  def Print(x: String) : Unit  = println(x)

  // ---------------------------------

  // return value is not determined by input values to the function read.
  def Read: String = readLine()

  //===================================================

  // sqrt_pure is referential transparent. (We can replace any call by the result)

  // Implement Print_pure/Read_Pure
  def Print_pure(x: String) : Unit = ???
  def Read_pure: String = ???

  //-------------------------------------------

  trait IO[A] { self =>
    def run: A

    def map[B](f: A => B): IO[B] = new IO[B] {def run: B = f(self.run)}
    def flatMap[B](f: A => IO[B]) : IO[B] = new IO[B] {def run: B = f(self.run).run }
  }

  def PrintIO(msg: String) = new IO[Unit] {
    def run: Unit = { println(msg) }
  }

  // no side effect. Nothing is printed
  val printIO = PrintIO("test")
  // side effect happens here:
  printIO.run

  def ReadIO: IO[String] = new IO[String] {
    def run: String = { readLine()}
  }

  // no side effect
  val readIO = ReadIO
  // side effect happens here
  readIO.run

  // we need a way chain calls

  // no side effect
  val io = PrintIO("What is your name?").flatMap(_ => ReadIO).flatMap(name => PrintIO(s"hello $name"))

  // side effect happens here:
  io.run


  // syntactic sugar: for comprehension
  //
  // This works because IO is monad!

  val io2 = for {
    _ <- PrintIO("What is your name?")
    name <- ReadIO
    _ <- PrintIO(s"hello $name")
  } yield ()

  io2.run


  // Problem:
  //   If we chain many functions, the call stack is too small => StackOverflow

  def Greeter() =for {
    _ <- PrintIO("hello")
  } yield ()

  def repeat[A](io: IO[A]):IO[A] = {
    io.flatMap(_ => repeat(io))
  }

  //repeat(Greeter()).run // Stack Overflow!!!!!


  // Solution: Trampolining

  trait IO2[A] { self =>
    def flatMap[B](f: A => IO2[B]): IO2[B] = FlatMap(self, f)
    def map[B](f: A => B) : IO2[B] = flatMap(a => Return(f(a)))
  }

  case class FlatMap[A,B](sub: IO2[A], k: A => IO2[B]) extends IO2[B]
  case class Suspend[A](f: () => A) extends IO2[A]
  case class Return[A,B](a: A) extends IO2[A]

  object IO2 {
    @tailrec
    def run[A](io: IO2[A]): A = {
      io match {
        case Return(a) => a
        case Suspend(f) => f()
        case FlatMap(sub, k) => sub match {
          case Return(a) => run(k(a))
          case Suspend(f) => run(k(f()))
          case FlatMap(sub2, k2) => run( sub2.flatMap( a=> k2(a).flatMap(k) ))
        }
      }
    }
  }

  // sub: IO2[A], k: A => IO2[B], run: IO2[A] => A
  //
  // If sub: Return(a)
  // run(k(a))
  // If sub: Suspend(f)
  // run(k(f()))
  // If sub: FlatMap(sub2, k2)
  // run( sub2.flatMap( a => k2(a).flatMap(k))

  def repeat2[A](io: IO2[A]):IO2[A] = {
    io.flatMap(_ => repeat2(io))
  }

  def PrintIO2(msg: String) = Suspend[Unit](() => println("hello"))

  def Greeter2 = for {
    _ <- PrintIO2("hello")
  } yield ()

  IO2.run(repeat2(Greeter2)) // no StackOverflow, the call stack is on the heap!
}
