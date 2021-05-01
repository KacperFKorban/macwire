package com.softwaremill

case class A()
case class B()
case class C(a: A, b: B)
case class D(a: A, c: C)

import com.softwaremill.macwire._

object Test extends App {
  lazy val a = A()
  lazy val b = B()
  lazy val c = wire[C]
  lazy val d = wire[D]

  println(c)
  println(d)
}