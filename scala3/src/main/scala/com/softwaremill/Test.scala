package com.softwaremill

case class A()
case class B()
case class C(a: A)

import com.softwaremill.macwire._

trait ParentModule {
  lazy val a = A()
  lazy val b = B()
}

@Module
trait AggregatedModule {
  // lazy val a = A()
}

trait TestModule(aggregatedModule: AggregatedModule) extends ParentModule {
  lazy val c = wire[C]
}

object Test extends App {
  val m = new TestModule(new AggregatedModule {}) {}

  println(m.c)
}
