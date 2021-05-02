package com.softwaremill.macwire

import scala.quoted.*

case class Module() extends scala.annotation.StaticAnnotation

inline def wire[T]: T = ${wireImpl[T]}

def wireImpl[T: Type](using Quotes): Expr[T] = {
  import quotes.reflect.*

  val wiredType = TypeRepr.of[T].typeSymbol
  val wiredDef = Symbol.spliceOwner.owner
  val wiredOwner = wiredDef.owner

  val paramss = wiredType.primaryConstructor.paramSymss.flatten.map { param =>
    println(s"Looking for: $param")
    val paramType = Ref(param).tpe.widen

    val ownFields =
      wiredOwner.memberFields
    val annotatedModulesFields =
      wiredOwner.memberFields.map(_.tree)
        .collect { case v: ValDef => v.tpt.tpe.typeSymbol }
        .filter(_.annotations.exists(_.tpe.typeSymbol.name.contains("Module")))
        .flatMap(_.memberFields)
    
    (ownFields ++ annotatedModulesFields).filter { field =>
      if (wiredDef == field) {
        println(s"  Not checking: $field")
        false
      } else {
        println(s"  Checking: $field")
        val fieldType = Ref(field).tpe.widen
        fieldType <:< paramType
      }
    } match {
      case Nil => report.throwError(s"Cannot find value for type: $paramType")
      case l@(first :: second :: _) => report.throwError(s"For type: $paramType, found multiple values: $l")
      case List(field) => Ref(field)
    }
  }

  val result = Apply(Select(New(TypeIdent(wiredType)), wiredType.primaryConstructor), paramss)
  println(result.show(using Printer.TreeAnsiCode))
  result.asExprOf[T]
}