package com.softwaremill.macwire

import scala.quoted.*

case class Module() extends scala.annotation.StaticAnnotation

inline def wire[T]: T = ${wireImpl[T]}

def wireImpl[T: Type](using Quotes): Expr[T] = {
  import quotes.reflect.*

  case class FieldEntry(field: Symbol, fieldRef: Term)

  val wiredType = TypeRepr.of[T].typeSymbol
  val wiredDef = Symbol.spliceOwner.owner
  val wiredOwner = wiredDef.owner

  def isMatchingField(paramType: TypeRepr, fieldEntry: FieldEntry): Boolean = {
    if (wiredDef == fieldEntry.field) {
      println(s"  Not checking: ${fieldEntry.field}")
      false
    } else {
      println(s"  Checking: ${fieldEntry.field}")
      val fieldType = Ref(fieldEntry.field).tpe.widen
      fieldType <:< paramType
    }
  }

  val paramss = wiredType.primaryConstructor.paramSymss.flatten.map { param =>
    println(s"Looking for: $param")
    val paramType = Ref(param).tpe.widen

    val ownFields =
      wiredOwner.memberFields.map { field => FieldEntry(field, Ref(field)) }

    val annotatedModulesFields =
      wiredOwner.memberFields.map(_.tree)
        .collect { case v: ValDef => v }
        .filter(_.tpt.tpe.typeSymbol.annotations.exists(_.tpe.typeSymbol.name.contains("Module")))
        .flatMap(f => f.tpt.tpe.typeSymbol.memberFields.map(field => (field, f.symbol)))
        .map { (field, owner) => FieldEntry(field, Select(Ref(owner), field)) }
    
    (ownFields ++ annotatedModulesFields).filter(isMatchingField(paramType, _)) match {
      case Nil => report.throwError(s"Cannot find value for type: $paramType")
      case l@(first :: second :: _) => report.throwError(s"For type: $paramType, found multiple values: $l")
      case List(fieldEntry) =>
        fieldEntry.fieldRef
    }
  }

  val result = Apply(Select(New(TypeIdent(wiredType)), wiredType.primaryConstructor), paramss)
  println(result.show(using Printer.TreeAnsiCode))
  result.asExprOf[T]
}
