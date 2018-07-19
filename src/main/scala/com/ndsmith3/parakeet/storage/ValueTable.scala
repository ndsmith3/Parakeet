package com.ndsmith3.parakeet.storage

import com.ndsmith3.parakeet.ast._
import com.ndsmith3.parakeet.exception.{UnknownTokenException, ReassignmentException}

case class TableEntry(valueName: String, valueType: String, value: Option[Primitive])

object ValueTable {
  type ValueTable = List[TableEntry]

  def get(table: ValueTable, name: String): Primitive = table.find(_.valueName == name) match {
    case Some(TableEntry(_, _, Some(value))) => value
    case Some(TableEntry(_, _, None))        => throw new Exception("TODO")
    case None                          => throw new UnknownTokenException(name)
  }

  def addType(table: ValueTable, name: String, typeName: String): ValueTable = table.find(_.valueName == name) match {
    case Some(TableEntry(_, actualType, _)) => throw new Exception("TODO")
    case None                               => TableEntry(name, typeName, None) :: table
  }

  def addValue(table: ValueTable, name: String, value: Primitive): ValueTable = table.find(_.valueName == name) match {
    case Some(TableEntry(_, _, Some(_)))       => throw new ReassignmentException(name)
    case Some(TableEntry(_, actualType, None)) => if (value.typeName == actualType) TableEntry(name, value.typeName, Some(value)) :: table else throw new Exception("TODO")
    case None                                  => TableEntry(name, value.typeName, Some(value)) :: table
  }
}
