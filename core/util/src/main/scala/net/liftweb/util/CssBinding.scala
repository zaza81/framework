/*
 * Copyright 2007-2011 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.liftweb
package util


import scala.xml._
import common._
import java.util.{List => JavaList}
import scala.collection.mutable.ListBuffer



/**
 * A type class that creates a NodeSeq=>Seq[NodeSeq] for a value of some type.
 * The apply method delegates to the function passed to the constructor.
 * @param f a function T=>NodeSeq=>Seq[NodeSeq] that returns a NodeSeq=>NodeSeq for a T
 */
class CanBindN[-T](f: T => NodeSeq => Seq[NodeSeq]) extends (T => NodeSeq => Seq[NodeSeq]) {
  /**
   * Given a value of type T, return a function that can be used in Lift binding.
   * The function takes a NodeSeq (the content from the template) and returns
   * a Seq[NodeSeq]. The return value is collection-valued because some types repeatedly
   * transform the same template content (e.g., "th *" #> List("Column Header 1", "Column Header 2"))
   */
  def apply(v: T) = f(v)
}

/**
 * Defines the CanBindN implicits that are available by default
 */
object CanBindN {
  /**
   * Bind a single bindable value.
   * Given a type that has an implicit CanBind, delegate to that CanBind,
   * wrapping the result in a List is its single element
   */
  implicit def single[T](implicit canBind: CanBind[T]): CanBindN[T] =
    new CanBindN[T](v => ns => List(canBind(v)(ns)))

  /**
   * Bind zero or more bindable values.
   * Given a type that has an implicit CanBind, and a context viewable as an
   * Iterable of that type, apply all the CanBinds
   */
  implicit def iterable[T, I[_]](implicit canBind: CanBind[T], toIter: I[T]=>Iterable[T]): CanBindN[I[T]] =
    new CanBindN[I[T]](v => ns => Helpers.ensureUniqueId(toIter(v).toSeq map (_ apply ns)))

  /**
   * Bind a function that, given a NodeSeq, returns zero or more bindable values.
   * Given a type that has an implicit CanBindN, and a context viewable as an
   * Iterable of that type, apply all the CanBindNs.
   */
  implicit def iterableFunc[T, I[_]](implicit canBindN: CanBindN[T], toIter: I[T]=>Iterable[T]): CanBindN[NodeSeq=>I[T]] =
    new CanBindN[NodeSeq=>I[T]](v => ns => toIter(v(ns)).toSeq flatMap (_ apply ns))
}

class CanBind[-T](f: T => NodeSeq => NodeSeq) extends (T => NodeSeq => NodeSeq) {
  def apply(v: T) = f(v)
}

//TODO obviate StringPromotable
//TODO obviate Bindable

object CanBind {
  /**
   * Bind a string by replacing content with a Text node (or NodeSeq.empty in the case of null)
   */
  implicit val string = new CanBind[String](str => _ => (if (null eq str) NodeSeq.Empty else Text(str)))

  /**
   * Bind a NodeSeq by replacing content with it
   */
  implicit val nodeSeq: CanBind[NodeSeq] = new CanBind[NodeSeq](ns => _ => ns)

  /**
   * Bind a Seq[Node] by replacing content with it, via NodeSeq.fromSeq
   */
  implicit val seqNode: CanBind[Seq[Node]] = new CanBind[Seq[Node]](ns => _ => NodeSeq fromSeq ns)

  /**
   * Bind a function NodeSeq=>U where U has an implicit CanBind, by applying it
   * to content and replacing it with the result
   */
  implicit def func[U](implicit canBind: CanBind[U]): CanBind[NodeSeq => U] =
    new CanBind[NodeSeq => U](f => in => f(in) apply in)

  /**
   * Bind an object that extends Bindable, by calling its asHtml method
   * and replacing content with its result.
   * Mapper and Record fields implement Bindable.
   */
  implicit val bindable: CanBind[Bindable] = new CanBind[Bindable](bindable => _ => bindable.asHtml)

  /**
   * Bind something that has a conversion to StringPromotable, by
   * calling the StringPromotable's toString method and replacing content with it wrapped in a Text node.
   * StringPromotable includes Int, Long, Boolean, and Symbol
   */
  implicit def stringPromotable[T](implicit view: T=>StringPromotable) = new CanBind[T]({strPromo => _ =>
    view(strPromo).toString match {
      case null => NodeSeq.Empty
      case s => Text(s)
    }
  })


}



/**
 * An intermediate class used to promote a String or a CssSelector to
 * something that can be associated with a value to apply to the selector
 * @param stringSelector the unparsed css selector string
 * @param css the parsed CssSelector object
 */
final class ToCssBindPromoter(stringSelector: Box[String], css: Box[CssSelector]) {
  def #>[T](v: T)(implicit canBindN: CanBindN[T]): CssSel =
    new CssBindImpl(stringSelector, css) {
      def calculate(in: NodeSeq): Seq[NodeSeq] = canBindN.apply(v)(in)
    }
  def replaceWith[T: CanBindN](v: T): CssSel = #>[T](v)
}


/**
 * A trait that has some helpful implicit conversions from
 * Iterable[NodeSeq], Seq[String], Box[String], and Option[String]
 */
trait IterableConst {
  def constList(nodeSeq: NodeSeq): Seq[NodeSeq]
}

import scala.collection.JavaConversions._

/**
 * The implementation for a NodeSeq Iterable Const
 */
final case class NodeSeqIterableConst(it: Iterable[NodeSeq]) extends IterableConst {
  def this(it: JavaList[NodeSeq]) = this(it: Iterable[NodeSeq])

  def constList(nodeSeq: NodeSeq): Seq[NodeSeq] = it.toSeq
}

/**
 * The implementation for a NodeSeq => NodeSeq Iterable Const
 */
final case class NodeSeqFuncIterableConst(it: Iterable[NodeSeq => NodeSeq]) extends IterableConst {
  def this(it: JavaList[NodeSeq => NodeSeq]) = this(it: Iterable[NodeSeq => NodeSeq])

  def constList(nodeSeq: NodeSeq): Seq[NodeSeq] = Helpers.ensureUniqueId(it.map(_(nodeSeq)).toSeq)
}

/**
 * The implementation for a Box[NodeSeq => Node] Iterable Const
 */
final case class BoxNodeSeqFuncIterableConst(it: Box[NodeSeq => NodeSeq]) extends IterableConst {

  def constList(nodeSeq: NodeSeq): Seq[NodeSeq] = it.toList.map(_(nodeSeq))
}

/**
 * The implementation for a Option[NodeSeq => Node] Iterable Const
 */
final case class OptionNodeSeqFuncIterableConst(it: Option[NodeSeq => NodeSeq]) extends IterableConst {

  def constList(nodeSeq: NodeSeq): Seq[NodeSeq] = it.toList.map(_(nodeSeq))
}

/**
 * Sequence of String iterable const
 */
final case class SeqStringIterableConst(it: Iterable[String]) extends IterableConst {
  def this(it: JavaList[String]) = this(it: Iterable[String])

  def constList(nodeSeq: NodeSeq): Seq[NodeSeq] = it.map(a => Text(a)).toSeq
}

/**
 * Sequence of Bindable iterable const
 */
final case class SeqBindableIterableConst(it: Iterable[Bindable]) extends IterableConst {
  def this(it: JavaList[Bindable]) = this(it: Iterable[Bindable])

  def constList(nodeSeq: NodeSeq): Seq[NodeSeq] = it.map(_.asHtml).toSeq
}

/**
 * The companion object that does the helpful promotion of common
 * collection types into an IterableConst,
 * e.g. Iterable[NodeSeq], Seq[String], Box[String], and Option[String]
 */
object IterableConst {
  /**
   * Converts anything that can be converted into an Iterable[NodeSeq]
   * into an IterableConst.  This includes Seq[NodeSeq]
   */
  implicit def itNodeSeq(it: Iterable[NodeSeq]): IterableConst =
    NodeSeqIterableConst(it)

  /**
   * Converts anything that can be converted into an Box[NodeSeq]
   * Bind a value that has a CanBindN implicit available
   */
  implicit def boxNodeSeq(it: Box[NodeSeq]): IterableConst =
    NodeSeqIterableConst(it.toList)

  /**
   * Converts anything that can be converted into an Box[NodeSeq]
   */
  implicit def optionNodeSeq(it: Option[NodeSeq]): IterableConst =
    NodeSeqIterableConst(it.toList)

  /**
   * Converts anything that can be converted into an Iterable[NodeSeq]
   * into an IterableConst.  This includes Seq[NodeSeq], Option[NodeSeq],
   * and Box[NodeSeq]
   * Bind a value that has a CanBindN implicit available
   */
  implicit def itNodeSeq(it: JavaList[NodeSeq]): IterableConst =
    new NodeSeqIterableConst(it)

  implicit def itNodeSeqFunc(it: Iterable[NodeSeq => NodeSeq]): IterableConst =
    NodeSeqFuncIterableConst(it)

  implicit def itNodeSeqFunc(it: JavaList[NodeSeq => NodeSeq]): IterableConst =
    new NodeSeqFuncIterableConst(it)

  implicit def boxNodeSeqFunc(it: Box[NodeSeq => NodeSeq]): IterableConst =
    BoxNodeSeqFuncIterableConst(it)

  implicit def optionNodeSeqFunc(it: Option[NodeSeq => NodeSeq]): IterableConst =
    OptionNodeSeqFuncIterableConst(it)

  implicit def itStringPromotable(it: Iterable[String]): IterableConst =
    SeqStringIterableConst(it)

  implicit def javaListStringPromotable(it: JavaList[String]): IterableConst =
    new SeqStringIterableConst(it)

  implicit def boxString(it: Box[String]): IterableConst =
    SeqStringIterableConst(it.toList)

  implicit def optionString(it: Option[String]): IterableConst =
    SeqStringIterableConst(it.toList)

  implicit def itBindable(it: Iterable[Bindable]): IterableConst =
    SeqBindableIterableConst(it)

  implicit def itBindable(it: JavaList[Bindable]): IterableConst =
    new SeqBindableIterableConst(it)


  implicit def boxBindablePromotable(it: Box[Bindable]): IterableConst =
    SeqBindableIterableConst(it.toList)

  implicit def optionBindablePromotable(it: Option[Bindable]): IterableConst =
    SeqBindableIterableConst(it.toList)

  implicit def optionStringPromotable[T](o: Option[T])(implicit view:T=>StringPromotable) = optionString(o.map(view(_).toString))
}

sealed trait IterableFunc extends Function1[NodeSeq, Seq[NodeSeq]] {
  def apply(ns: NodeSeq): Seq[NodeSeq]
}

object IterableFunc {
  implicit def itNodeSeq[C <% Iterable[NodeSeq]](it: NodeSeq => C): IterableFunc =
    new IterableFunc {
      def apply(in: NodeSeq): Seq[NodeSeq] = it(in).toSeq
    }

  implicit def itNodeSeqPromotable(it: NodeSeq => NodeSeq): IterableFunc =
    new IterableFunc {
      def apply(in: NodeSeq): Seq[NodeSeq] = List(it(in))
    }


  implicit def itStringFuncPromotable(it: NodeSeq => String): IterableFunc =
    new IterableFunc {
      def apply(in: NodeSeq): Seq[NodeSeq] = it(in) match {
        case null => List(NodeSeq.Empty)
        case str => List(Text(str))}
    }


  implicit def itStringPromotable(it: NodeSeq => Seq[String]): IterableFunc =
    new IterableFunc {
      def apply(in: NodeSeq): Seq[NodeSeq] = it(in).filter(_ ne null).map(a => Text(a))
    }

  implicit def boxStringPromotable(it: NodeSeq => Box[String]): IterableFunc =
    new IterableFunc {
      def apply(in: NodeSeq): Seq[NodeSeq] = it(in).filter(_ ne null).toList.map(a => Text(a))
    }


  implicit def optionStringPromotable(it: NodeSeq => Option[String]): IterableFunc =
    new IterableFunc {
      def apply(in: NodeSeq): Seq[NodeSeq] = it(in).filter(_ ne null).toList.map(a => Text(a))
    }
}


/**
 * This trait marks something that can be promoted into a String.
 * The companion object has helpful conversions from Int,
 * Symbol, Long, and Boolean
 */
trait StringPromotable

object StringPromotable {
  implicit def hasStringConversion[T <% String](in: T): StringPromotable =
    new StringPromotable {
      override val toString: String = in
    }
  implicit def jsCmdToStrPromo(in: ToJsCmd): StringPromotable =
    new StringPromotable {
      override val toString = in.toJsCmd
    }

  implicit def jsCmdToStrPromo(in: (_, ToJsCmd)): StringPromotable =
    new StringPromotable {
      override val toString = in._2.toJsCmd
    }


  implicit def intToStrPromo(in: Int): StringPromotable =
    new StringPromotable {
      override val toString = in.toString
    }

  implicit def symbolToStrPromo(in: Symbol): StringPromotable =
    new StringPromotable {
      override val toString = in.name
    }

  implicit def longToStrPromo(in: Long): StringPromotable =
    new StringPromotable {
      override val toString = in.toString
    }

  implicit def booleanToStrPromo(in: Boolean): StringPromotable =
    new StringPromotable {
      override val toString = in.toString
    }
}

/**
 * A passthrough function that does not change the nodes
 *
 * @tag CssFunction
 */
object PassThru extends Function1[NodeSeq, NodeSeq] {
  def apply(in: NodeSeq): NodeSeq = in
}

/**
 * Replaces the nodes with an Empty NodeSeq.  Useful
 * for removing unused nodes
 *
 * @tag CssFunction
 */
object ClearNodes extends Function1[NodeSeq, NodeSeq] {
  def apply(in: NodeSeq): NodeSeq = NodeSeq.Empty
}

/**
 * This CssBind will clear all nodes marked with the class
 * clearable.  Designers can mark extra nodes in markup with
 * class="clearable" and this Bind will make them go away
 */
class ClearClearable extends CssBindImpl(Full(".clearable"), CssSelectorParser.parse(".clearable")) {

  def calculate(in: NodeSeq): Seq[NodeSeq] = Nil
}

/**
 * This CssBind will clear all nodes marked with the class
 * clearable.  Designers can mark extra nodes in markup with
 * class="clearable" and this Bind will make them go away
 */
object ClearClearable extends ClearClearable


