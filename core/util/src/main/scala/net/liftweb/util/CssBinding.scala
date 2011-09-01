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
 * An intermediate class used to promote a String or a CssSelector to
 * something that can be associated with a value to apply to the selector
 */
final class ToCssBindPromoter(stringSelector: Box[String], css: Box[CssSelector]) {

  /**
   * Inserts a String constant according to the CssSelector rules
   */
  def #>(str: String): CssSel = new CssBindImpl(stringSelector, css) {
    def calculate(in: NodeSeq): Seq[NodeSeq] =
      List(if (null eq str) NodeSeq.Empty else Text(str))
  }


  /**
   * Inserts a NodeSeq constant according to the CssSelector rules
   */
  def #>(ns: NodeSeq): CssSel = new CssBindImpl(stringSelector, css) {
    def calculate(in: NodeSeq): Seq[NodeSeq] = List(ns)
  }

  /**
   * A function that transforms the content according to the CssSelector rules
   */
  def #>(nsFunc: NodeSeq => NodeSeq): CssSel = new CssBindImpl(stringSelector, css) {
    def calculate(in: NodeSeq): Seq[NodeSeq] = List(nsFunc(in))
  }

  /**
   * Inserts a Bindable constant according to the CssSelector rules.
   * Mapper and Record fields implement Bindable.
   */
  def #>(bindable: Bindable): CssSel = new CssBindImpl(stringSelector, css) {
    def calculate(in: NodeSeq): Seq[NodeSeq] = List(bindable.asHtml)
  }

  /**
   * Inserts a StringPromotable constant according to the CssSelector rules.
   * StringPromotable includes Int, Long, Boolean, and Symbol
   */
  def #>(strPromo: StringPromotable): CssSel = new CssBindImpl(stringSelector, css) {
    def calculate(in: NodeSeq): Seq[NodeSeq] =
      List(Text(strPromo.toString))
  }

  /**
   * Applies the N constants according to the CssSelector rules.
   * This allows for Seq[String], Seq[NodeSeq], Box[String],
   * Box[NodeSeq], Option[String], Option[NodeSeq]
   */
  def #>(itrConst: IterableConst): CssSel = new CssBindImpl(stringSelector, css) {
    def calculate(in: NodeSeq): Seq[NodeSeq] = itrConst.constList(in)
  }

  /**
   * Apply the function and then apply the results account the the CssSelector
   * rules.
   * This allows for NodeSeq => Seq[String], NodeSeq =>Seq[NodeSeq],
   * NodeSeq => Box[String],
   * NodeSeq => Box[NodeSeq], NodeSeq => Option[String],
   * NodeSeq =>Option[NodeSeq]
   */
  def #>(itrFunc: IterableFunc): CssSel = new CssBindImpl(stringSelector, css) {
    def calculate(in: NodeSeq): Seq[NodeSeq] = itrFunc(in)
  }

  /**
   * Inserts a String constant according to the CssSelector rules
   */
  def replaceWith(str: String): CssSel = new CssBindImpl(stringSelector, css) {
    def calculate(in: NodeSeq): Seq[NodeSeq] =
      List(if (null eq str) NodeSeq.Empty else Text(str))
  }

  /**
   * Inserts a NodeSeq constant according to the CssSelector rules
   */
  def replaceWith(ns: NodeSeq): CssSel = new CssBindImpl(stringSelector, css) {
    def calculate(in: NodeSeq): Seq[NodeSeq] = List(ns)
  }

  /**
   * A function that transforms the content according to the CssSelector rules
   */
  def replaceWith(nsFunc: NodeSeq => NodeSeq): CssSel = new CssBindImpl(stringSelector, css) {
    def calculate(in: NodeSeq): Seq[NodeSeq] = List(nsFunc(in))
  }

  /**
   * Inserts a Bindable constant according to the CssSelector rules.
   * Mapper and Record fields implement Bindable.
   */
  def replaceWith(bindable: Bindable): CssSel = new CssBindImpl(stringSelector, css) {
    def calculate(in: NodeSeq): Seq[NodeSeq] = List(bindable.asHtml)
  }

  /**
   * Inserts a StringPromotable constant according to the CssSelector rules.
   * StringPromotable includes Int, Long, Boolean, and Symbol
   */
  def replaceWith(strPromo: StringPromotable): CssSel = new CssBindImpl(stringSelector, css) {
    def calculate(in: NodeSeq): Seq[NodeSeq] = strPromo.toString match {
      case null => NodeSeq.Empty
      case str => List(Text(str))
    }
  }

  /**
   * Applies the N constants according to the CssSelector rules.
   * This allows for Seq[String], Seq[NodeSeq], Box[String],
   * Box[NodeSeq], Option[String], Option[NodeSeq]
   */
  def replaceWith(itrConst: IterableConst): CssSel = new CssBindImpl(stringSelector, css) {
    def calculate(in: NodeSeq): Seq[NodeSeq] = itrConst.constList(in)
  }

  /**
   * Apply the function and then apply the results account the the CssSelector
   * rules.
   * This allows for NodeSeq => Seq[String], NodeSeq =>Seq[NodeSeq],
   * NodeSeq => Box[String],
   * NodeSeq => Box[NodeSeq], NodeSeq => Option[String],
   * NodeSeq =>Option[NodeSeq]
   */
  def replaceWith(itrFunc: IterableFunc): CssSel = new CssBindImpl(stringSelector, css) {
    def calculate(in: NodeSeq): Seq[NodeSeq] = itrFunc(in)
  }
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


