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
 * A type class that creates CssSel instances for a value of some type
 * @param apply a function stringSelector => css => x => cssSel. stringSelector and css are the arguments for CssBindImpl's constructor
 */
class CanBind[-T](val apply: Box[String] => Box[CssSelector] => T => CssSel)

object CanBind {
  /**
   * Inserts a String constant according to the CssSelector rules
   */
  implicit val string = new CanBind[String](stringSelector => css => str =>
    new CssBindImpl(stringSelector, css) {
      def calculate(in: NodeSeq): Seq[NodeSeq] =
        List(if (null eq str) NodeSeq.Empty else Text(str))
    }
  )

  /**
   * Inserts a NodeSeq constant according to the CssSelector rules
   */
  implicit val nodeSeq = new CanBind[NodeSeq](stringSelector => css => ns =>
    new CssBindImpl(stringSelector, css) {
      def calculate(in: NodeSeq): Seq[NodeSeq] = List(ns)
    }
  )

  /**
   * A function that transforms the content according to the CssSelector rules
   */
  implicit val nodeSeqFunc = new CanBind[NodeSeq=>NodeSeq](stringSelector => css => nsFunc =>
    new CssBindImpl(stringSelector, css) {
      def calculate(in: NodeSeq): Seq[NodeSeq] = List(nsFunc(in))
    }
  )

  /**
   * Inserts a Bindable constant according to the CssSelector rules.
   * Mapper and Record fields implement Bindable.
   */
  implicit val bindable = new CanBind[Bindable](stringSelector => css => bindable =>
    new CssBindImpl(stringSelector, css) {
      def calculate(in: NodeSeq): Seq[NodeSeq] = List(bindable.asHtml)
    }
  )

  /**
   * Inserts a StringPromotable constant according to the CssSelector rules.
   * StringPromotable includes Int, Long, Boolean, and Symbol
   */
  implicit def stringPromotable[T<%StringPromotable] = new CanBind[T](stringSelector => css => strPromo =>
    new CssBindImpl(stringSelector, css) {
      def calculate(in: NodeSeq): Seq[NodeSeq] =
        List(Text(strPromo.toString))
    }
  )

  /**
   * Applies the N constants according to the CssSelector rules.
   * This allows for Seq[String], Seq[NodeSeq], Box[String],
   * Box[NodeSeq], Option[String], Option[NodeSeq]
   */
  implicit def iterableConst[T<%IterableConst] = new CanBind[T](stringSelector => css => itrConst =>
    new CssBindImpl(stringSelector, css) {
      def calculate(in: NodeSeq): Seq[NodeSeq] = itrConst.constList(in)
    }
  )

  /**
   * Apply the function and then apply the results account the the CssSelector
   * rules.
   * This allows for NodeSeq => Seq[String], NodeSeq =>Seq[NodeSeq],
   * NodeSeq => Box[String],
   * NodeSeq => Box[NodeSeq], NodeSeq => Option[String],
   * NodeSeq =>Option[NodeSeq]
   */
  implicit def iterableFunc[T<%IterableFunc] = new CanBind[T](stringSelector => css => itrFunc =>
    new CssBindImpl(stringSelector, css) {
      def calculate(in: NodeSeq): Seq[NodeSeq] = itrFunc(in)
    }
  )
}


/**
 * An intermediate class used to promote a String or a CssSelector to
 * something that can be associated with a value to apply to the selector
 * @param stringSelector the unparsed css selector string
 * @param css the parsed CssSelector object
 */
final class ToCssBindPromoter(stringSelector: Box[String], css: Box[CssSelector]) {
  def #>[T](v: T)(implicit canBind: CanBind[T]): CssSel = canBind.apply(stringSelector)(css)(v)
  def replaceWith[T: CanBind](v: T): CssSel = #>[T](v)
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
 * This trait is both a NodeSeq => NodeSeq and has the ability
 * to chain CssSel instances so that they can be applied
 * en masse to incoming NodeSeq and do the transformation.
 */
sealed trait CssSel extends Function1[NodeSeq, NodeSeq] {
  def &(other: CssSel): CssSel = (this, other) match {
    case (AggregatedCssBindFunc(a), AggregatedCssBindFunc(b)) =>
      AggregatedCssBindFunc(a ::: b)
    case (AggregatedCssBindFunc(a), o: CssBind) =>
      AggregatedCssBindFunc(a ::: List(o))
    case (t: CssBind, AggregatedCssBindFunc(a)) =>
      AggregatedCssBindFunc(t :: a)
    case (t: CssBind, o: CssBind) => AggregatedCssBindFunc(List(t, o))
  }

  /**
   * A Java callable aggregator
   */
  def and(that: CssSel): CssSel = this & that

  /**
   * promote a String to a ToCssBindPromotor
   */
  private implicit def strToCssBindPromoter(str: String): ToCssBindPromoter =
    new ToCssBindPromoter(Full(str), CssSelectorParser.parse(str))


  /**
   * Inserts a String constant according to the CssSelector rules
   */
  def sel(selector: String, str: String): CssSel = this & (selector #> str)

  /**
   * Inserts a String constant according to the CssSelector rules
   */
  def sel(selector: String, str: NodeSeq): CssSel = this & (selector #> str)


  /**
   * Inserts a String constant according to the CssSelector rules
   */
  def sel(selector: String, str: NodeSeq => NodeSeq): CssSel = this & (selector #> str)

  /**
   * Inserts a String constant according to the CssSelector rules
   */
  def sel(selector: String, str: Bindable): CssSel = this & (selector #> str)

  /**
   * Inserts a String constant according to the CssSelector rules
   */
  def sel(selector: String, str: StringPromotable): CssSel = this & (selector #> str)

  /**
   * Inserts a String constant according to the CssSelector rules
   */
  def sel(selector: String, str: IterableConst): CssSel = this & (selector #> str)

  /**
   * Inserts a String constant according to the CssSelector rules
   */
  def sel(selector: String, str: IterableFunc): CssSel = this & (selector #> str)
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



private final case class AggregatedCssBindFunc(binds: List[CssBind]) extends CssSel {
  private lazy val (good, bad) = binds.partition{_.css.isDefined}
  private lazy val selectorMap = new SelectorMap(good)

  def apply(in: NodeSeq): NodeSeq = bad match {
    case Nil => selectorMap(in)
    case bv => bad.flatMap(_(in)) ++ selectorMap(in)
  }
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



private class SelectorMap(binds: List[CssBind]) extends Function1[NodeSeq, NodeSeq] {

  // The KidsSubNode always has to go last or else we
  // get into an issue where we're trying to apply the whole
  // transform to the whole shooting match
  private def sortBinds(lst: List[CssBind]): List[CssBind] =  {
    lst.sortWith {
      case (SubNode(me: EmptyBox), SubNode(_)) => true
      case (SubNode(_), SubNode(them: EmptyBox)) => false
      case (SubNode(Full(KidsSubNode())), SubNode(_)) => false
      case (SubNode(Full(PrependKidsSubNode())), SubNode(_)) => false
      case (SubNode(Full(AppendKidsSubNode())), SubNode(_)) => false
      case (SubNode(_), SubNode(Full(KidsSubNode()))) => true
      case (SubNode(_), SubNode(Full(PrependKidsSubNode()))) => true
      case (SubNode(_), SubNode(Full(AppendKidsSubNode()))) => true
      case _ => true
    }
  }

  private val (idMap, nameMap, clzMap, attrMap, elemMap,
               starFunc, selectThis: Box[CssBind])  = {
    var idMap: Map[String, List[CssBind]] = Map()
    var nameMap: Map[String, List[CssBind]] = Map()
    var clzMap: Map[String, List[CssBind]] = Map()
    var attrMap: Map[String, Map[String, List[CssBind]]] = Map()
    var elemMap: Map[String, List[CssBind]] = Map()
    var starFunc: Box[List[CssBind]] = Empty

    val selThis: Box[CssBind] = binds.flatMap {
      b =>
        b.css.open_!.subNodes match {
          case Full(SelectThisNode(_)) => List(b)
          case _ => Nil
        }
    }.headOption

    binds.foreach {
      case i @ CssBind(IdSelector(id, _)) =>
        idMap += (id -> sortBinds(i :: idMap.getOrElse(id, Nil)))

      case i @ CssBind(ElemSelector(id, _)) =>
        elemMap += (id -> sortBinds(i :: elemMap.getOrElse(id, Nil)))


      case i @ CssBind(StarSelector(_)) => starFunc = Full(sortBinds(i :: starFunc.openOr(Nil)))

      case i @ CssBind(NameSelector(name, _)) =>
        nameMap += (name -> sortBinds(i :: nameMap.getOrElse(name, Nil)))

      case i @ CssBind(ClassSelector(clz, _)) =>
        clzMap += (clz -> sortBinds(i :: clzMap.getOrElse(clz, Nil)))

      case i @ CssBind(AttrSelector(name, value, _)) => {
        val oldMap = attrMap.getOrElse(name, Map())
        attrMap += (name -> (oldMap + (value -> sortBinds(i :: oldMap.getOrElse(value, Nil)))))
      }
    }

    (idMap, nameMap, clzMap, attrMap, elemMap, starFunc, selThis)
  }

  private def findElemIfThereIsOne(in: NodeSeq): NodeSeq = in match {
    case e: Elem => e
    case ns if ns.length == 1 && ns(0).isInstanceOf[Elem] => ns(0)
    case ns => ns
  }

  private abstract class SlurpedAttrs(val id: Box[String],val name: Box[String]) {
    def attrs: Map[String, String]
    def classes: List[String]

    def removeId(in: MetaData) = in.filter {
      case up: UnprefixedAttribute => up.key != "id"
      case _ => true
    }

    private final def isSelThis(bind: CssBind): Boolean =
      bind.css.open_!.subNodes match {
        case Full(SelectThisNode(_)) => true
        case _ => false
      }

    final def applyRule(bindList: List[CssBind], realE: Elem, onlySelThis: Boolean): NodeSeq =
      bindList match {
        case Nil => realE

        // ignore selectThis commands outside the
        // select context
        case bind :: xs
        if onlySelThis && isSelThis(bind) => applyRule(xs, realE, onlySelThis)

        case bind :: xs => {
          applyRule(bind, realE) flatMap {
            case e: Elem => applyRule(xs, e, onlySelThis)
            case x => x
          }
        }
      }

    final def applyAttributeRules(bindList: List[CssBind], elem: Elem): Elem = {
      bindList.map(b => (b, b.css.open_!.subNodes.open_!)).
      foldLeft(elem){
        case (elem, (bind, AttrSubNode(attr))) => {
          val calced = bind.calculate(elem).map(findElemIfThereIsOne _)
          val filtered = elem.attributes.filter{
            case up: UnprefixedAttribute => up.key != attr
            case _ => true
          }

          val newAttr = if (calced.isEmpty) {
            filtered
          } else {
            val flat: NodeSeq = calced.flatMap(a => a)
            new UnprefixedAttribute(attr, flat, filtered)
          }

          new Elem(elem.prefix,
                   elem.label, newAttr,
                   elem.scope, elem.child :_*)
        }

        case (elem, (bind, AttrAppendSubNode(attr))) => {
          val org: NodeSeq = elem.attribute(attr).getOrElse(NodeSeq.Empty)
          val calced = bind.calculate(elem).toList.map(findElemIfThereIsOne _)


          if (calced.isEmpty) {
            elem
          } else {
            val filtered = elem.attributes.filter{
              case up: UnprefixedAttribute => up.key != attr
              case _ => true
            }

            val flat: NodeSeq = if (attr == "class") {
              if (org.isEmpty) {
                calced.dropRight(1).flatMap(a => a ++ Text(" ")) ++
                calced.takeRight(1).head
              } else {
                org ++ Text(" ") ++
                calced.dropRight(1).flatMap(a => a ++ Text(" ")) ++
                calced.takeRight(1).head
              }
            } else {
              org ++ (calced.flatMap(a => a): NodeSeq)
            }

            val newAttr = new UnprefixedAttribute(attr, flat, filtered)

            new Elem(elem.prefix,
                     elem.label, newAttr,
                     elem.scope, elem.child :_*)

          }
        }

        case (elem, (bind, AttrRemoveSubNode(attr))) => {
          val org: NodeSeq = elem.attribute(attr).getOrElse(NodeSeq.Empty)
          val calced = bind.calculate(elem).toList.map(findElemIfThereIsOne _)

          if (calced.isEmpty || org.isEmpty) { // if either is empty, then return the Elem unmodified
            elem
          } else {
            val filtered = elem.attributes.filter{
              case up: UnprefixedAttribute => up.key != attr
              case _ => true
            }

            val flat: Box[NodeSeq] = if (attr == "class") {
                val set = Set(calced.map(_.text) :_*)
                SuperString(org.text).charSplit(' ').toList.
                  filter(_.length > 0).filter(s => !set.contains(s)) match {
                  case Nil => Empty
                  case xs => Full(Text(xs.mkString(" ")))
                }
            } else {
              if (org.text == calced.flatMap(a => a).text) Empty else Full(org)
            }

            val newAttr = flat match {
              case Full(a) => new UnprefixedAttribute(attr, a, filtered)
              case _ => filtered
            }

            new Elem(elem.prefix,
                     elem.label, newAttr,
                     elem.scope, elem.child :_*)

          }
        }
      }
    }


    // This is where the rules are applied
    final def applyRule(bind: CssBind, realE: Elem): NodeSeq = {
      def uniqueClasses(cv: String*): String = {
        import Helpers._

        val ls: List[String] = cv.toList.flatMap(_.charSplit(' '))
        import scala.collection.mutable._
        val hs: HashSet[String] = new HashSet()
        val ret: ListBuffer[String] = new ListBuffer()
        ls.foreach {
          v =>
            if (!hs.contains(v)) {
              hs += v
              ret += v
            }
        }
        ret.mkString(" ")
      }

      def mergeAll(other: MetaData, stripId: Boolean): MetaData = {
        var oldAttrs = attrs - (if (stripId) "id" else "")

        var builtMeta: MetaData = Null
        var pos = other

        while (pos != Null) {
          pos match {
            case up: UnprefixedAttribute if stripId && up.key == "id" =>
              // ignore the id attribute

            case up: UnprefixedAttribute if up.key == "class" => {
              oldAttrs.get("class") match {
                case Some(ca) => {
                  oldAttrs -= "class"
                  builtMeta = new UnprefixedAttribute("class",
                                                      uniqueClasses(up.value.
                                                                    text,
                                                                    ca),
                                                      builtMeta)
                }

                case _ => builtMeta = up.copy(builtMeta)
              }
            }

            case up: UnprefixedAttribute => {
              oldAttrs -= up.key
              builtMeta = up.copy(builtMeta)
            }

            case pa: PrefixedAttribute => {
              oldAttrs -= (pa.pre+":"+pa.key)
              builtMeta = pa.copy(builtMeta)
            }
            case _ =>
          }

          pos = pos.next
        }

        for {
          (k, v) <- oldAttrs
        } {
          import Helpers._
          k.charSplit(':') match {
            case p :: k :: _ =>
              builtMeta = new PrefixedAttribute(p, k, v, builtMeta)
            case k :: _ => builtMeta = new UnprefixedAttribute(k, v, builtMeta)
            case _ =>
          }
        }

        builtMeta
      }

      // we can do an open_! here because all the CssBind elems
      // have been vetted
      bind.css.open_!.subNodes match {
        case Full(SelectThisNode(kids)) => {
          throw new RetryWithException(if (kids) realE.child else realE)
        }

        case Full(todo: WithKids) => {
          val calced = bind.calculate(realE.child)
          calced.length match {
            case 0 => new Elem(realE.prefix, realE.label, realE.attributes, realE.scope)
            case 1 => new Elem(realE.prefix, realE.label,
                               realE.attributes, realE.scope,
                               todo.transform(realE.child, calced.head) :_*)
            case _ if id.isEmpty =>
              calced.map(kids => new Elem(realE.prefix, realE.label,
                                          realE.attributes, realE.scope,
                                          todo.transform(realE.child, kids) :_*))

            case _ => {
              val noId = removeId(realE.attributes)
              calced.toList.zipWithIndex.map {
                case (kids, 0) =>
                  new Elem(realE.prefix, realE.label,
                           realE.attributes, realE.scope,
                           todo.transform(realE.child, kids) :_*)
                case (kids, _) =>
                  new Elem(realE.prefix, realE.label,
                           noId, realE.scope,
                           todo.transform(realE.child, kids) :_*)
              }
            }
          }
        }

        case x: EmptyBox => {
          val calced = bind.calculate(realE).map(findElemIfThereIsOne _)

          calced.length match {
            case 0 => NodeSeq.Empty
            case 1 => {
              calced.head match {
                case Group(g) => g
                case e: Elem => new Elem(e.prefix,
                                         e.label, mergeAll(e.attributes, false),
                                         e.scope, e.child :_*)
                case x => x
              }
            }

            case n => {
              calced.toList.zipWithIndex.flatMap {
                case (Group(g), _) => g
                case (e: Elem, 0) =>
                  new Elem(e.prefix,
                           e.label, mergeAll(e.attributes, false),
                           e.scope, e.child :_*)
                case (e: Elem, _) =>
                  new Elem(e.prefix,
                           e.label, mergeAll(e.attributes, true),
                           e.scope, e.child :_*)
                case (x, _) =>
                  x
              }
            }
          }
        }
      }
    }


    final def forId(in: Elem, buff: ListBuffer[CssBind]) {
      for {
        rid <- id
        bind <- idMap.get(rid)
      } buff ++= bind
    }

    final def forElem(in: Elem, buff: ListBuffer[CssBind]) {
      for {
        bind <- elemMap.get(in.label)
      } buff ++= bind
    }

    final def forStar(buff: ListBuffer[CssBind]) {
      for {
        bind <- starFunc
      } buff ++= bind
    }

    final def forName(in: Elem, buff: ListBuffer[CssBind]) {
      for {
        rid <- name
        bind <- nameMap.get(rid)
      } buff ++= bind
    }

    def findClass(clz: List[String], buff: ListBuffer[CssBind]) {
      clz match {
        case Nil => ()
          case x :: xs => {
            clzMap.get(x) match {
              case Some(cb) => buff ++= cb
              case _ =>
            }
            findClass(xs, buff)
          }
      }
    }

    def forClass(in: Elem, buff: ListBuffer[CssBind]) {
      findClass(classes, buff)
    }

    def forAttr(in: Elem, buff: ListBuffer[CssBind]) {
      if (attrMap.isEmpty || attrs.isEmpty) ()
      else {
        for {
          (key, map) <- attrMap
          v <- attrs.get(key)
          cb <- map.get(v)
        } buff ++= cb
      }
    }
  }

  private def slurpAttrs(in: MetaData): SlurpedAttrs = {
    var id: Box[String] = Empty
    var cur: MetaData = in
    var name: Box[String] = Empty
    var clzs: List[String] = Nil
    var theAttrs: Map[String, String] = Map()

    while (cur != Null) {
      cur match {
        case up: UnprefixedAttribute if (null ne up.value) => {
          val key = up.key
          val value = up.value.text
          import Helpers._
          key match {
            case "id" => id = Full(value)
            case "name" => name = Full(value)
            case "class" => clzs = value.charSplit(' ')
            case _ =>
          }

          theAttrs += key -> value
        }

        case pa: PrefixedAttribute if (null ne pa.value) => {
          theAttrs += ((pa.pre+":"+pa.key) -> pa.value.text)
        }

        case _ =>
      }

      cur = cur.next
    }

    new SlurpedAttrs(id, name) {
      def attrs: Map[String, String] = theAttrs
      def classes: List[String] = clzs
    }
  }

  final private def treatElem(e: Elem, onlySel: Boolean): NodeSeq = {
    val slurp = slurpAttrs(e.attributes)
    val lb = new ListBuffer[CssBind]

    slurp.forId(e, lb)
    slurp.forName(e, lb)
    slurp.forClass(e, lb)
    slurp.forElem(e, lb)
    slurp.forAttr(e, lb)
    slurp.forStar(lb)

    if (onlySel) {
      lb.toList.filter(_.selectThis_?) match {
        case Nil => {
          run(e.child, onlySel)
          NodeSeq.Empty
        }

        case csb :: _ =>
          throw new RetryWithException(if (csb.selectThisChildren_?)
            e.child else e)
      }
    } else {
      lb.toList.filterNot(_.selectThis_?)  match {
        case Nil => new Elem(e.prefix, e.label,
                             e.attributes, e.scope, run(e.child, onlySel) :_*)
        case csb =>
          // do attributes first, then the body
          csb.partition(_.attrSel_?) match {
            case (Nil, rules) =>  slurp.applyRule(rules, e, onlySel)
            case (attrs, Nil) => {
              val elem = slurp.applyAttributeRules(attrs, e)
              new Elem(elem.prefix, elem.label,
                       elem.attributes, elem.scope, run(elem.child, onlySel) :_*)
            }

            case (attrs, rules) => {
              slurp.applyRule(rules,
                              slurp.applyAttributeRules(attrs, e),
                              onlySel)
            }
          }
          // slurp.applyRule(csb, e, onlySel)
      }
    }
  }

  final def apply(in: NodeSeq): NodeSeq = selectThis match {
    case Full(_) => {
      try {
        run(in, true)
      } catch {
        case RetryWithException(newElem) =>
          run(newElem, false)
      }
    }

    case _ => run(in, false)
  }

  final private def run(in: NodeSeq, onlyRunSel: Boolean): NodeSeq =
      in flatMap {
        case Group(g) => run(g, onlyRunSel)
        case e: Elem => treatElem(e, onlyRunSel)
        case x => x
      }
}


private case class RetryWithException(e: NodeSeq) extends Exception()

object CssBind {
  def unapply(in: CssBind): Option[CssSelector] = in.css
}

sealed trait CssBind extends CssSel {
  def stringSelector: Box[String]
  def css: Box[CssSelector]

  override def toString(): String = "CssBind("+stringSelector+", "+css+")"

  def apply(in: NodeSeq): NodeSeq = css match {
    case Full(c) => selectorMap(in)
    case _ => Helpers.errorDiv(
      <div>
      Syntax error in CSS selector definition: {stringSelector openOr "N/A"}.
      The selector will not be applied.
      </div>) openOr NodeSeq.Empty
  }

  /**
   * Is this CssBind a SelectThis bind?
   */
  private[util] def selectThis_? : Boolean = css match {
    case Full(sel) => {
      sel.subNodes match {
        case Full(SelectThisNode(_)) => true
        case _ => false
      }
    }

    case _ => false
  }

  /**
   * Is this an Attribute mutating node?
   */
  private[util] def attrSel_? : Boolean = css match {
    case Full(sel) => {
      sel.subNodes match {
        case Full(x: AttributeRule) => true
        case _ => false
      }
    }

    case _ => false
  }

  private[util] def selectThisChildren_? : Boolean = css match {
    case Full(sel) => {
      sel.subNodes match {
        case Full(SelectThisNode(children)) => children
        case _ => false
      }
    }

    case _ => false
  }

  private lazy val selectorMap: SelectorMap = new SelectorMap(List(this))

  def calculate(in: NodeSeq): Seq[NodeSeq]
}

/**
 * An abstract implementation of CssBind.  You can instantiate
 * this class and create a custom calculate method
 */
abstract class CssBindImpl(val stringSelector: Box[String], val css: Box[CssSelector]) extends CssBind {
  def calculate(in: NodeSeq): Seq[NodeSeq]
}

/**
 * Bridge from Java-land to Scala
 */
final class CssJBridge {
  /**
   * promote a String to a ToCssBindPromotor
   */
  private implicit def strToCssBindPromoter(str: String): ToCssBindPromoter =
    new ToCssBindPromoter(Full(str), CssSelectorParser.parse(str))

  def sel(selector: String, value: String): CssSel = selector #> value
  def sel(selector: String, value: NodeSeq): CssSel = selector #> value
  def sel(selector: String, value: NodeSeq => NodeSeq): CssSel = selector #> value
  def sel(selector: String, value: Bindable): CssSel = selector #> value

  /**
   * Inserts a String constant according to the CssSelector rules
   */
  def sel(selector: String, str: StringPromotable): CssSel = (selector #> str)

  /**
   * Inserts a String constant according to the CssSelector rules
   */
  def sel(selector: String, str: IterableConst): CssSel = (selector #> str)

  /**
   * Inserts a String constant according to the CssSelector rules
   */
  def sel(selector: String, str: IterableFunc): CssSel = (selector #> str)

}
