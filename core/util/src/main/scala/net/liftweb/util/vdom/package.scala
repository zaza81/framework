package net.liftweb.util

import net.liftweb.json._

import scala.xml.{Text, UnprefixedAttribute, Node}

package object vdom {
  val pcdata = "#PCDATA"
  case class VNode(tag:String, attributes:Map[String, String] = Map(), children:List[VNode] = List(), text:Option[String] = None)

  trait VNodeTransform
  case class VNodeInsert(position:Int, node:VNode) extends VNodeTransform

  case class VNodeTransformTree(transforms:List[VNodeTransform], children:List[VNodeTransformTree])

  val typeHints = ShortTypeHints(List(classOf[VNodeInsert]))
  val formats = new Formats {
    override val dateFormat: DateFormat = DefaultFormats.lossless.dateFormat
    override val typeHints = vdom.typeHints
    override val typeHintFieldName = "type"
  }

  private def isText(n:Node) = n.label == pcdata
  private def isntWhitespace(n:Node) = !isText(n) || !n.text.trim.isEmpty

  object VNode {
    def text(t:String):VNode = VNode(pcdata, Map(), List(), Some(t))
    def fromXml(n:Node):VNode = {
      if(n.label == pcdata) text(n.text)
      else {
        val attrs:Map[String, String] = n.attributes
          .collect { case UnprefixedAttribute(k, Text(v), _) => k -> v }
          .toMap
        val children:List[VNode] = n.nonEmptyChildren
          .filter(isntWhitespace)
          .map(fromXml)
          .toList

        VNode(n.label, attrs, children)
      }
    }
  }
  object VDom {
    def diff(a:Node, b:Node):VNodeTransformTree = {
      val aChildren = a.nonEmptyChildren.filter(isntWhitespace)
      val bChildren = b.nonEmptyChildren.filter(isntWhitespace)

      val additions = bChildren.zipWithIndex.drop(aChildren.length)
        .map { case (n, i) => VNodeInsert(i, VNode.fromXml(n)) }
        .toList
      val children = aChildren.zip(bChildren)
        .collect { case (ca, cb) if ca != cb => diff(ca, cb) }  // This != check probably would benefit from memoizing
        .toList

      VNodeTransformTree(additions, children)
    }
  }

  object VDomHelpers extends VDomHelpers
  trait VDomHelpers {
    def node(child:VNodeTransformTree*):VNodeTransformTree = VNodeTransformTree(List(), child.toList)
    def text(t:String) = VNode(pcdata, Map(), List(), Some(t))

    implicit class EnhancedVNodeTransformTree(t:VNodeTransformTree) {
      def withTransforms(transform:VNodeTransform*) = t.copy(transforms = transform.toList)
    }
  }

}
