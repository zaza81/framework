package net.liftweb
package util

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

import net.liftweb.common._

object CssSelectorMacros {
  implicit class CssSelectorMacros(context: StringContext) extends AnyRef {
    def c(in: Any*): CssSelector = macro parseSelector
  }

  private def abortWithMessage(context: Context, message: String) = {
    context.abort(context.enclosingPosition, message)
  }

  def parseSelector(context: Context)(in: context.Expr[Any]*) = {
    import context.universe._

    case class TreeificationInfo(tree: context.Tree, modifiedAttribute: Option[String] = None)

    def treeifyFailure(failure: Failure): context.Tree = {
      val Failure(message, exceptionBox, chain) = failure

      // We only keep the exception message, rather than try to treeify
      // everything about it. Failures are unlikely for the SubNode Box
      // anyway.
      val exceptionTree =
        exceptionBox.map { exception =>
          q"Full(new Exception(${exception.toString}))"
        } openOr {
          q"Empty"
        }

      val chainTree =
        chain.map { failure =>
          treeifyFailure(failure)
        } openOr {
          q"Empty"
        }

      q"Failure($message, $exceptionTree, $chainTree)"
    }

    def treeifySubNode(subNode: SubNode): TreeificationInfo = {
      subNode match {
        case KidsSubNode() =>
          TreeificationInfo(q"KidsSubNode()")
        case PrependKidsSubNode() =>
          TreeificationInfo(q"PrependKidsSubNode()")
        case AppendKidsSubNode() =>
          TreeificationInfo(q"AppendKidsSubNode()")
        case SurroundKids() =>
          TreeificationInfo(q"SurroundKids()")

        case DontMergeAttributes =>
          TreeificationInfo(q"DontMergeAttributes")

        case AttrSubNode(attr) =>
          TreeificationInfo(q"AttrSubNode($attr)", Some(attr))
        case AttrAppendSubNode(attr) =>
          TreeificationInfo(q"AttrAppendSubNode($attr)", Some(attr))
        case AttrRemoveSubNode(attr) =>
          TreeificationInfo(q"AttrRemoveSubNode($attr)", Some(attr))

        case SelectThisNode(kids) =>
          TreeificationInfo(q"SelectThisNode($kids)")
      }
    }

    def treeifySubNodes(subNodes: Box[SubNode]): TreeificationInfo = {
      subNodes match {
        case Empty => TreeificationInfo(q"Empty")
        case failure: Failure => TreeificationInfo(treeifyFailure(failure))
        case Full(subNode) =>
          val subNodeTreeInfo = treeifySubNode(subNode)

          subNodeTreeInfo.copy(tree = q"Full(${subNodeTreeInfo.tree})")
      }
    }

    def treeifySelector(selector: CssSelector): TreeificationInfo = {
      import context.universe._

      val subNodeTreeInfo = treeifySubNodes(selector.subNodes)
      val subNodeTree = subNodeTreeInfo.tree

      selector match {
        case EnclosedSelector(parent, child) =>
          val parentTreeInfo = treeifySelector(parent)
          val childTreeInfo = treeifySelector(child)

          TreeificationInfo(
            q"EnclosedSelector(${parentTreeInfo.tree}, ${childTreeInfo.tree})",
            parentTreeInfo.modifiedAttribute orElse childTreeInfo.modifiedAttribute
          )

        case AttrModifyingSelector(attribute, selector) =>
          subNodeTreeInfo.copy(tree = q"AttrModifyingSelector($attribute, $subNodeTree)")

        case ElemSelector(elem, subNodes) =>
          subNodeTreeInfo.copy(tree = q"ElemSelector($elem, $subNodeTree)")
        case IdSelector(id, subNodes) =>
          subNodeTreeInfo.copy(tree = q"IdSelector($id, $subNodeTree)")
        case ClassSelector(className, subNodes) =>
          subNodeTreeInfo.copy(tree = q"ClassSelector($className, $subNodeTree)")
        case NameSelector(name, subNodes) =>
          subNodeTreeInfo.copy(tree = q"NameSelector($name, $subNodeTree)")
        case AttrSelector(name, value, subNodes) =>
          subNodeTreeInfo.copy(tree = q"AttrSelector($name, $value, $subNodeTree)")
        case StarSelector(subNodes, singleDepth) =>
          subNodeTreeInfo.copy(tree = q"StarSelector($subNodeTree, $singleDepth)")
      }
    }

    context.prefix.tree match {
      case q"""CssSelectorMacros.CssSelectorMacros(scala.StringContext.apply($selector))""" =>
        selector match {
          case Literal(Constant(cssSelectorString: String)) =>
            val parsed = CssSelectorParser.parse(cssSelectorString)

            parsed match {
              case Failure(message, _, _) =>
                abortWithMessage(context, s"Invalid CSS selector: $message.")
              case Empty =>
                abortWithMessage(context, "Invalid CSS selector.")

              case Full(selector) =>
                val selectorTreeInfo = treeifySelector(selector)

                selectorTreeInfo.modifiedAttribute.map { attribute =>
                  q"AttrModifyingSelector($attribute, ${selectorTreeInfo.tree})"
                } getOrElse {
                  selectorTreeInfo.tree
                }
            }
          case _ =>
            abortWithMessage(context, "The c macro only supports simple Strings for now.")
        }
      case _ =>
        abortWithMessage(context, "The c macro only supports simple Strings for now.")
    }
  }
}
