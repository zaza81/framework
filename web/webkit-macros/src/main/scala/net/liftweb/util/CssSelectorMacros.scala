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

    def treeifySubNode(subNode: SubNode): context.Tree = {
      subNode match {
        case KidsSubNode() =>
          q"KidsSubNode()"
        case PrependKidsSubNode() =>
          q"PrependKidsSubNode()"
        case AppendKidsSubNode() =>
          q"AppendKidsSubNode()"
        case SurroundKids() =>
          q"SurroundKids()"

        case DontMergeAttributes =>
          q"DontMergeAttributes"

        case AttrSubNode(attr) =>
          q"AttrSubNode($attr)"
        case AttrAppendSubNode(attr) =>
          q"AttrAppendSubNode($attr)"
        case AttrRemoveSubNode(attr) =>
          q"AttrRemoveSubNode($attr)"

        case SelectThisNode(kids) =>
          q"SelectThisNode($kids)"
      }
    }

    def treeifySubNodes(subNodes: Box[SubNode]): context.Tree = {
      subNodes match {
        case Empty => q"Empty"
        case failure: Failure => treeifyFailure(failure)
        case Full(subNode) =>
          q"Full(${treeifySubNode(subNode)})"
      }
    }

    def treeifySelector(selector: CssSelector): context.Tree = {
      import context.universe._

      selector match {
        case EnclosedSelector(parent, kid) =>
          q"EnclosedSelector(${treeifySelector(parent)}, ${treeifySelector(kid)})"

        case ElemSelector(elem, subNodes) =>
          q"ElemSelector($elem, ${treeifySubNodes(subNodes)})"
        case IdSelector(id, subNodes) =>
          q"IdSelector($id, ${treeifySubNodes(subNodes)})"
        case ClassSelector(className, subNodes) =>
          q"ClassSelector($className, Empty)"
        case NameSelector(name, subNodes) =>
          q"NameSelector($name, Empty)"
        case AttrSelector(name, value, subNodes) =>
          q"AttrSelector($name, $value, Empty)"
        case StarSelector(subNodes, singleDepth) =>
          q"StarSelector(${treeifySubNodes(subNodes)}, $singleDepth)"
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
                treeifySelector(selector)
            }
          case _ =>
            abortWithMessage(context, "The c macro only supports simple Strings for now.")
        }
      case _ =>
        abortWithMessage(context, "The c macro only supports simple Strings for now.")
    }
  }
}
