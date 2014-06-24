package net.liftweb
package documentation

import java.io.{File,FileWriter}

import scala.io.Source
import scala.xml._

import common._
import util.Html5
import util.Helpers._

sealed trait ItemType
object ItemType {
  case object Singleton extends ItemType
  case object Class extends ItemType
  case object Trait extends ItemType
}

case class FileInfo(file: File, contents: String, nestLevel: Int)

case class MemberApiInfo(modifier: String, name: String, paramString: String, result: String)
case class ApiInfo(itemType: ItemType, name: String, signature: String, members: List[MemberApiInfo])

object AddSearchToApiDocs extends App {
  object FileWithContents {
    def unapply(file: File): Option[(File, String)] = {
      tryo(Source.fromFile(file).mkString).map((file, _))
    }
  }

  def apiFilesForDirectory(directoryFile: File, nestLevel: Int): Stream[FileInfo] = {
    directoryFile.listFiles.toStream flatMap {
      case file if file.getName.endsWith(".html") =>
        tryo(Source.fromFile(file).mkString).map(FileInfo(file, _, nestLevel)).toStream
      case directoryFile if directoryFile.isDirectory =>
        apiFilesForDirectory(directoryFile, nestLevel + 1)
      case _ =>
        Stream.empty
    }
  }

  def apiFiles: Box[Stream[FileInfo]] = {
    val baseFile = new File(args(0))

    for {
      apiDirectory <-
        ((Full(baseFile)
          .filter(_.exists) ?~ s"'$baseFile' should be a directory, but does not exist.")
          .filter(_.isDirectory) ?~ s"'$baseFile' should be a directory, not a file.")
    } yield {
      apiFilesForDirectory(apiDirectory, 0)
    }
  }

  private def forElement(label: String, handlerFn: (Elem)=>Unit): (NodeSeq)=>NodeSeq = {
    { ns: NodeSeq =>
      ns match {
        case matched: Elem if matched.label == label =>
          handlerFn(matched)

          matched

        case other =>
          other
      }
    }
  }

  private def hasClass_?(element: Elem, className: String) = {
    element.attribute("class") match {
      case Some(thing) =>
        charSplit(thing.text, ' ').exists(_ == className)
      case _ =>
        false
    }
  }

  /**
   * Returns a transformation function that leaves its contents untouched, but
   * extracts an ApiInfo from them. This ApiInfo is passed to the
   * `assignmentFn` immediately before returning the unchanged contents.
   */
  def extractApiInfo(assignmentFn: (ApiInfo)=>Unit): (NodeSeq)=>NodeSeq = {
    var info = ApiInfo(ItemType.Class, "", "", Nil)
    var extracted = false

    val extractType =
      "body [class]" #> forElement("body", { body =>
          if (hasClass_?(body, "value")) {
            info.copy(itemType = ItemType.Singleton)
          }

          extracted = hasClass_?(body, "value") || hasClass_?(body, "type")
      })

    val extractName =
      "#definition h1" #> forElement("h1", { h1 =>
        info = info.copy(name = h1.text.trim.replaceAll("\\s+", " "))
      })

    val extractSignature =
      "#signature" #> forElement("h4", { h4 =>
        val itemType =
          if (h4.text.contains("trait")) {
            ItemType.Trait
          } else {
            info.itemType
          }

        info = info.copy(
          itemType = itemType,
          signature = h4.text.trim.replaceAll("\\s+", " ")
        )
      })

    //val extractMembers =
    // "#allMembers li" #> {
    //  }

    val extractAll =
      extractType andThen
      extractName &
      extractSignature

    { ns: NodeSeq =>
      extractAll(ns)

      if (extracted)
        assignmentFn(info)

      ns
    }
  }

  if (args.length < 1) {
    Console.err.println(
      "Expected one argument: the base directory of the API documentation to add search to."
    )
  } else {
    val files =
      apiFiles match {
        case Failure(message, _, _) =>
          Console.err.println(s"Something went wrong: $message")
          sys.exit(1)
        case Empty =>
          Console.err.println(s"Something weird went wrong...")
          sys.exit(1)

        case Full(files) =>
          files
      }
      apiFiles openOr {
        Console.err
        sys.exit(1)
        Stream.empty
      }

    var apiInfo = List[ApiInfo]()

    val conversionResults: Box[List[Unit]] =
      files.map {
        case FileInfo(file, fileContents, nestLevel) =>
          println(s"Processing ${file.getName}")
          Html5.parse(fileContents).map { docHtml =>
            val directoryAdjustment = (1 to nestLevel).map(_ => "..").mkString("/")
            val libPath = s"$directoryAdjustment/lib/search.js"

            val transforms =
              "head *+" #> <script type="text/javascript" src={libPath}></script> andThen
              extractApiInfo(apiInfo ::= _)

            Html5.write(
              transforms(docHtml)(0),
              new FileWriter(file),
              stripComment = true,
              convertAmp = false
            )
          }
      }.toList.toSingleBox("Conversions failed; got errors:")

    println(apiInfo)

    conversionResults match {
      case ParamFailure(message, _, _, boxes: List[_]) =>
        val errors =
          boxes.collect {
            case Failure(message, _, _) => message
          }.mkString("\n - ")

        Console.err.println(s"$message\n - $errors")
        sys.exit(errors.length)

      case _ =>
    }
  }
}
