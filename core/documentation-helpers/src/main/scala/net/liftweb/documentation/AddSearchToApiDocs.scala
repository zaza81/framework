package net.liftweb
package documentation

import java.io.{File,FileWriter}

import scala.io.Source
import scala.xml._

import common._
import util.Html5
import util.Helpers._


case class FileInfo(file: File, contents: String, nestLevel: Int)

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

    val conversionResults: Box[List[Unit]] =
      files.map {
        case FileInfo(file, fileContents, nestLevel) =>
          println(s"Processing ${file.getName}")
          Html5.parse(fileContents).map { docHtml =>
            val directoryAdjustment = (1 to nestLevel).map(_ => "..").mkString("/")
            val libPath = s"$directoryAdjustment/lib/search.js"

            val transforms =
              "head *+" #> <script type="text/javascript" src={libPath}></script>

            Html5.write(
              transforms(docHtml)(0),
              new FileWriter(file),
              stripComment = true,
              convertAmp = false
            )
          }
      }.toList.toSingleBox("Conversions failed; got errors:")

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
