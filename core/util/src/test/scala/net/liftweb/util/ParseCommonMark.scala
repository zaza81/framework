package net.liftweb
package util

import scala.xml._

/**
 * This file is meant for use with the CommonMark spec tester
 * (https://github.com/jgm/stmd). Since the spec runner there expects to run an
 * executable, this is a thin wrapper around lift-util's `CommonMarkParser`.
 *
 * Output is sent to stderr so that output redirection can be used to filter
 * out sbt's output, which can't be disabled. A sample command for `bash`:
 *
 * ```
 * sbt "project lift-util"  "test:run-main net.liftweb.util.ParseCommonMark" 2>&1 1> boom
 * ```
 */
object ParseCommonMark extends App {
  val commonMark = io.Source.stdin.getLines.mkString("\n")

  CommonMarkParser.parse(commonMark).map { result =>
    result match {
      case node: Node =>
        Html5.write(node, new java.io.PrintWriter(System.err), true, true)
      case _ =>
        Console.err.println(result)
    }
  }
}
