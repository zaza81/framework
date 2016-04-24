package net.liftweb.http

import net.liftweb.util.VDom.VDomHelpers
import VDomHelpers._

import org.scalacheck.{Gen, Properties, Prop}
import Prop._

import scala.xml.Node

object VDomGen {
  import org.scalacheck.Gen._

  def genMutation(n:Node) = for {
    index <- choose(1, nodeCount(n) - 1)
    newSibling <- genNode
  } yield {
    insertNode(n, newSibling, index, true)
  }

  def genNode:Gen[Node] = for {
    tag <- oneOf("p", "br", "hr")
  } yield <xml></xml>.copy(label = tag)
}

object UpdateDOMProperties extends Properties("UpdateDOM") {
  val template = recFilter(
    <body data-lift-content-id="main">
      <div id="main" data-lift="surround?with=default;at=content">
        <h2>Welcome to chat</h2>
        <span>Say something!</span>
        <form method="post" data-lift="form.ajax">
          <div data-lift="Chat.submit">
          <input type="text" id="chat-in" name="in"/>
            <input type="submit" value="Submit"/>
            </div>
        </form>
        <div>
          <ul data-lift="Chat.messages">
            <li class="chat-message">Message 1</li>
            <li class="chat-message clearable">Message 2</li>
            <li class="chat-message clearable">Message 3</li>
          </ul>
        </div>
      </div>
    </body>,
    isntWhitespace)

  property("UpdateDOM should handle an arbitrary mutation of our static template") = forAll(VDomGen.genMutation(template)) { after =>
    UpdateDOMSpec.roundTrip(template, after) == after
  }
}
