/*
 * Copyright 2007-2015 WorldWide Conferencing, LLC
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
package http

import net.liftweb.common._
import net.liftweb.actor._
import net.liftweb.util.Helpers._
import net.liftweb.util._
import net.liftweb.json._
import scala.xml.{NodeSeq, Text, Elem, Node, Group, Null, PrefixedAttribute, UnprefixedAttribute}
import scala.collection.mutable.ListBuffer
import net.liftweb.http.js._
import JsCmds._
import JE._
import java.util.Locale

/**
 * A case class that contains the information necessary to set up a
 * `[[LiftCometActor]]`.
 */
final case class CometCreationInfo(cometType: String,
                                   cometName: Box[String],
                                   cometHtml: NodeSeq,
                                   cometAttributes: Map[String, String],
                                   session: LiftSession)

/**
 * Base trait specifying the core interface for Lift's comet actors. The most
 * interesting implementing classes are the `[[MessagingCometActor]]` and
 * `[[RenderingCometActor]]` classes, which provide simple message push and more
 * complex component rendering and rerendering, respectively.
 *
 * The fundamental behavior of comet actors is as regular `[[LiftActor]]`s, with
 * some helper functions to assist in sending updates to the client. The core
 * interface also provides functions that give insight into the lifecycle of the
 * actor, including information about the last time an update was sent to the
 * client and various timeouts and error handlers.
 *
 * Note that like most actors, you should never interact with actor state
 * directly; instead, _all_ actions should send a message to the actor, and
 * state should be managed in the message handler. This avoids concurrency
 * issues with simultaneous updaters of the actor's state.
 *
 * Functions invoked from the client via `[[S.fmapFunc]]`, including form field
 * bindings, can interact with comet state directly as long as they are bound
 * inside the comet actor's message handler. Lift transparently invokes these
 * callbacks via a message sent to the actor so they are coherent with the rest
 * of the actor's state.
 */
trait LiftCometActor extends TypedActor[Any, Any] with ForwardableActor[Any, Any] with Dependent {
  /**
   * An id that identifies this comet uniquely, preferably universally
   * so. Typically generated at instantiation time via
   * `[[HttpHelpers.nextFuncName]]`.
   */
  def uniqueId: String

  /**
   * The timestamp when we last saw a listener initiate or terminate a
   * connection.
   */
  def lastListenerTime: Long

  @deprecated("lastRenderTime only makes sense on MessagingCometActors and will be removed from the LiftCometActor interface.", "3.1.0")
  def lastRenderTime: Long

  // Exposes initCometActor for internal invocation by Lift when external
  // initialization is needed.
  private[http] def callInitCometActor(creationInfo: CometCreationInfo) {
    initCometActor(creationInfo)
  }

  /**
   * Override in sub-class to customise timeout for the render()-method for the specific comet
   */
  @deprecated("cometRenderTimeout only makes sense on RenderingCometActors and will be removed from the LiftCometActor interface. It will also be made protected.", "3.1.0")
  def cometRenderTimeout: Long // = LiftRules.cometRenderTimeout

  /**
   * Provides the base rendering for a `RenderingCometActor` that failed to finish rendering
   * within `[[cometRenderTimeout]]` milliseconds of being asked
   * to. `RenderingCometActor`s have `cometRenderTimeout` milliseconds to finish
   * their initial rendering; if they have not finished within that timeframe,
   * this method provides the fallback content.
   * 
   * Do _not_ manipulate actor-state here. If you want to manipulate state, send
   * the actor a new message from inside this method. Because it is called in
   * reaction to a message taking too long to process, it is invoked outside of
   * the usual message handling pipeline.
   * 
   * A possible render timeout handler:
   * {{{
   *   override def renderTimeoutHandler(): Box[NodeSeq] = {
   *     Full(<div>Comet {this.getClass} timed out, timeout is {cometRenderTimeout}ms</div>)
   *   }
   * }}}
   */
  @deprecated("cometRenderTimeoutHandler only makes sense on RenderingCometActors and will be removed from the LiftCometActor interface. It will also be made protected.", "3.1.0")
  def cometRenderTimeoutHandler(): Box[NodeSeq] = Empty

  @deprecated("cometProcessingTimeout is no longer used and will be removed.", "3.1.0")
  def cometProcessingTimeout = LiftRules.cometProcessingTimeout

  @deprecated("cometProcessingTimeoutHandler is no longer used and will be removed.", "3.1.0")
  def cometProcessingTimeoutHandler(): JsCmd = Noop

  protected def initCometActor(creationInfo: CometCreationInfo): Unit

  /**
   * A `String` describing what type of comet this is. Comets without types are
   * no longer well supported, and the ability to override `theType` will soon
   * disappear, as a comet's type is used for internal tracking and lookup, and
   * provided to the comet as part of its setup process.
   */
  def theType: Box[String]

  /**
   * A customizable name for this comet, possibly empty. The ability to override
   * `name` will soon disappear, as the comet's name is used for internal
   * tracking and lookup, and provided to the comet as part of its setup
   * process.
   */
  def name: Box[String]

  /**
   * Indicates whether, when rendered, this comet's contents should be
   * surrounded by a generated container.
   */
  @deprecated("hasOuter only makes sense on RenderingCometActors and will be removed from the LiftCometActor interface.", "3.1.0")
  def hasOuter: Boolean

  /**
   * Builds a container for the rendered contents of this `RenderingCometActor`,
   * based on the value of `[[parentTag]]`.
   */
  @deprecated("buildSpan only makes sense on RenderingCometActors and will be removed from the LiftCometActor interface and renamed to buildContainer.", "3.1.0")
  def buildSpan(xml: NodeSeq): NodeSeq

  /**
   * A template `Elem` for the container this `RenderingCometActor`'s rendered
   * contents will be wrapped in if `hasOuter` is `true`.
   */
  @deprecated("parentTag only makes sense on RenderingCometActors and will be removed from the LiftCometActor interface and renamed to containerElement.", "3.1.0")
  def parentTag: Elem

  /**
   * See `[[flushWiringUpdates]]`.
   */
  @deprecated("Use flushWiringUpdates instead.","3.1.0")
  def poke(): Unit = flushWiringUpdates

  /**
   * Make this actor flush any `Wiring` updates in the component.
   *
   * This method is actor-safe and may be called from any thread, not just the
   * Actor's message handler thread.
   */
  def flushWiringUpdates(): Unit = {}

  /**
   * Indicates whether this comet actor intends to capture the `[[Req]]`s that
   * add the comet. Note that a single comet actor can exist on multiple pages,
   * and that in these cases, only the first request that causes the comet to be
   * created is made available for capture.
   *
   * Setting this to `true` ensures that the request that is handed to the
   * `[[captureInitialReq]]` method is snapshotted, meaning that its body is
   * fully read and stored. For performance reasons, this is only done when
   * explicitly requested, so this flag will ensure that that step is performed
   * before the comet gets an opportunity to read it.
   *
   * Note that you'll also have to override `captureInitialReq` to actually
   * store the `Req`; by default, that method is a no-op.
   */
  def sendInitialReq_? : Boolean = false

  // FIXME Does this only makes sense for RenderingCometActor?
  /**
   * Hook for `Wiring` to notify this comet actor when a `[[Cell]]` bound from
   * within the actor was changed.
   */
  def predicateChanged(which: Cell[_]): Unit = {
    flushWiringUpdates()
  }

  /**
   * The locale to be used for this comet actor. By default, this is set to the
   * creating session's locale when the actor is initialized.
   */
  def cometActorLocale: Locale = _myLocale

  private var _myLocale = Locale.getDefault()

  private[http] def setCometActorLocale(loc: Locale) {
    _myLocale = loc
  }
}

/**
 * Provides access to the current comet actor, if we are currently handling
 * messages for a comet actor. Usually accessed through `[[S.comet]]`.
 */
object CurrentCometActor extends ThreadGlobal[LiftCometActor]

/**
 * Takes care of the plumbing for building Comet-based Web Apps
 */
trait BaseCometActor extends LiftActor with LiftCometActor with CssBindImplicits {
  private val logger = Logger(classOf[CometActor])

  val uniqueId = Helpers.nextFuncName

  @volatile
  private var _lastListenerTime: Long = millis

  /**
   * The last "when" sent from the listener
   * @return the last when sent from the listener
   */
  def lastListenerTime: Long = _lastListenerTime

  /**
    * If this method returns true, this CometActor will be treated as a source
    * of continuous partial updates. Usually the basic assumption of a Lift
    * Comet Actor is that render will display the current state of the actor.
    * Partial updates may modify that display, but it is always possible to
    * recreate the complete display by calling render again (in which case
    * pending partial updates can be safely discarded). If this method
    * returns true, render will not be called, but partial updates are
    * guaranteed to always be sent to the client. This is useful as a means to
    * push arbitrary JavaScript to the client without a distinct UI for the
    * comet actor itself.
    */
  //private[http] def partialUpdateStream_? : Boolean = false

  @transient private var listeners: List[(ListenerId, AnswerRender => Unit)] = Nil

  protected var deltas: List[Delta] = Nil
  private val notices = new ListBuffer[(NoticeType.Value, NodeSeq, Box[String])]

  private var _deltaPruner: (BaseCometActor, List[Delta]) => List[Delta] =
    (actor, d) => {
      val m = Helpers.millis
      d.filter(d => (m - d.timestamp) < 120000L)
    }

  private var _theSession: LiftSession = _

  def theSession = _theSession

  private var _name: Box[String] = Empty

  @volatile private var _defaultHtml: NodeSeq = _

  /**
   * If this comet was initialized from a `[[Comet]]` snippet
   * invocation, contains the contents of the snippet. For example, for
   * this invocation:
   *
   * {{{
   * <div data-lift="comet?type=MagicComet">
   *   <p>Some text</p>
   * </div>
   * }}}
   *
   * `defaultHtml` would be `<p>Some text</p>` after comet
   * initialization.
   */
  def defaultHtml: NodeSeq = _defaultHtml

  /**
   * The optional name for this comet. This can be used to send messages
   * to just this instance. A given comet [[theType type]] + name
   * combination is unique across a single Lift session.
   */
  def name: Box[String] = _name

  // FIXME This is no longer optional, should become a `String`.
  private var _theType: Box[String] = Empty

  /**
   * The type of this comet. Often corresponds to the name of the comet
   * class, but if you use `[[LiftRules.cometCreation]]` or
   * `[[LiftRules.cometCreationFactory]]`, that may not be the case.
   */
  def theType: Box[String] = _theType

  private var _attributes: Map[String, String] = Map.empty

  /**
   * If this comet was initialized from a `[[Comet]]` snippet
   * invocation, contains the attributes passed to that snippet. For
   * example, for this invocation:
   *
   * {{{
   * <div data-lift="comet?type=MagicComet&name=Hello&myAttribute=13"
   * }}}
   *
   * `attributes` would be, roughly, `("type" -> "Magic", "name" ->
   * "Hello", "myAttribute" -> "13")`.
   */
  def attributes = _attributes

  /**
   * The lifespan of this comet.  By default `lifespan` is `Empty`,
   * meaning a `LiftCometActor` will last for the entire session that it
   * was created in, even if the comet is not currently on a page the
   * user has open.
   * 
   * When set to `Full`, the comet will be shut down if it is not seen
   * on a page for some amount of time after the `lifespan` has
   * passed. How long after the `lifespan` a comet will be killed is
   * not precisely defined, though it should be within 30s.
   *
   * A comet is "seen" on a page when a request is opened from a client
   * browser that is waiting for updates from it, or when such a request
   * is closed. Thus, if the `lifespan` is less than the
   * `[[LiftRules.cometRequestTimeout]]`, you run the risk of the actor
   * being killed while it is still visible on a page (because we only
   * see it when the request in question starts and ends, not in
   * between).
   */
  def lifespan: Box[TimeSpan] = Empty

  // Used to find out when we shut the comet down to control running
  // further messages.
  private var _shutDownAt = millis

  private var _running = true

  /**
   * True if the actor is currently running, false if it has been shut
   * down.
   *
   * A comet can be explicitly shut down by sending it a `ShutDown`
   * message, or it can be automatically shut down when a session is
   * being shut down or when the comet has lived past its
   * `[[lifespan]]`.
   */
  protected def running = _running

  /**
   * Initializes the comet actor. Generally you should override
   * `localSetup` rather than this method; this method is mostly
   * meant for traits and classes that significantly modify the comet
   * lifecycle.
   */
  protected def initCometActor(creationInfo: CometCreationInfo) {
    _theType = Full(creationInfo.cometType)
    _name = creationInfo.cometName
    _defaultHtml = creationInfo.cometHtml
    _attributes = creationInfo.cometAttributes
    _theSession = creationInfo.session
  }

  @deprecated("Originally used as a default prefix for old-style binding, now barely used; will be removed in Lift 3.1.0; JSON functions will be tagged in a comment with the comet type + name instead of the default prefix.", "3.0.0")
  def defaultPrefix: Box[String] = Empty

  // FIXME Make this be type + name, and do it after type is mandatory.
  private lazy val _defaultPrefix: String = (defaultPrefix or _name) openOr "comet"

  /**
   * Return the list of `ListenerId`s for all long poll agents that are
   * waiting for this CometActor to change its state. This method is
   * useful for detecting presence.
   */
  protected def cometListeners: List[ListenerId] = listeners.map(_._1)

  /**
   * This method will be called when there's a change in the long poll
   * listeners. The method does nothing by default, but you can override
   * it to get a granular sense of how many browsers care about this
   * comet (in conjunction with `[[cometListeners]]`).
   *
   * Note that this method should not block for any material time, and
   * if there's any complicated processing to do, you should use
   * Scheduler.schedule or send a message to the comet. Do not change
   * the comet's state from this method.
   */
  protected def listenerTransition(): Unit = {}

  // FIXME Split out JSON functionality into a trait.
  /**
   * If there's actor-specific JSON behavior on failure to make the JSON
   * call, include the JavaScript here.
   */
  def onJsonError: Box[JsCmd] = Empty

  // FIXME Split out JSON functionality into a trait.
  /**
   * Override this method to deal with JSON sent from the browser
   * via the `[[jsonSend]]` function. If you use the jsonSend call, you
   * will get a `JObject(JField("command", cmd), JField("param",
   * params))`.
   */
  def receiveJson: PartialFunction[JsonAST.JValue, JsCmd] = Map()

  // FIXME Split out JSON functionality into a trait.
  /**
   * The JavaScript call that you use to send the data to the
   * server. For example:
   *
   * {{{
   * "button [onclick]" #> jsonSend("Hello", JsRaw("Dude".encJs))
   * }}}
   *
   * See `[[JsonCall]]` for more on the parameters it accepts.
   */
  def jsonSend: JsonCall = _sendJson

  // FIXME Split out JSON functionality into a trait.
  /**
   * The call that packages up the JSON and tosses it to the server.  If
   * you set `[[autoIncludeJsonCode]]` to true, then this will be
   * included in the stuff sent to the server.
   */
  def jsonToIncludeInCode: JsCmd = _jsonToIncludeCode

  // FIXME Revamp the way `createJsonFunc` works to not produce JS.
  // FIXME Split out JSON functionality into a trait.
  private lazy val (_sendJson, _jsonToIncludeCode) = S.createJsonFunc(Full(_defaultPrefix), onJsonError, receiveJson _)

  /**
   * Set this method to true to have the Json call code included in the
   * Comet output
   */
  def autoIncludeJsonCode: Boolean = false

  /**
   * Called to log an exception-related error during message dispatch
   * along with the exception that caused it.
   *
   * By default, simply uses the comet's logger to log at an ERROR
   * level.
   */
  protected def reportError(message: String, exception: Exception) {
    logger.error(message, exception)
  }

  /**
   * Wraps `[[composeFunction]]`, which does actual message handling,
   * with the appropriate session and request state setup. Don't
   * override this unless you really know what you're doing.
   */
  protected override def messageHandler = {
    val what = composeFunction
    val myPf: PartialFunction[Any, Unit] = new PartialFunction[Any, Unit] {
      def apply(in: Any): Unit =
        CurrentCometActor.doWith(BaseCometActor.this) {
          S.initIfUninitted(theSession) {
            RenderVersion.doWith(uniqueId) {
              S.functionLifespan(true) {
                try {
                  what.apply(in)
                } catch {
                  case e if exceptionHandler.isDefinedAt(e) => exceptionHandler(e)
                  case e: Exception => reportError("Message dispatch for " + in, e)
                }

                val updatedJs = S.jsToAppend
                if (updatedJs.nonEmpty) {
                  pushMessage(updatedJs)
                }

                if (S.functionMap.size > 0) {
                  theSession.updateFunctionMap(S.functionMap,
                    uniqueId, lastRenderTime)
                  S.clearFunctionMap
                }
              }
            }
          }
        }

      def isDefinedAt(in: Any): Boolean =
        CurrentCometActor.doWith(BaseCometActor.this) {
          S.initIfUninitted(theSession) {
            RenderVersion.doWith(uniqueId) {
              S.functionLifespan(true) {
                try {
                  what.isDefinedAt(in)
                } catch {
                  case e if exceptionHandler.isDefinedAt(e) => exceptionHandler(e); false
                  case e: Exception => reportError("Message test for " + in, e); false
                }
              }
            }
          }
        }
    }

    myPf
  }

  /**
   * By default, `BaseCometActor` handles `RedirectShortcutException`,
   * which is used to handle many types of redirects in Lift. If you
   * override this `PartialFunction` to do your own exception handling
   * and want redirects from e.g. `S.redirectTo` to continue working
   * correctly, make sure you chain back to this implementation.
   */
  override def exceptionHandler: PartialFunction[Throwable, Unit] = {
    case  ResponseShortcutException(_, Full(redirectUri), _) =>
      pushMessage(RedirectTo(redirectUri))

    case other if super.exceptionHandler.isDefinedAt(other) =>
      super.exceptionHandler(other)
  }

  /**
   * Handle messages sent to this actor before anything else.
   *
   * Note that if you catch a message needed for the comet lifecycle
   * here, built-in lifecycle handling won't take place!
   */
  def highPriority: PartialFunction[Any, Unit] = Map.empty

  /**
   * Handle messages sent to this actor if the built-in message
   * handling didn't catch it. This is the most common place to install
   * comet-specific message handling.
   */
  def lowPriority: PartialFunction[Any, Unit] = Map.empty

  /**
   * Handle messages sent to this Actor before the built-in message
   * handling occurs.
   *
   * Note that if you catch a message needed for the comet lifecycle
   * here, built-in lifecycle handling won't take place!
   */
  def mediumPriority: PartialFunction[Any, Unit] = Map.empty

  private[http] def _lowPriority: PartialFunction[Any, Unit] = {
    case s => logger.debug("CometActor " + this + " got unexpected message " + s)
  }

  // TODO Split asking/whosAsking into separate trait?
  private var askingWho: Box[LiftCometActor] = Empty
  private var whosAsking: Box[LiftCometActor] = Empty
  private var answerWith: Box[Any => Any] = Empty

  private lazy val _mediumPriority: PartialFunction[Any, Unit] = {
    case l@Unlisten(seq) => {
      _lastListenerTime = millis
      askingWho match {
        case Full(who) => forwardMessageTo(l, who) // forward l
        case _ => listeners = listeners.filter(_._1 != seq)
      }
      listenerTransition()
    }


    case l@Listen(when, seqId, toDo) => {
      _lastListenerTime = millis
      askingWho match {
        case Full(who) => forwardMessageTo(l, who) // who forward l
        case _ =>
          if (when < lastRenderTime && ! partialUpdateStream_?) {
            toDo(AnswerRender(new XmlOrJsCmd(spanId, lastRendering,
              buildSpan _, notices.toList),
              whosAsking openOr this, lastRenderTime, wasLastFullRender))
            clearNotices
          } else {
            _lastRenderTime = when
            deltas.filter(_.when > when) match {
              case Nil => listeners = (seqId, toDo) :: listeners

              case all@(hd :: xs) => {
                toDo(AnswerRender(new XmlOrJsCmd(spanId, Empty, Empty,
                  Full(all.reverse.foldLeft(Noop)(_ & _.js)), Empty, buildSpan, false, notices.toList),
                  whosAsking openOr this, hd.when, false))
                clearNotices
              }
            }
            deltas = _deltaPruner(this, deltas)
          }
      }
      listenerTransition()
    }


    case PerformSetupComet2(initialReq) => {
      localSetup()
      captureInitialReq(initialReq)
      performReRender(true)
    }

    /**
     * Update the defaultHtml... sent in dev mode
     */
    case UpdateDefaultHtml(html) => {
      val redo = html != _defaultHtml

      _defaultHtml = html

      if (redo) {
        performReRender(false)
      }
    }

    case AskRender =>
      askingWho match {
        case Full(who) => forwardMessageTo(AskRender, who) //  forward AskRender
        case _ => {
          val out =
            if (receivedDelta || alwaysReRenderOnPageLoad) {
              try {
                Full(performReRender(false))
              } catch {
                case e if exceptionHandler.isDefinedAt(e) => {
                  exceptionHandler(e)
                  Empty
                }
                case e: Exception => {
                  reportError("Failed performReRender", e)
                  Empty
                }
              }
            } else {
              Empty
            }

          reply(AnswerRender(new XmlOrJsCmd(spanId, out.openOr(lastRendering),
            buildSpan _, notices.toList),
            whosAsking openOr this, lastRenderTime, true))
          clearNotices
        }
      }


    case ActionMessageSet(msgs, req) =>
      S.doCometParams(req.params) {
        val computed: List[Any] =
          msgs.flatMap {
            f => try {
              List(f())
            } catch {
              case e if exceptionHandler.isDefinedAt(e) => exceptionHandler(e); Nil
              case e: Exception => reportError("Ajax function dispatch", e); Nil
            }
          }

        reply(computed ::: List(S.noticesToJsCmd))
      }

    case AskQuestion(what, who, otherlisteners) => {
      this.spanId = who.uniqueId
      this.listeners = otherlisteners ::: this.listeners
      startQuestion(what)
      whosAsking = Full(who)
      this.reRender(true)
      listenerTransition()
    }

    case AnswerQuestion(what, otherListeners) =>
      askingWho.foreach {
        ah => {
          reply("A null message to release the actor from its send and await reply... do not delete this message")
          // askingWho.unlink(self)
          ah ! ShutDown
          this.listeners = this.listeners ::: otherListeners
          this.askingWho = Empty
          val aw = answerWith
          answerWith = Empty
          aw.foreach(_(what))
          performReRender(true)
          listenerTransition()
        }
      }

    case ShutdownIfPastLifespan =>
      for {
        ls <- lifespan if listeners.isEmpty && (lastListenerTime + ls.millis + 1000l) < millis
      } {
        this ! ShutDown
      }

    case ReRender(all) => performReRender(all)

    case SetDeltaPruner(f) =>
      _deltaPruner = f
      deltas = f(this, deltas)

    case Error(id, node) => notices += ((NoticeType.Error, node, id))

    case Warning(id, node) => notices += ((NoticeType.Warning, node, id))

    case Notice(id, node) => notices += ((NoticeType.Notice, node, id))

    case ClearNotices => clearNotices

    case ShutDown =>
      logger.info("The CometActor " + this + " Received Shutdown")
      askingWho.foreach(_ ! ShutDown)
      theSession.removeCometActor(this)
      _localShutdown()

    case PartialUpdateMsg(cmdF) => {
      val cmd: JsCmd = cmdF.apply
      val time = Helpers.nextNum
      val delta = JsDelta(time, cmd)
      receivedDelta = true
      theSession.updateFunctionMap(S.functionMap, uniqueId, time)
      S.clearFunctionMap
      deltas = _deltaPruner(this,  (delta :: deltas))
      if (!listeners.isEmpty) {
        val postPage = theSession.postPageJavaScript()
        val rendered =
          AnswerRender(new XmlOrJsCmd(spanId, Empty, Empty,
            Full(cmd & postPage),
            Empty, buildSpan, false,
            notices.toList),
            whosAsking openOr this, time, false)
        clearNotices
        listeners.foreach(_._2(rendered))
        listeners = Nil
        listenerTransition()
      }
    }
  }

  /**
   * Clear the common dependencies for Wiring.  This method will
   * clearPostPageJavaScriptForThisPage() and unregisterFromAllDependencies().
   * The combination will result in a clean slate for Wiring during a redraw.
   * You can change the behavior of the wiring dependency management by
   * overriding this method.
   */
  protected def clearWiringDependencies() {
    if (!manualWiringDependencyManagement) {
      theSession.clearPostPageJavaScriptForThisPage()
      unregisterFromAllDependencies()
    }
  }

  /**
   * By default, Lift deals with managing wiring dependencies.  This means on
   * each full render (a full render will happen on `[[reRender]]` or on a page
   * load if there have been partial updates.), you may want to manually deal
   * with wiring dependencies.  If you do, override this method and return true.
   */
  protected def manualWiringDependencyManagement = false

  def unWatch = pushMessage(Call("lift.unlistWatch", uniqueId))

  /**
   * Poke the CometActor and cause it to do a partial update Noop which
   * will have the effect of causing the component to redisplay any
   * Wiring elements on the component.
   * This method is Actor-safe and may be called from any thread, not
   * just the Actor's message handler thread.
   */
  override def poke(): Unit = {
    if (running) {
      pushMessage(Noop)
    }
  }

  /**
   * Perform a partial update of the comet component based
   * on the JsCmd.  This means that the JsCmd will be sent to
   * all of the currently listening browser tabs.  This is the
   * preferred method over reRender to update the component
   */
  protected def pushMessage(cmd: =>JsCmd) {
    this ! PartialUpdateMsg(() => cmd)
  }

  protected def startQuestion(what: Any) {
  }

  /**
   * This method will be called after the actor has started and been
   * initialized. Do any actor setup here.  DO NOT do initialization in the
   * constructor or in `[[initCometActor]]`... Do it here.
   */
  protected def localSetup(): Unit = {
  }

  /**
   * Comet actors live outside the HTTP request/response cycle. However, it may
   * be useful to know what `[[Req]]` led to the creation of the comet.  You can
   * override this method and capture the initial `Req` object.  Note that
   * keeping a reference to the `Req` may lead to memory retention issues if the
   * `Req` contains large message bodies, etc.  It's optimal to capture the path
   * or capture any request parameters that you care about rather the keeping
   * the whole `Req` reference.
   */
  protected def captureInitialReq(initialReq: Box[Req]) {
  }

  private def _localShutdown() {
    localShutdown()
    clearNotices
    listeners = Nil
    askingWho = Empty
    whosAsking = Empty
    deltas = Nil
    receivedDelta = false
    jsonHandlerChain = Map.empty
    _running = false
    _shutDownAt = millis
  }

  /**
   * This method will be called as part of the shut-down of the actor.  Release
   * any resources here.
   */
  protected def localShutdown(): Unit = {
  }

  /**
   * Compose the message handler function. By default, composes `highPriority
   * orElse mediumPriority orElse internalHandler orElse lowPriority orElse
   * internalHandler`.
   *
   * This hook lets you change how the handler works if doing stuff in
   * `highPriority`, `mediumPriority` and `lowPriority` is not enough.
   */
  protected def composeFunction: PartialFunction[Any, Unit] = composeFunction_i

  private def composeFunction_i: PartialFunction[Any, Unit] = {
    // if we're no longer running don't pass messages to the other handlers
    // just pass them to our handlers
    if (!_running && (millis - 20000L) > _shutDownAt)
      _mediumPriority orElse _lowPriority
    else
      highPriority orElse mediumPriority orElse
        _mediumPriority orElse lowPriority orElse _lowPriority
  }

  // FIXME Should be in RenderingCometActor?
  /**
   * Ask another `[[LiftCometActor]]` a question.  That other comet will take
   * over the screen real estate until the question is answered.
   */
  protected def ask(who: LiftCometActor, what: Any)(answerWith: Any => Unit) {
    who.callInitCometActor(CometCreationInfo(who.uniqueId, name, defaultHtml, attributes, theSession))
    theSession.addCometActor(who)

    who ! PerformSetupComet2(Empty)
    askingWho = Full(who)
    this.answerWith = Full(answerWith)
    who ! AskQuestion(what, this, listeners)
  }

  // FIXME Should be in RenderingCometActor?
  protected def answer(answer: Any) {
    whosAsking.foreach(_ !? AnswerQuestion(answer, listeners))
    whosAsking = Empty
    performReRender(false)
  }

  /**
   * Similar with S.error
   */
  def error(n: String) {
    error(Text(n))
  }

  /**
   * Similar with S.error
   */
  def error(n: NodeSeq) {
    notices += ((NoticeType.Error, n, Empty))
  }

  /**
   * Similar with S.error
   */
  def error(id: String, n: NodeSeq) {
    notices += ((NoticeType.Error, n, Full(id)))
  }

  /**
   * Similar with S.error
   */
  def error(id: String, n: String) {
    error(id, Text(n))
  }

  /**
   * Similar with S.notice
   */
  def notice(n: String) {
    notice(Text(n))
  }

  /**
   * Similar with S.notice
   */
  def notice(n: NodeSeq) {
    notices += ((NoticeType.Notice, n, Empty))
  }

  /**
   * Similar with S.notice
   */
  def notice(id: String, n: NodeSeq) {
    notices += ((NoticeType.Notice, n, Full(id)))
  }

  /**
   * Similar with S.notice
   */
  def notice(id: String, n: String) {
    notice(id, Text(n))
  }

  /**
   * Similar with S.warning
   */
  def warning(n: String) {
    warning(Text(n))
  }

  /**
   * Similar with S.warning
   */
  def warning(n: NodeSeq) {
    notices += ((NoticeType.Warning, n, Empty))
  }

  /**
   * Similar with S.warning
   */
  def warning(id: String, n: NodeSeq) {
    notices += ((NoticeType.Warning, n, Full(id)))
  }

  /**
   * Similar with S.warning
   */
  def warning(id: String, n: String) {
    warning(id, Text(n))
  }

  private def clearNotices {
    notices.clear
  }
}

@deprecated("Use RenderingCometActor instead.", "3.0.0")
trait CometActor extends RenderingCometActor

// Temporary placeholder until we can split out `BaseCometActor` stuff properly.
trait RenderingCometActor extends MessagingCometActor {
  // Temporary placeholder until we can split out `BaseCometActor` stuff
  // properly.
  def cometRenderTimeout: Long = LiftRules.cometRenderTimeout

  private val logger = Logger(classOf[RenderingCometActor])

  private var spanId = uniqueId

  @volatile private var _lastRenderTime = Helpers.nextNum

  /**
   * Get the current render clock for the CometActor
   * @return
   */
  def renderClock: Long = lastRenderTime

  def lastRenderTime: Long =  _lastRenderTime

  private[this] var lastCachedRendering: RenderOut = _

  // Access the last rendering; if `dontCacheRendering` is set, renders
  // the comet; otherwise, provides the previously cached rendering.
  private def lastRendering: RenderOut =
    if (dontCacheRendering) {
      val ret = render
      theSession.updateFunctionMap(S.functionMap, uniqueId, lastRenderTime)
      ret
    } else {
      lastCachedRendering
    }

  /**
   * set the last rendering... ignore if we're not caching
   */
  private def lastRendering_=(last: RenderOut) {
    if (!dontCacheRendering) {
      lastCachedRendering = last
    }
  }

  private var receivedDelta = false
  private var wasLastFullRender = false

  /**
   * Set to `true` if we should run `[[render]]` on every page load.
   * Defaults to `false`.
   */
  protected def alwaysReRenderOnPageLoad = false

  /**
   * Indicates whether, when rendered, this comet's contents should be
   * surrounded by a generated container.
   *
   * @see parentTag
   */
  def hasOuter = true

  /**
   * The template `Elem` for the container this `RenderingCometActor`'s
   * rendered contents will be wrapped in if `hasOuter` is `true`.
   */
  def parentTag = <div style="display: inline"/>

  // FIXME Deprecate and rename to `buildContainer`.
  /**
   * Creates the container element acting as the real estate for comet
   * rendering.
   */
  def buildSpan(xml: NodeSeq): Elem = {
    parentTag.copy(child = xml) % ("id" -> spanId)
  }

  override protected def initCometActor(creationInfo: CometCreationInfo) {
    if (! dontCacheRendering) {
      lastCachedRendering = RenderOut(Full(defaultHtml), Empty, Empty, Empty, false)
    }

    super.initCometActor(creationInfo)
  }

  /**
   * A part of the comet's screen real estate that is not updated by
   * default with `[[reRender]]`.
   *
   * This block of HTML is useful, for example, the editor part of a
   * comet-based control where the data is JSON and updated with
   * `partialUpdate`s.
   */
  def fixedRender: Box[NodeSeq] = Empty

  /**
   * Calculate `fixedRender` and capture the postpage javascript.
   */
  protected def calcFixedRender: Box[NodeSeq] = {
    fixedRender.map { fixedContent =>
      theSession.postPageJavaScript() match {
        case Nil =>
          fixedContent
        case postPageCommands => {
          fixedContent ++ Script(postPageCommands)
        }
      }
    }
  }

  // We have to cache fixedRender and only change it if the template
  // changes or we get a reRender(true).
  private def internalFixedRender: Box[NodeSeq] = {
    if (! cacheFixedRender) {
      calcFixedRender
    } else {
      cachedFixedRender.get
    }
  }

  private val cachedFixedRender: FatLazy[Box[NodeSeq]] = FatLazy(calcFixedRender)

  /**
   * By default, we do not cache the value of `fixedRender`.  If it's
   * expensive to recompute it each time there's a conversion of
   * something to a `[[RenderOut]]`, override this method to enable
   * caching.
   */
  protected def cacheFixedRender = false

  /**
   * Set this method to true if you want to avoid caching the
   * rendering.  This trades space for time.
   */
  protected def dontCacheRendering: Boolean = false

  /**
   * This is the main method to override to define what is rendered by this
   * comet into its container.
   *
   * There are implicit conversions for a bunch of stuff to `[[RenderOut]]`
   * (including `NodeSeq`).  Thus, if you don't declare the return type to be
   * something other than `RenderOut` and return something that's coercible into
   * `RenderOut`, the compiler "does the right thing"(tm) for you.
   *
   * There's an implicit conversion for `(NodeSeq)=>NodeSeq`, so you can
   * return a function (e.g., a `[[CssBindFunc]]`) that will convert the
   * `[[defaultHtml]]` to the correct output.  There's an implicit conversion
   * from `[[JsCmd]]`, so you can return some JavaScript to be sent to the
   * browser. Lastly, there is an implicit conversions for `NodeSeq`, so you can
   * also return XML directly here.
   *
   * Note that this method will be called each time a new browser tab is opened
   * to the comet component or the comet component is otherwise accessed
   * during a full page load (this is true even if a `[[partialUpdate]]` has
   * occurred). It is meant to set the content of the component to the current
   * state of whatever it is representing.
   *
   * When `render` is called, any pending `partialUpdate`s are cleared, so they
   * should be true partial updates---calling `render` at a point in time Y
   * should produce the same thing as calling `render` at a previous point in
   * time X + processing any partial updates between times X and Y. For a more
   * formal state and partial update representation, see `[[StatefulComet]]`.
   *
   * You may also want to look at the `[[fixedRender]]` method which is only
   * called once and sets up a stable rendering state.
   */
  def render: RenderOut

  /**
   * Causes the entire component to be re-rendered (by calling `[[render]]`) and
   * the latest rendering pushed out to any listeners to replace their current
   * contents.  This method will cause the entire component to be rendered which
   * can result in a huge blob of JavaScript to be sent to the client.  It's a
   * much better practice to use partialUpdate for non-trivial comet
   * components.
   *
   * @param sendAll If `true`, the fixed part of the comet will also be
   *        re-rendered.
   */
  def reRender(sendAll: Boolean) {
    this ! ReRender(sendAll)
  }

  /**
   * Synonym for `reRender(false)`, which re-renders everything except the fixed
   * part of the comet's contents.
   */
  def reRender() {
    reRender(false)
  }

  private def performReRender(sendAll: Boolean): RenderOut = {
    if (! partialUpdateStream_?) {
      _lastRenderTime = Helpers.nextNum
    }

    if (sendAll) {
      cachedFixedRender.reset
    }

    if (sendAll || !cacheFixedRender) {
      clearWiringDependencies()
    }

    wasLastFullRender = sendAll & hasOuter
    if (! partialUpdateStream_?) {
      deltas = Nil
    }
    receivedDelta = false

    if (!dontCacheRendering) {
      lastRendering = render
    }

    theSession.updateFunctionMap(S.functionMap, uniqueId, lastRenderTime)

    val out = lastRendering

    val rendered: AnswerRender =
      AnswerRender(new XmlOrJsCmd(spanId, out, buildSpan _, notices.toList),
        this, lastRenderTime, sendAll)

    clearNotices
    listeners.foreach(_._2(rendered))
    listeners = Nil

    out
  }

  /**
   * Perform a partial update of the comet component based on the `JsCmd`.  This
   * means that the `JsCmd` will be sent to all of the currently listening
   * browser tabs, and should represent a change in the state that was rendered
   by `render` to some new state.
   *
   * Rerunning `render` after a partial update has been sent should produce the
   * same content that the client will have after applying the given `cmd`. This
   * is the preferred method over `reRender` to update the component, as it is
   * more efficient.
   *
   * For a more formal state and partial update representation, see
   * `[[StatefulComet]]`.
   */
  protected def partialUpdate(cmd: =>JsCmd): Unit = {
    pushMessage(cmd)
  }

  /**
   * A helpful implicit conversion that takes a `(NodeSeq)=>NodeSeq`
   * (for example a `[[CssSel]]`) and converts it to a `Box[NodeSeq]` by
   * applying the function to `[[defaultHtml]]`.
   */
  protected implicit def nodeSeqFuncToBoxNodeSeq(f: (NodeSeq)=>NodeSeq): Box[NodeSeq] = {
    Full(f(defaultHtml))
  }

  /**
   * Convert a `(NodeSeq)=>NodeSeq` to a `[[RenderOut]]`.
   *
   * The `[[render]]` method returns a `RenderOut`.  This method implicitly (in
   * Scala) or explicitly (in Java) will convert a `(NodeSeq)=>NodeSeq` to a
   * RenderOut, which is helpful if you use Lift's CSS Selector Transforms to
   * define rendering.
   */
  protected implicit def nsToNsFuncToRenderOut(f: NodeSeq => NodeSeq) = {
    val additionalJs =
      if (autoIncludeJsonCode) {
        Full(jsonToIncludeInCode)
      } else {
        Empty
      }

    new RenderOut((Box !! defaultHtml).map(f), internalFixedRender, additionalJs, Empty, false)
  }

  /**
   * Convert a `Seq[Node]` (the superclass of `NodeSeq`) to a `[[RenderOut]]`.
   *
   * The `render` method returns a `RenderOut`.  This method implicitly (in
   * Scala) or explicitly (in Java) will convert a `NodeSeq` to a `RenderOut`,
   * which is helpful if you produce a `NodeSeq` in your render method.
   */
  protected implicit def arrayToRenderOut(in: Seq[Node]): RenderOut = {
    val additionalJs =
      if (autoIncludeJsonCode) {
        Full(jsonToIncludeInCode)
      } else {
        Empty
      }

      new RenderOut(Full(in: NodeSeq), internalFixedRender, additionalJs, Empty, false)
  }

  /**
   * Convert a `JsCmd` to a `[[RenderOut]]`.
   *
   * The `render` method returns a `RenderOut`.  This method implicitly (in
   * Scala) or explicitly (in Java) will convert a `JsCmd` to a `RenderOut`,
   * which is helpful if you produce JavaScript in the form of a `JsCmd` in your
   * render method.
   */
  protected implicit def jsToXmlOrJsCmd(in: JsCmd): RenderOut = {
    val additionalJs =
      if (autoIncludeJsonCode) {
        Full(in & jsonToIncludeInCode)
      } else {
        Full(in)
      }

    new RenderOut(Empty, internalFixedRender, additionalJs, Empty, false)
  }

  implicit def pairToPair(in: (String, Any)): (String, NodeSeq) = (in._1, Text(in._2 match {
    case null => "null"
    case s => s.toString
  }))

  implicit def nodeSeqToFull(in: NodeSeq): Box[NodeSeq] = Full(in)

  implicit def elemToFull(in: Elem): Box[NodeSeq] = Full(in)
}

trait MessagingCometActor extends BaseCometActor {
  override final private[http] def partialUpdateStream_? = true

  override final def render = NodeSeq.Empty

  // Temporary placeholder until we can split out `BaseCometActor` stuff
  // properly.
  def cometRenderTimeout: Long = LiftRules.cometRenderTimeout

  protected def pushMessage(cmd: =>JsCmd) {
    partialUpdate(cmd)
  }
}

/**
 * Subclass from this class if you're in Java-land and want a `[[CometActor]]`.
 */
abstract class CometActorJ extends LiftActorJ with CometActor {
  override def lowPriority = _messageHandler
}

/**
 * Subclass from this class if you want a `[[CometActorJ]]` with
 * `[[CometListener]]` functionality.
 */
abstract class CometActorJWithCometListener extends CometActorJ with CometListener {
  override def lowPriority = _messageHandler
}

abstract class Delta(val when: Long) {
  def js: JsCmd

  val timestamp = millis
}

case class JsDelta(override val when: Long, js: JsCmd) extends Delta(when)

sealed abstract class CometMessage

/**
 * Impersonates the actual comet response content
 */
private[http] class XmlOrJsCmd(val id: String,
                               _xml: Box[NodeSeq],
                               _fixedXhtml: Box[NodeSeq],
                               val javaScript: Box[JsCmd],
                               val destroy: Box[JsCmd],
                               spanFunc: (NodeSeq) => NodeSeq,
                               ignoreHtmlOnJs: Boolean,
                               notices: List[(NoticeType.Value, NodeSeq, Box[String])]) {
  def this(id: String, ro: RenderOut, spanFunc: (NodeSeq) => NodeSeq, notices: List[(NoticeType.Value, NodeSeq, Box[String])]) =
    this (id, ro.xhtml, ro.fixedXhtml, ro.script, ro.destroyScript, spanFunc, ro.ignoreHtmlOnJs, notices)

  val xml = _xml.flatMap(content => S.session.map(s => s.processSurroundAndInclude("JS SetHTML id: " + id, content)))
  val fixedXhtml = _fixedXhtml.flatMap(content => S.session.map(s => s.processSurroundAndInclude("JS SetHTML id: " + id, content)))

  /**
   * Returns the JsCmd that will be sent to client
   */
  def toJavaScript(session: LiftSession, displayAll: Boolean): JsCmd = {
    val updateJs =
      (if (ignoreHtmlOnJs) Empty else xml, javaScript, displayAll) match {
        case (Full(xml), Full(js), false) => LiftRules.jsArtifacts.setHtml(id, Helpers.stripHead(xml)) & JsCmds.JsTry(js, false)
        case (Full(xml), _, false) => LiftRules.jsArtifacts.setHtml(id, Helpers.stripHead(xml))
        case (Full(xml), Full(js), true) => LiftRules.jsArtifacts.setHtml(id + "_outer", (
          spanFunc(Helpers.stripHead(xml)) ++ fixedXhtml.openOr(Text("")))) & JsCmds.JsTry(js, false)
        case (Full(xml), _, true) => LiftRules.jsArtifacts.setHtml(id + "_outer", (
          spanFunc(Helpers.stripHead(xml)) ++ fixedXhtml.openOr(Text(""))))
        case (_, Full(js), _) => js
        case _ => JsCmds.Noop
      }
    val fullUpdateJs =
      LiftRules.cometUpdateExceptionHandler.vend.foldLeft(updateJs) { (commands, catchHandler) =>
        JsCmds.Run(
          "try{" +
            commands.toJsCmd +
          "}catch(e){" +
            catchHandler.toJsCmd +
          "}"
        )
      }

    var ret: JsCmd = JsCmds.JsTry(JsCmds.Run("destroy_" + id + "();"), false) &
       fullUpdateJs &
       JsCmds.JsTry(JsCmds.Run("destroy_" + id + " = function() {" + (destroy.openOr(JsCmds.Noop).toJsCmd) + "};"), false)

    S.appendNotices(notices)
    ret = S.noticesToJsCmd & ret
    ret
  }

  def inSpan: NodeSeq = xml.openOr(Text("")) ++ javaScript.map(s => Script(s)).openOr(Text(""))

  def outSpan: NodeSeq = Script(Run("var destroy_" + id + " = function() {" + (destroy.openOr(JsCmds.Noop).toJsCmd) + "}")) ++
    fixedXhtml.openOr(Text(""))
}

/**
 * Update the comet XML on each page reload in dev mode
 */
case class UpdateDefaultHtml(html: NodeSeq) extends CometMessage

case class PartialUpdateMsg(cmd: () => JsCmd) extends CometMessage

case object AskRender extends CometMessage

case class AnswerRender(response: XmlOrJsCmd, who: LiftCometActor, when: Long, displayAll: Boolean) extends CometMessage

case class PerformSetupComet2(initialReq: Box[Req]) extends CometMessage

case object ShutdownIfPastLifespan extends CometMessage

case class AskQuestion(what: Any, who: LiftCometActor, listeners: List[(ListenerId, AnswerRender => Unit)]) extends CometMessage

case class AnswerQuestion(what: Any, listeners: List[(ListenerId, AnswerRender => Unit)]) extends CometMessage

case class Listen(when: Long, uniqueId: ListenerId, action: AnswerRender => Unit) extends CometMessage

case class Unlisten(uniqueId: ListenerId) extends CometMessage

case class ActionMessageSet(msg: List[() => Any], req: Req) extends CometMessage

case class ReRender(doAll: Boolean) extends CometMessage

case class ListenerId(id: Long)

case class Error(id: Box[String], msg: NodeSeq) extends CometMessage

case class Warning(id: Box[String], msg: NodeSeq) extends CometMessage

case class Notice(id: Box[String], msg: NodeSeq) extends CometMessage

case object ClearNotices extends CometMessage

case class SetDeltaPruner(f: (LiftCometActor, List[Delta]) => List[Delta]) extends CometMessage

object Error {
  def apply(node: NodeSeq): Error = Error(Empty, node)

  def apply(node: String): Error = Error(Empty, Text(node))

  def apply(id: String, node: String): Error = Error(Full(id), Text(node))

  def apply(id: String, node: NodeSeq): Error = Error(Full(id), node)
}

object Warning {
  def apply(node: NodeSeq): Warning = Warning(Empty, node)

  def apply(node: String): Warning = Warning(Empty, Text(node))

  def apply(id: String, node: String): Warning = Warning(Full(id), Text(node))

  def apply(id: String, node: NodeSeq): Warning = Warning(Full(id), node)
}

object Notice {
  def apply(node: NodeSeq): Notice = Notice(Empty, node)

  def apply(node: String): Notice = Notice(Empty, Text(node))

  def apply(id: String, node: String): Notice = Notice(Full(id), Text(node))

  def apply(id: String, node: NodeSeq): Notice = Notice(Full(id), node)
}

/**
 * The `RenderOut` case class encapsulates the data rendered for a
 * `[[RenderingCometActor]]`'s rendering.
 * Thanks to implicit conversions, a `RenderOut` in a comet can come from:
 *  - `[[NodeSeq]]` or `[[Elem]]`
 *  - `[[JsCmd]]`
 *  - `(NodeSeq)=>NodeSeq` or `[[CssSel]]`
 * 
 * @param html The base HTML to display.
 * @param fixedXhtml The "fixed" part of the body.  This is ignored unless
 *        `[[reRender]]` is called with `true` as its first parameter.
 * @param script Any JavaScript to be executed on render.
 * @param destroyScript Executed when the comet widget is redrawn; useful to
 *        clean up after setup that `script` does (e.g., if you register drag or
 *        mouse-over or some events, you unregister them here so the page
 *        doesn't leak resources.)
 * @param ignoreHtmlOnJs If the reason for sending the render is a Comet update,
 *        ignore the xhtml part and just run the JS commands.  This is useful
 *        in IE when you need to redraw the stuff inside `<table><tr><td>...`,
 *        since just using `innerHtml` on `<tr>` is broken in IE.
 */
case class RenderOut(xhtml: Box[NodeSeq], fixedXhtml: Box[NodeSeq], script: Box[JsCmd], destroyScript: Box[JsCmd], ignoreHtmlOnJs: Boolean) {
  def this(xhtml: NodeSeq) = this (Full(xhtml), Empty, Empty, Empty, false)

  def this(xhtml: NodeSeq, js: JsCmd) = this (Full(xhtml), Empty, Full(js), Empty, false)

  def this(xhtml: NodeSeq, js: JsCmd, destroy: JsCmd) = this (Full(xhtml), Empty, Full(js), Full(destroy), false)

  def ++(cmd: JsCmd) =
    RenderOut(xhtml, fixedXhtml, script.map(_ & cmd) or Full(cmd),
      destroyScript, ignoreHtmlOnJs)
}

private[http] object Never extends Serializable

