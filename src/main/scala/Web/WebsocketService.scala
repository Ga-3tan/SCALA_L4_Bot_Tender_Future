package Web

import Data.MessageService

import scala.collection.mutable

trait WebsocketService :
  val websockets: mutable.ListBuffer[cask.WsChannelActor] = mutable.ListBuffer()

  /**
    * Add one websocket
    */
  def add(websocket: cask.WsChannelActor): Unit

  /**
    * Remove the specified websocket
    */
  def remove(websocket: cask.WsChannelActor): Unit

  /**
    * Send the last 20 messages to all connected websockets
    */
  def sendMessagesToAll(messages: Seq[(MessageService.Username, MessageService.MsgContent)]): Unit

class WebsocketImpl extends WebsocketService :

  override def sendMessagesToAll(messages: Seq[(MessageService.Username, MessageService.MsgContent)]): Unit =
    websockets.foreach(
      ws => ws.send(cask.Ws.Text(
        Layouts.getBoardMessageContent(messages).foldLeft("")(_ + _))
      )
    )

  override def add(websocket: cask.WsChannelActor): Unit =
    websockets.addOne(websocket)

  override def remove(websocket: cask.WsChannelActor): Unit =
    websockets -= websocket
