package net.liftweb.mongodb


import java.util.concurrent.ConcurrentHashMap

import com.mongodb.MongoException
import com.mongodb.async.client.{MongoClient, MongoCollection, MongoDatabase}
import com.mongodb.async.SingleResultCallback
import net.liftweb.util.ConnectionIdentifier
import org.bson.Document

import scala.concurrent.Promise


private[mongodb] class SingleBooleanVoidCallback(f: () => Unit) extends SingleResultCallback[Void] {
  private[this] val p = Promise[Boolean]()
  override def onResult(result: java.lang.Void, t: Throwable): Unit = {
    if (t == null) {
      f()
      p.success(true)
    }
    else p.failure(t)
  }
  def future = p.future
}
/*
  Async version of MongoClient
 */
object MongoAsync {

  /*
  * HashMap of Mongo instance and db name tuples, keyed by ConnectionIdentifier
  */
  private val dbs = new ConcurrentHashMap[ConnectionIdentifier, (MongoClient, String)]

  /**
   * Define a Mongo db using a MongoClient instance.
   */
  def defineDb(name: ConnectionIdentifier, mngo: MongoClient, dbName: String) {
    dbs.put(name, (mngo, dbName))
  }

  /*
  * Get a DB reference
  */
  def getDb(name: ConnectionIdentifier): Option[MongoDatabase] = dbs.get(name) match {
    case null => None
    case (mngo, db) => Some(mngo.getDatabase(db).withCodecRegistry(com.mongodb.MongoClient.getDefaultCodecRegistry))  }


  def useSession[T](ci: ConnectionIdentifier)(f: (MongoDatabase) => T): T = {
    val db = getDb(ci) match {
      case Some(mongo) => mongo
      case _ => throw new MongoException("Mongo not found: "+ci.toString)
    }
    f(db)
  }

  /**
   * Executes function {@code f} with the mongo named {@code name} and collection names {@code collectionName}.
   * Gets a collection for you.
   */
  def useCollection[T](name: ConnectionIdentifier, collectionName: String)(f: (MongoCollection[Document]) => T): T = {
    val coll = getCollection(name, collectionName) match {
      case Some(collection) => collection
      case _ => throw new MongoException("Mongo not found: "+collectionName+". ConnectionIdentifier: "+name.toString)
    }

    f(coll)
  }

  def closeAll(): Unit = {
        import scala.collection.JavaConversions._
        dbs.values.foreach { case (mngo, _) =>
            mngo.close()
          }
        dbs.clear()
      }

  /*
* Get a Mongo collection. Gets a Mongo db first.
*/
  private def getCollection(name: ConnectionIdentifier, collectionName: String): Option[MongoCollection[Document]] = getDb(name) match {
    case Some(mongo) if mongo != null => Some(mongo.getCollection(collectionName))
    case _ => None
  }

}