/*
 * Copyright 2010-2011 WorldWide Conferencing, LLC
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
package mongodb
package record
package field

import net.liftweb.common.{Box, Empty, Failure, Full}
import net.liftweb.http.js.JE.Str
import net.liftweb.json.JsonAST.{JNothing, JObject, JValue}
import net.liftweb.record.{Field, MandatoryTypedField, Record}
import scala.xml.NodeSeq

import com.mongodb.{BasicDBObject, BasicDBObjectBuilder, DBObject, DBRef}
import com.mongodb.util.JSON
import org.bson.types.ObjectId
import org.bson.BSONObject

/*
* Field for storing a DBRef
*/
class DBRefField[OwnerType <: BsonRecord[OwnerType], RefType <: MongoRecord[RefType]](rec: OwnerType, ref: RefType)
  extends Field[DBRef, OwnerType] with MandatoryTypedField[DBRef] {

  /*
  * get the referenced object
  */
  def obj = synchronized {
    if (!_calcedObj) {
      _calcedObj = true
      this._obj = ref.meta.findAny(value.getId)
    }
    _obj
  }

  def cached_? : Boolean = synchronized { _calcedObj }

  def primeObj(obj: Box[RefType]) = synchronized {
    _obj = obj
    _calcedObj = true
  }

  private var _obj: Box[RefType] = Empty
  private var _calcedObj = false

  def asJs = Str(toString)

  def asJValue = (JNothing: JValue) // not implemented

  def setFromJValue(jvalue: JValue) = Empty // not implemented

  def asXHtml = <div></div>

  def defaultValue = new DBRef(null, null, null)

  def setFromAny(in: Any): Box[DBRef] = in match {
    case ref: DBRef => Full(set(ref))
    case Some(ref: DBRef) => Full(set(ref))
    case Full(ref: DBRef) => Full(set(ref))
    case seq: Seq[_] if !seq.isEmpty => seq.map(setFromAny).apply(0)
    case (s: String) :: _ => setFromString(s)
    case null => Full(set(null))
    case s: String => setFromString(s)
    case None | Empty | Failure(_, _, _) => Full(set(null))
    /**
     * BWM - 8/19/11
     * In some cases such as Lazy decoding MongoDB may return an
     * instance of BSONObject instead of a concrete DBRef for performance reasons.
     *
     * If we didn't catch an actual DBRef above, but find a BSONObject, attempt to extract
     * the reference related information from the reference to instantiate a new DBRef
     * ( future releases of the java driver are expected to return DBRefs but this is
     * a good safe fallback which performs better than serializing to JSON )
     */
    case dbo: BSONObject if dbo.containsField("$ref") && dbo.containsField("$id") =>
      setRawRef(dbo.get("$ref").asInstanceOf[String], dbo.get("$id"))
    case o => setFromString(o.toString) // TODO - Consider using com.mongodb.util.JSON.serialize instead of assuming toString creates JSON
  }

  /**
   * Create and set a reference from 'raw' DBRef
   * info that came either from JSON or a basic nested
   * DBObject (as Lazy mode may produce)
   *
   * Caller is expected to have already converted the $id
   * field to a valid type (such as ObjectId if appropriate)
   *
   * TODO - Shouldn't we be running an invariant check that
   * $ref points to the collection we expect it to?!
   */
  def setRawRef(refTo: String, id: AnyRef): Box[DBRef] =
    MongoDB.use(ref.meta.mongoIdentifier) { db =>
      Full(set(new DBRef(db, refTo, id)))
    }


  // assume string is json
  def setFromString(in: String): Box[DBRef] = {
    /* BWM - 8/19/11
     * We shouldn't ever assume things are BasicDBObject, which is
     * a very specific concrete instance of DBObject. Treat it instead
     * as a generic interface of BSONObject (since we don't need
     * the DBObject partialObject hooks)
     *
     * It is entirely possible to get something other than BasicDBObject
     * back which would cause this to blow up at runtime.
     */
    val dbo = JSON.parse(in).asInstanceOf[BSONObject]
    MongoDB.use(ref.meta.mongoIdentifier) ( db => {
      val id = dbo.get("$id").toString
      ObjectId.isValid(id) match {
        case true => setRawRef(dbo.get("$ref").toString, new ObjectId(id))
        case false => setRawRef(dbo.get("$ref").toString, id)
      }
    })
  }

  def toForm: Box[NodeSeq] = Empty

  def owner = rec
}

