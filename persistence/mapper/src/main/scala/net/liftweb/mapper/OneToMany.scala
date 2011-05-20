/*
 * Copyright 2006-2011 WorldWide Conferencing, LLC
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
package mapper


/**
 * Add this trait to a Mapper for managed one-to-many support
 * For example: class Contact extends LongKeyedMapper[Contact] with OneToMany[Long, Contact] { ... }
 * @tparam K the type of the primary key
 * @tparam T the mapper type
 * @author nafg
 */
trait OneToMany[K,T<:KeyedMapper[K, T]] extends KeyedMapper[K,T] { this: T =>
  //private var oneToManyFields: List[MappedOneToManyBase[_]] = Nil
  private[mapper] lazy val oneToManyFields: List[MappedOneToManyBase[_]] = {
    new FieldFinder[T, MappedOneToManyBase[_]](
      getSingleton,
      net.liftweb.common.Logger(classOf[OneToMany[K,T]])
    ).accessorMethods map (_.invoke(this).asInstanceOf[MappedOneToManyBase[_]])
  }
  
  /**
   * An override for save to propagate the save to all children
   * of this parent.
   * Returns false as soon as the parent or a one-to-many field returns false.
   * If they are all successful returns true.
   */
  override def save = {
    val ret = super.save &&
      oneToManyFields.forall(_.save)
    ret
  }

  /**
   * An override for delete_! to propagate the deletion
   * to all children of one-to-many fields implementing Cascade.
   * Returns false as soon as the parent or a one-to-many field returns false.
   * If they are all successful returns true.
   */
  override def delete_! = {
    super.delete_! &&
      oneToManyFields.forall( _ match {
        case f: Cascade[_] => f.delete_!
        case _ => true
      } )
  }


  /**
   * This implicit allows a MappedForeignKey to be used as foreignKey function.
   * Returns a function that takes a Mapper and looks up the actualField of field on the Mapper.
   */
  implicit def foreignKey[K, O<:Mapper[O], T<:KeyedMapper[K,T]](field: MappedForeignKey[K,O,T]): O=>MappedForeignKey[K,O,T] =
    field.actualField(_).asInstanceOf[MappedForeignKey[K,O,T]]

  /**
   * Simple OneToMany support for children from the same table
   */
  class MappedOneToMany[O <: Mapper[O]](meta: MetaMapper[O], foreign: MappedForeignKey[K,O,T], qp: QueryParam[O]*)
    extends MappedOneToManyBase[O](
      ()=>{
        val ret = meta.findAll(By(foreign, primaryKeyField) :: qp.toList : _*)
        for(child <- ret) {
          foreign.actualField(child).asInstanceOf[MappedForeignKey[K,O,T]].primeObj(net.liftweb.common.Full(OneToMany.this : T))
        }
        ret
      },
      foreign
    )

  /**
   * This is the base class to use for fields that represent one-to-many or parent-child relationships.
   * Maintains a list of children, tracking pending additions and deletions, and
   * keeping their foreign key pointed to this mapper.
   * Implements Buffer, so the children can be managed as one.
   * Most users will use MappedOneToMany, however to support children from multiple tables
   * it is necessary to use MappedOneToManyBase.
   * @param reloadFunc A function that returns a sequence of children from storage.
   * @param foreign A function that gets the MappedForeignKey on the child that refers to this parent
   */
  class MappedOneToManyBase[O <: Mapper[_]](val reloadFunc: ()=>Seq[O],
                                      val foreign: O => MappedForeignKey[K,_,T]) extends scala.collection.mutable.Buffer[O] {
    private var inited = false
    private var _delegate: List[O] = _
    /**
     * children that were added before the parent was ever saved
    */
    private var unlinked: List[O] = Nil
    protected def delegate: List[O] = {
      if(!inited) {
        refresh
        inited = true
      }
      _delegate
    }
    protected def delegate_=(d: List[O]) = _delegate = d

    //oneToManyFields = this :: oneToManyFields

    /**
     * Takes ownership of e. Sets e's foreign key to our primary key
     */
    protected def own(e: O) = {
      foreign(e) match {
        case f: MappedLongForeignKey[O,T] with MappedForeignKey[_,_,T] =>
          f.apply(OneToMany.this)
        case f =>
          f.set(OneToMany.this.primaryKeyField)
      }
      if(!OneToMany.this.saved_?)
         unlinked ::= e
      e
    }
    /**
     * Relinquishes ownership of e. Resets e's foreign key to its default value.
     */
    protected def unown(e: O) = {
      val f = foreign(e)
      f.set(f.defaultValue)
      unlinked = unlinked filter {e.ne}
      e
    }
    /**
     * Returns the backing List
     */
    def all = delegate

    // 2.8: return this
    def +=(elem: O) = {
      delegate = delegate ++ List(own(elem))
      this
    }
    // 2.7
    //def readOnly = all
    def length = delegate.length
    // 2.7
    //def elements = delegate.elements
    // 2.8
    def iterator = delegate.iterator
    def apply(n: Int) = delegate(n)

    // 2.7
    /* def +:(elem: O) = {
      delegate ::= own(elem)
      this
    } */
    // 2.8
    def +=:(elem: O) = {
      delegate ::= own(elem)
      this
    }

    override def indexOf[B >: O](e: B): Int =
      delegate.indexWhere(e.asInstanceOf[AnyRef].eq)

    // 2.7
    // def insertAll(n: Int, iter: Iterable[O]) {
    // 2.8
    def insertAll(n: Int, iter: Traversable[O]) {
      val (before, after) = delegate.splitAt(n)
      iter foreach own
      delegate = before ++ iter ++ after
    }

    def update(n: Int, newelem: O) {
      unown(delegate(n))
      val (before, after) = (delegate.take(n), delegate.drop(n+1))
      delegate = before ++ List(own(newelem)) ++ after
    }

    def remove(n: Int) = {
      val e = unown(delegate(n))
      delegate = delegate.filterNot(e eq)
      e
    }


    def clear() {
      while(delegate.length>0)
        remove(0)
    }

    /**
     * Reloads the children from storage.
     * NOTE: This may leave children in an inconsistent state.
     * It is recommended to call save or clear() before calling refresh.
     */
    def refresh {
      delegate = reloadFunc().toList
      if(saved_?)
        unlinked = Nil
      else
        unlinked = _delegate
    }

    /**
     * Saves this "field," i.e., all the children it represents.
     * Returns false as soon as save on a child returns false.
     * Returns true if all children were saved successfully.
     */
    def save = {
      unlinked foreach {u =>
        val f = foreign(u)
        if(f.obj.map(_ eq OneToMany.this) openOr true) // obj is Empty or this
          f.set(OneToMany.this.primaryKeyField.is)
      }
      unlinked = Nil
      delegate = delegate.filter {e =>
          foreign(e).is == OneToMany.this.primaryKeyField.is ||
            foreign(e).obj.map(_ eq OneToMany.this).openOr(false) // obj is this but not Empty
      }
      delegate.forall(_.save)
    }

    override def toString = {
      val c = getClass.getSimpleName
      val l = c.lastIndexOf("$")
      c.substring(c.lastIndexOf("$",l-1)+1, l) + delegate.mkString("[",", ","]")
    }
  }

  /**
   * Adds behavior to delete orphaned fields before save.
   */
  trait Owned[O<:Mapper[_]] extends MappedOneToManyBase[O] {
    var removed: List[O] = Nil
    override def unown(e: O) = {
      removed = e :: removed
      super.unown(e)
    }
    override def own(e: O) = {
      removed = removed filter {e ne}
      super.own(e)
    }
    override def save = {
      val unowned = removed.filter{ e =>
        val f = foreign(e)
        f.is == f.defaultValue
      }
      unowned foreach {_.delete_!}
      super.save
    }
  }

  /**
   * Trait that indicates that the children represented
   * by this field should be deleted when the parent is deleted.
   */
  trait Cascade[O<:Mapper[_]] extends MappedOneToManyBase[O] {
    def delete_! = {
      delegate.forall { e =>
          if(foreign(e).is ==
            OneToMany.this.primaryKeyField.is) {
              e.delete_!
            }
          else
            true // doesn't constitute a failure
      }
    }
  }
}



/**
 * A subclass of MappedLongForeignKey whose value can be
 * get and set as the target parent mapper instead of as its primary key.
 * @author nafg
 */
class LongMappedMapper[T<:Mapper[T], O<:KeyedMapper[Long,O]](theOwner: T, foreign: => KeyedMetaMapper[Long, O])
  extends MappedLongForeignKey[T,O](theOwner, foreign) with LongMappedForeignMapper[T,O]


/**
 * A subtype of MappedLongForeignKey whose value can be
 * get and set as the target parent mapper instead of as its primary key.
 * @deprecated Use LongMappedMapper instead, to avoid mixing in MappedLongForeignKey manually. May be folded into it in the future.
 * @author nafg
 */
//TODO fold into LongMappedMapper
@deprecated("Use LongMappedMapper or LongMappedForeignLey instead")
trait LongMappedForeignMapper[T<:Mapper[T],O<:KeyedMapper[Long,O]]
                              extends MappedLongForeignKey[T,O]
                              with LifecycleCallbacks {
  import net.liftweb.common.{Box, Empty, Full}

  override def apply(f: O) = {
    this(Full(f))
  }
  override def apply(f: Box[O]) = {
    val ret = super.apply(f)
    primeObj(f)
    ret
  }

  override def set(v: Long) = {
    val ret = super.set(v)
    primeObj(if(defined_?) dbKeyToTable.find(i_is_!) else Empty)
    ret
  }

  override def beforeSave {
    if(!defined_?)
      for(o <- obj)
        set(o.primaryKeyField.is)
    super.beforeSave
  }

  import net.liftweb.util.FieldError
  val valHasObj = (value: Long) =>
    if (obj eq Empty) List(FieldError(this, scala.xml.Text("Required field: " + name)))
    else Nil
}

class FieldFinder[A <: Mapper[A], T: ClassManifest](metaMapper: Mapper[A], logger: net.liftweb.common.Logger) {
    import java.lang.reflect._
      
    logger.debug("Created FieldFinder for " + classManifest[T].erasure)
      
    def isMagicObject(m: Method) = m.getReturnType.getName.endsWith("$"+m.getName+"$") && m.getParameterTypes.length == 0
    
    def typeFilter: Class[_]=>Boolean = classManifest[T].erasure.isAssignableFrom

    /**
     * Find the magic mapper fields on the superclass
     */
    def findMagicFields(onMagic: Mapper[A], staringClass: Class[_]): List[Method] = {
      // If a class name ends in $module, it's a subclass created for scala object instances
      def deMod(in: String): String =
        if (in.endsWith("$module")) in.substring(0, in.length - 7)
        else in

      // find the magic fields for the given superclass
      def findForClass(clz: Class[_]): List[Method] = clz match {
        case null => Nil
        case c =>
          // get the names of fields that represent the type we want

          val fields = Map(c.getDeclaredFields.
                           filter{f =>
                              val ret = typeFilter(f.getType)
                              logger.debug("typeFilter(" + f.getType + "); T=" + classManifest[T].erasure)
                              ret
                           }.
                           map(f => (deMod(f.getName), f)) :_*)
                           
          logger.debug("fields: " + fields)

          // this method will find all the super classes and super-interfaces
          def getAllSupers(clz: Class[_]): List[Class[_]] = clz match {
            case null => Nil
            case c =>
              c :: c.getInterfaces.toList.flatMap(getAllSupers) :::
              getAllSupers(c.getSuperclass)
          }

          // does the method return an actual instance of an actual class that's
          // associated with this Mapper class
          def validActualType(meth: Method): Boolean = {
            try {
              // invoke the method
              meth.invoke(onMagic) match {
                case null =>
                  logger.debug("Not a valid mapped field: %s".format(meth.getName))
                  false
                case inst =>
                  // do we get a T of some sort back?
                  if (!typeFilter(inst.getClass)) false
                  else {
                    // find out if the class name of the actual thing starts
                    // with the name of this class or some superclass...
                    // basically, is an inner class of this class
                    getAllSupers(clz).find{
                      c =>
                      inst.getClass.getName.startsWith(c.getName)}.isDefined
                  }
              }

            } catch {
              case e =>
                logger.debug("Not a valid mapped field: %s, got exception: %s".format(meth.getName, e))
                false
            }
          }

          // find all the declared methods
          val meths = c.getDeclaredMethods.toList.
          filter(_.getParameterTypes.length == 0). // that take no parameters
          filter(m => Modifier.isPublic(m.getModifiers)). // that are public
          filter(m => fields.contains(m.getName) && // that are associated with private fields
                 fields(m.getName).getType == m.getReturnType).
          filter(validActualType) // and have a validated type  

          meths ::: findForClass(clz.getSuperclass)
      }

      findForClass(staringClass).distinct
    }

    lazy val accessorMethods = findMagicFields(metaMapper, metaMapper.getClass.getSuperclass)
  }



