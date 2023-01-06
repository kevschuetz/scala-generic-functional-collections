package coll
import scala.collection.mutable.ListBuffer

sealed trait FList[+E] extends FIterable[E, FList] {

}

object FList{
  /**
   * Represents the empty list.
   */
  case object FNil extends FList[Nothing] {
    override def isEmpty: Boolean = true
    override def add[U >: Nothing](elem: U): FList[U] = FCons(elem, this)
    override def foreach(action: Nothing => Unit): Unit = ()
    override protected[this] def newBuilder[Y]: Builder[Y, FList] = FListBuilder()
  }

  case class FCons[+E](head: E, tail: FList[E]) extends FList[E] {
    override def isEmpty: Boolean = false

    override def add[U >: E](elem: U): FList[U] = FCons(elem, this)

    override def foreach(action: E => Unit): Unit = {
      action(head)
      tail.foreach(action)
    }

    override protected[this] def newBuilder[Y]: Builder[Y, FList] = FListBuilder()
  }

  class FListBuilder[A] extends Builder[A, FList] {
    private val elements = ListBuffer.empty[A]

    override def collect(elem: A ): Unit = elements += elem

    override def build: FList[A] = {
      // Reverse the elements in the ListBuffer
      val reversedElements = elements.reverse

      // Use add to build an FList from the elements in the ListBuffer
      var flist: FList[A] = FList.FNil
      reversedElements.foreach(elem => flist = flist.add(elem))
      flist
    }
  }


}





