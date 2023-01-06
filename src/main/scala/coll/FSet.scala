package coll

import collection.mutable.ListBuffer

trait FSet[+E] extends FIterable[E, FSet] {
  import HashTree.*

  val tree: HashTree[E]

  /**
   * Returns true if the collection is empty, false otherwise.
   *
   * @return true if the collection is empty, false otherwise
   */
  override def isEmpty: Boolean = tree match {
    case HashTree.Empty => true
    case _ => false
  }

  /**
   * Adds an element to the collection.
   *
   * @param elem the element to add
   * @tparam U the type of the element to add
   * @return a new collection containing the added element
   */
  def add[U >: E](elem: U): FSet[U] = {
    val newTree = tree.add(elem)
    if(newTree != tree){
      new FSet[U]{val tree: HashTree[U] = newTree}
    }else{
      this
    }
  }

  /**
   * Applies a function to each element in the collection.
   *
   * @param action the function to apply to each element
   */
  def foreach(action: E => Unit): Unit = tree match {
    case n : HashTree.Node[E] => {
      def applyRecursive(n: HashTree.Node[E]): Unit = {
        n.values.foreach(action)
        n.left match{
          case l: HashTree.Node[E] => applyRecursive(l)
          case _ => ()
        }
        n.right match{
          case r: HashTree.Node[E] => applyRecursive(r)
          case _ => ()
        }
      }
      applyRecursive(n)
    }
    case _ => ()
  }

  /**
   * Creates a new `Builder` object for collecting elements and creating the result collection.
   *
   * @tparam Y the type of the elements that will be collected
   * @return a new `Builder` object
   */
  protected[this] def newBuilder[Y]: Builder[Y, FSet] =
    new Builder[Y, FSet]{
      private val elements = ListBuffer.empty[Y]

      override def collect(elem: Y): Unit = elements += elem

      override def build: FSet[Y] = {
        var hashTree: HashTree[Y] = HashTree.Empty
        elements.foreach(elem => hashTree = hashTree.add(elem))
        new FSet[Y]{val tree: HashTree[Y] = hashTree} 
      }
    }
}



