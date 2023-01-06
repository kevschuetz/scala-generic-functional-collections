package coll

trait Builder[A, +C[+A]] {
  def collect(elem: A) : Unit
  def build : C[A]
}

trait FIterable[+E, +C[+X]] {
  /**
   * Returns true if the collection is empty, false otherwise.
   *
   * @return true if the collection is empty, false otherwise
   */
  def isEmpty: Boolean

  /**
   * Adds an element to the collection.
   *
   * @param elem the element to add
   * @tparam U the type of the element to add
   * @return a new collection containing the added element
   */
  def add[U >: E](elem: U): C[U]

  /**
   * Applies a function to each element in the collection.
   *
   * @param action the function to apply to each element
   */
  def foreach(action: E => Unit): Unit

  /**
   * Creates a new `Builder` object for collecting elements and creating the result collection.
   *
   * @tparam Y the type of the elements that will be collected
   * @return a new `Builder` object
   */
  protected[this] def newBuilder[Y]: Builder[Y, C]

  def map[R](f: E => R): C[R] = {
    val builder = newBuilder[R]
    foreach(E => builder.collect(f.apply(E)))
    builder.build
  }

  def filter(p: E => Boolean) : C[E] = {
    val builder = newBuilder[E]
    foreach(E => {
      if(p.apply(E))
        builder.collect(E)
    })
    builder.build
  }

  def fold[R](z: R)(acc: (R, E) => R): R = {
    var result = z
    foreach(E => result = acc.apply(result, E))
    result
  }

  def sum(f: E => Int) : Int = fold(0)((R,E) => R + f.apply(E))


  def count : Int = fold(0)((R,E) => R+1)

  def toString(sep: String = ", ", pre: String = "", post: String = "",
               toString: E => String = e => e.toString): String = fold(pre)((R,E) => R + (if(R.equals(pre)) "" else sep) + toString(E)) + post
}