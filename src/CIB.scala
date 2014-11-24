package finder
import Array._
import java.io._

class CIB(val n: Int, val t: List[List[Int]], val name: String = "A") {

  // return true if the table is complete
  def isFull = {
    if ((t.length == n - 1) && (t(n - 2).length == n - 1))
      true
    else
      false
  }

  // The binary operation
  def bin(x: Int, y: Int) = {
    if (x < y) t(y - 1)(x)
    else if (x == y) x
    else t(x - 1)(y)
  }

  // Push a new element into an incomplete operation table.
  def push(y: Int): List[List[Int]] = {
    val m = this.t.length
    val x = this.t.last
    if (x.length == m) // last column is full, start a new one
      if (m == n - 1) throw new Error("push called on full table")
      else
        this.t ::: List(List(y))
    else
      (this.t take this.t.length - 1) ::: List(x ::: List(y))
  }

  override def toString = printTable(this.t)

  def printTable(t: List[List[Int]]): String = t match {
    case List(x) => x.toString
    case x :: xs => x.toString + ", " + printTable(xs)
  }


  // Given a full table, create the corresponding symmetric array.
  // Example: this.t = (List(List(2), List(1,3), List(1,0,4), List(1,0,0,0)))
  //          ans = [0, 2, 1, 1, 1]
  //                [2, 1, 3, 0, 0]
  //                [1, 3, 2, 4, 0]
  //                [1, 0, 4, 3, 0]
  //                [1, 0, 0, 0, 4]
  // (Note: we assume a commutative idempotent binary operation.)
  def cibtable: Array[Array[Int]] = {
    if (this.isFull == false) throw new Error("cibtable requires full table")
    else {
      var ans = ofDim[Int](this.n, this.n)
      for (i <- 0 to this.n - 1) {
        ans(i)(i) = i
      }
      for (j <- 0 to this.t.length - 1) {
        for (i <- 0 to j) {
          ans(i)(j + 1) = this.t(j)(i)
          ans(j + 1)(i) = this.t(j)(i)
        }
      }
      ans
    }

  }

  // For now, just want to make sure the 0 and 1 form a 2 element meet semilattice
  def isValid = (this.t(0)(0)==0)
//  def isValid = true

  def printList(args: List[_]): Unit = {
    args.foreach(println)
  }

}
