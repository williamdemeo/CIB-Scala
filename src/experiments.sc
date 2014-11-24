import finder._
import java.io._

object test_CIB {
	val A = new CIB(5, List(List(2), List(1,3), List(1,0,4), List(1,0,0,0)))
                                                  //> A  : finder.CIB = List(2), List(1, 3), List(1, 0, 4), List(1, 0, 0, 0)
	A.isFull                                  //> res0: Boolean = true
	A.bin(0,0)                                //> res1: Int = 0
	A.bin(0,1)                                //> res2: Int = 2
	A.bin(0,2)                                //> res3: Int = 1
	A.bin(1,1)                                //> res4: Int = 1
	A.cibtable                                //> res5: Array[Array[Int]] = Array(Array(0, 2, 1, 1, 1), Array(2, 1, 3, 0, 0), 
                                                  //| Array(1, 3, 2, 4, 0), Array(1, 0, 4, 3, 0), Array(1, 0, 0, 0, 4))
	val B = new CIB(5, List(List(2)))         //> B  : finder.CIB = List(2)
	B.isFull                                  //> res6: Boolean = false
	val C = new CIB(5, B.push(1))             //> C  : finder.CIB = List(2), List(1)
	val D = new CIB(5, C.push(3))             //> D  : finder.CIB = List(2), List(1, 3)
	val E = new CIB(5, D.push(1))             //> E  : finder.CIB = List(2), List(1, 3), List(1)
	val F = new CIB(5, E.push(0))             //> F  : finder.CIB = List(2), List(1, 3), List(1, 0)
	val G = new CIB(5, F.push(4))             //> G  : finder.CIB = List(2), List(1, 3), List(1, 0, 4)
	val H = new CIB(5, G.push(1))             //> H  : finder.CIB = List(2), List(1, 3), List(1, 0, 4), List(1)
	val I = new CIB(5, H.push(0))             //> I  : finder.CIB = List(2), List(1, 3), List(1, 0, 4), List(1, 0)
	val J = new CIB(5, I.push(0))             //> J  : finder.CIB = List(2), List(1, 3), List(1, 0, 4), List(1, 0, 0)
	val K = new CIB(J.n, J.push(0))           //> K  : finder.CIB = List(2), List(1, 3), List(1, 0, 4), List(1, 0, 0, 0)
	K.cibtable                                //> res7: Array[Array[Int]] = Array(Array(0, 2, 1, 1, 1), Array(2, 1, 3, 0, 0), 
                                                  //| Array(1, 3, 2, 4, 0), Array(1, 0, 4, 3, 0), Array(1, 0, 0, 0, 4))
	//val L = new CIB(5, K.push(1))

	def concat[T](xs: List[T], ys: List[T]): List[T] = (xs foldRight ys) (_ :: _)
                                                  //> concat: [T](xs: List[T], ys: List[T])List[T]

	// get_cib(List[CIB], CIB, Int): List[CIB]
	//
	// The function get_cibs builds up the list of CIB's recursively by trying
	// to push each of the numbers {0, 1, ..., n} into each of the (upper triangle)
	// positions of the CIB.t member, which is of type List[List[Int]].
	// This requires two mutually recursive functions (otherwise, mutable lists are required).
	//
	//   INPUT
	//     AA is the list of all CIB's found so far
	//     A is the CIB we are currently building
	//     i0 is the number the current CIB has in it's first position
	//        i.e. A.cibtable(0,1) = i0
	//
	//   NOTE: We only find numbers for the upper triangle of the cibtable.
	//         We construct CIB.t as the List of Lists:
	//         CIB.t = List( List(T(0,1)),
	//                       List(T(0,2), T(1,2)),
	//                       List(T(0,3), T(1,3), T(2,3)),...)
	//         Here T is used to denote the array CIB.cibtable.
	//
  def get_cibs(AA: List[CIB], A: CIB, i0: Int): List[CIB] = {
  	if ((A.isFull) &&  (i0 == A.n))
  		AA ::: List(A)
  	else if (A.isFull)
  		get_cibs(AA ::: List(A), new CIB(A.n, List(List(i0+1))), i0+1)
  	else
  		 // Now push each k in {0, 1, ..., n-1} to A, one at a time.
  		 // That is, each spawns a new branch, so can't simply call get_cibs.
  		 get_cibs_aux(AA, A, i0, A.n)
  }                                               //> get_cibs: (AA: List[finder.CIB], A: finder.CIB, i0: Int)List[finder.CIB]
  
  
  def get_cibs_aux(AA: List[CIB], A: CIB, i0: Int, kstep: Int): List[CIB] = {
  	if (kstep==0) AA
  	else {
  		val	newA = new CIB(A.n, A.push(A.n-kstep))
  		if (newA.isValid) // pushing (n-kstep) into next position results in a valid CIB
  		  get_cibs_aux( get_cibs(AA, newA, i0),	A, i0, kstep-1)
 		  else
  		  get_cibs_aux(AA, A, i0, kstep-1)
  	}
  }                                               //> get_cibs_aux: (AA: List[finder.CIB], A: finder.CIB, i0: Int, kstep: Int)Lis
                                                  //| t[finder.CIB]
  	
	
	val mylist = get_cibs(List(), new CIB(4, List(List(0))), 0)
                                                  //> mylist  : List[finder.CIB] = List(List(0), List(0, 0), List(0, 0, 0), List(
                                                  //| 0), List(0, 0), List(0, 0, 1), List(0), List(0, 0), List(0, 0, 2), List(0),
                                                  //|  List(0, 0), List(0, 0, 3), List(0), List(0, 0), List(0, 1, 0), List(0), Li
                                                  //| st(0, 0), List(0, 1, 1), List(0), List(0, 0), List(0, 1, 2), List(0), List(
                                                  //| 0, 0), List(0, 1, 3), List(0), List(0, 0), List(0, 2, 0), List(0), List(0, 
                                                  //| 0), List(0, 2, 1), List(0), List(0, 0), List(0, 2, 2), List(0), List(0, 0),
                                                  //|  List(0, 2, 3), List(0), List(0, 0), List(0, 3, 0), List(0), List(0, 0), Li
                                                  //| st(0, 3, 1), List(0), List(0, 0), List(0, 3, 2), List(0), List(0, 0), List(
                                                  //| 0, 3, 3), List(0), List(0, 0), List(1, 0, 0), List(0), List(0, 0), List(1, 
                                                  //| 0, 1), List(0), List(0, 0), List(1, 0, 2), List(0), List(0, 0), List(1, 0, 
                                                  //| 3), List(0), List(0, 0), List(1, 1, 0), List(0), List(0, 0), List(1, 1, 1),
                                                  //|  List(0), List(0, 0), List(1, 1, 2), List(0), List(0, 0), List(1, 1, 3), Li
                                                  //| st(0), List(0, 0), List
                                                  //| Output exceeds cutoff limit.
	mylist(1).cibtable                        //> res8: Array[Array[Int]] = Array(Array(0, 0, 0, 0), Array(0, 1, 0, 0), Array
                                                  //| (0, 0, 2, 1), Array(0, 0, 1, 3))
	mylist(1).n                               //> res9: Int = 4
	for (k <- 0 to mylist.length-1){
//		println("CIB"+mylist(k).n+"-"+k+".ua\n")
//		writeUAfile(mylist(k), "CIB"+mylist(k).n+"-"+k+".ua")
	}

		
		
  def writeUAfile(A: CIB, filename: String = "~/tmp/" + A.name + ".ua") = {
  //def writeUAfile(A: CIB, filename: String) = {
    val T = A.cibtable
    val writer = new PrintWriter(new File(filename))
    writer.write("<?xml version='1.0'?>\n")
    writer.write("<algebra>\n")
    writer.write("  <basicAlgebra>\n")
    writer.write("    <algName>"+A.name+"</algName>\n")
    writer.write("    <desc>commutative idempotent binar</desc>\n")
    writer.write("    <cardinality>" + A.n + "</cardinality>\n")
    writer.write("    <operations>\n")
    writer.write("      <op>\n")
    writer.write("        <opSymbol>\n")
    writer.write("          <opName>g</opName>\n")
    writer.write("          <arity>2</arity>\n")
    writer.write("        </opSymbol>\n")
    writer.write("        <opTable>\n")
    writer.write("          <intArray>\n")
    for (i <- 0 to A.n-1) {
	    writer.write("            <row r=\"["+i+"]\">")
      for (j <- 0 to A.n-1) {
        writer.write(T(i)(j) + ", ")
      }
      writer.write("</row>\n")
    }
    writer.write("          </intArray>\n")
    writer.write("        </opTable>\n")
    writer.write("      </op>\n")
    writer.write("  </operations>\n")
    writer.write("</basicAlgebra>\n")
    writer.write("</algebra>\n")
    writer.close()
  }                                               //> writeUAfile: (A: finder.CIB, filename: String)Unit
  
  	
		
	def printarray(T: Array[Array[Int]]) = {
		for (i <- 0 to T.length-1) {
			for (j<- 0 to T(i).length-1) {
				print(" " + T(i)(j));
			}
			println();
		}
	}                                         //> printarray: (T: Array[Array[Int]])Unit


}


object experiments {
	// Take a list of lists and return the length of the longest one.
	// def maxLen[T](xs: List[List[T]]) = (xs map (x=>x.length)) reduceLeft (_ max _)
	def maxLen[T](xs: List[List[T]]) = (xs map (x=>x.length)).max

	// Take a list of lists and return the index of the shortest one
	def minLenIdx[T](xs: List[List[T]]) = xs.zipWithIndex.minBy(_._1.length)._2

	// Take a list of lists and return the length of the shortest one
	def minLen[T](xs: List[List[T]]) = (xs map(x => x.length)).min
	
	// append a to the n-th list in a list of lists
	def pushAt[T](xs: List[List[T]], a: T, n: Int): List[List[T]] = {
		if (n==0) (xs.head ::: List(a)) :: xs.tail else xs.head :: pushAt(xs.tail, a, n-1)
	}
	
	// concatenate two lists (complexity: n)
	def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
		case List() => ys
		case z :: zs => z :: concat(zs, ys)
	}

	def pushAtShortest[T](xs: List[List[T]], a: T): List[List[T]] =	pushAt(xs, a, minLenIdx(xs))
	
                                                  
	// Add a new row when the number of rows is less than the length of the shortest list
	// Add a new row with a single entry t
	def newRow[T](xs: List[List[T]], a: T): List[List[T]] = {
		if (xs == Nil) List(List(a)) else xs ::: List(List(a))
	}
	
	val t0 = List(List(2), List(1,3), List(1,0))

	t0 take 1
	t0 take t0.length-1
	t0 drop t0.length-1
	(t0 take t0.length-1) ::: List(t0.last ::: List(4))
	
	maxLen(t0)
	minLenIdx(t0)
	pushAtShortest(t0, 1)
	
	

}