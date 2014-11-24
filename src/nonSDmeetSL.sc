package finder
import java.io._

object nonSDmeetSL {

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

	val L = new CIB(4, List(List(0)))         //> L  : finder.CIB = List(0)
	L                                         //> res8: finder.CIB = List(0)
	K                                         //> res9: finder.CIB = List(2), List(1, 3), List(1, 0, 4), List(1, 0, 0, 0)
	def isVal: Boolean = (L.t(0)(0)==0)       //> isVal: => Boolean
	isVal                                     //> res10: Boolean = true
}