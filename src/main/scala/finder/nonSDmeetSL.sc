package finder
import java.io._

object nonSDmeetSL {

	val A = new CIB(5, List(List(2), List(1,3), List(1,0,4), List(1,0,0,0)))

	A.isFull
	A.bin(0,0)
	A.bin(0,1)
	A.bin(0,2)
	A.bin(1,1)
	A.cibtable
	val B = new CIB(5, List(List(2)))
	B.isFull
	val C = new CIB(5, B.push(1))
	val D = new CIB(5, C.push(3))
	val E = new CIB(5, D.push(1))
	val F = new CIB(5, E.push(0))
	val G = new CIB(5, F.push(4))
	val H = new CIB(5, G.push(1))
	val I = new CIB(5, H.push(0))
	val J = new CIB(5, I.push(0))
	val K = new CIB(J.n, J.push(0))
	K.cibtable

	val L = new CIB(4, List(List(0)))
	L
	K
	def isVal: Boolean = (L.t(0)(0)==0)
	isVal
}