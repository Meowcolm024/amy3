enum MyBase{
	case Sub0()
	case Sub1(x: Int)
	case Sub2(x: Int, y: Int)
	case Sub3(s1: MyBase, s2: MyBase)
}

def consBase(x: Int): MyBase = {
	MyBase.Sub1(x)
}
def getInt(x: MyBase): Int = {
	x match {
		case MyBase.Sub0() => 0
		case MyBase.Sub1(i) => i
		case MyBase.Sub2(i, j) => i+j
		case MyBase.Sub3(p, q) => getInt(p) + getInt(q)
	}
}

def testLitMatch(): Int = {
	val x : Int = 1*10+5/2;
	(x + 12) match{
		case 1 => 1
		case 2 => 2
		case y => y*y+x
		case _ => 0
	} match {
		case 2 => 15
		case _ => 0
		case y => 11
	}
}

def testADTMatch(): Int = {
	val x: MyBase = MyBase.Sub0();
	val y: MyBase = consBase(12);
	MyBase.Sub3(MyBase.Sub2(2, 13), x) match{
		case MyBase.Sub1(any) => any
		case MyBase.Sub2(any, _) => any
		case MyBase.Sub3(_, MyBase.Sub1(_)) => 15
		case MyBase.Sub3(MyBase.Sub2(11, 13), _) => 16
		case MyBase.Sub3(any, z) => getInt(any)+100
		case MyBase.Sub3(_, _) => 17
	}
}