enum MyBase{
	case Sub0()
	case Sub1(x: Int)
	case Sub2(x: Int, y: Int)
	case Sub3(s1: MyBase, s2: MyBase)
}

def getInt(x: Int): Int = {x}

@main
def test(): Unit = {
    val x: MyBase = (
		MyBase.Sub0();
		val y: MyBase = MyBase.Sub1(12); 
		if(15-3 == 12){MyBase.Sub2(15, 16)} 
		else {y}
	);
	val y: MyBase = MyBase.Sub1(10);
	println(toString(
		(
      MyBase.Sub1(); 
      "Hello"++"World";
      1
    ) 
		+ 
		(
      getInt(15); 
      if(!(!(12345;false) || getInt(0) == 2)) {getInt(15)} 
      else {12}; 
      val tmp: Int = 2; 
      tmp
    )
	));
	Sub3(y,y) match{
		case MyBase.Sub2(_, _) => println("Hello1\n")
    case MyBase.Sub3(any, _) => any match{
      case Sub2(_, _) => println("Hello2\n")
      case _ => println("Hello3\n")
    }
		case _ => println("Hello4\n")
	}
}