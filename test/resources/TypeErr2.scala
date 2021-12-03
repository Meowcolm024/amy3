enum Color {
    case Red()
    case Black()
}

def err(i: Color): Color = {
   Color.Red() + Color.Black()
}