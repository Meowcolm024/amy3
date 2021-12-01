def boolToString(b: Boolean): String = {
  if (b) {
    "true"
  } else {
    "false"
  }
}

def intToString(i: Int): String = {
    if (i < 0) {
      "-" ++ intToString(-i)
    } else {
      val rem: Int = i % 10;
      val div: Int = i / 10;
      if (div == 0) { digitToString(rem) }
      else { intToString(div) ++ digitToString(rem) }
    }
}
