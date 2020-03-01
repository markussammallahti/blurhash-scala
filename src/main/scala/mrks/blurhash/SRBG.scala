package mrks.blurhash


object SRBG {
  def toLinear(input: Int): Double = {
    val v = input.toDouble / 255.0d

    if (v <= 0.04045d) {
      v / 12.92d
    }
    else {
      math.pow((v + 0.055d) / 1.055d, 2.4d)
    }
  }

  def fromLinear(value: Double): Int = {
    val v = math.max(0.0d, math.min(1.0d, value))

    if (v <= 0.0031308d) {
      (v * 12.92d * 255.0d + 0.5d).toInt
    }
    else {
      ((1.055d * math.pow(v, 1.0d / 2.4d) - 0.055d) * 255.0d + 0.5d).toInt
    }
  }
}
