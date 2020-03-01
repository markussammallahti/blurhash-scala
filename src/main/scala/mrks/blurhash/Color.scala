package mrks.blurhash


case class Color(red: Double, green: Double, blue: Double) {
  def * (multiplier: Double): Color = Color(red * multiplier, green * multiplier, blue * multiplier)
  def + (other: Color): Color = Color(red + other.red, green + other.green, blue + other.blue)

  def values: Seq[Double] = Seq(red, green, blue)

  def toInt: Int = {
    new java.awt.Color(
      SRBG.fromLinear(red),
      SRBG.fromLinear(green),
      SRBG.fromLinear(blue)
    ).getRGB
  }
}

object Color {
  def apply(value: Int): Color = {
    val color = new java.awt.Color(value)
    Color(
      SRBG.toLinear(color.getRed),
      SRBG.toLinear(color.getGreen),
      SRBG.toLinear(color.getBlue)
    )
  }
}
