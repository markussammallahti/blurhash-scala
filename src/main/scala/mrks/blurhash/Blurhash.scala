package mrks.blurhash

import java.awt.image.BufferedImage

import mrks.Compatibility


object Blurhash {
  sealed trait Error
  case object LengthError extends Error
  case object SizeMismatchError extends Error

  def isValid(input: String): Boolean = {
    (for {
      _ <- checkLength(input)
      _ <- checkSize(input)
    } yield {
      true
    }).isRight
  }

  private def checkLength(input: String): Either[Error, String] = {
    Either.cond(input.length >= 6, input, LengthError)
  }

  private def checkSize(input: String): Either[Error, String] = {
    val (x, y) = getSize(input)

    Either.cond(input.length == 4 + 2 * x * y, input , SizeMismatchError)
  }

  def decode(input: String, width: Int, height: Int, punch: Double = 1.0): Either[Error, BufferedImage] = {
    for {
      _ <- checkLength(input)
      _ <- checkSize(input)
    } yield {
      doDecode(input, width, height, punch)
    }
  }

  private def doDecode(input: String, width: Int, height: Int, punch: Double): BufferedImage = {
    val (numY, numX)  = getSize(input)
    val maximumValue  = getMaximumValue(input)
    val colors        = getColors(input, numY, numX, maximumValue, punch)

    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

    for {
      x <- 0 until width
      y <- 0 until height
    } yield {
      val targetColors = for {
        i <- 0 until numX
        j <- 0 until numY
      } yield {
        val basis = getBasis(x, y, i, j, width, height)
        colors(i + j * numX) * basis
      }

      val color = targetColors.reduce(_ + _)
      image.setRGB(x, y, color.toInt)
    }

    image
  }

  def encode(image: BufferedImage, cx: Int, cy: Int): String = {
    val width   = image.getWidth
    val height  = image.getHeight

    val factors = for {
      y <- 0 until cy
      x <- 0 until cx
    } yield {
      val normalization = if (x == 0 && y == 0) 1.0d else 2.0d
      val scale         = normalization / (width * height)

      val targetFactors = for {
        i <- 0 until width
        j <- 0 until height
      } yield {
        val basis = getBasis(x, y, i, j, width, height)
        Color(image.getRGB(i, j)) * basis
      }

      targetFactors.reduce(_ + _) * scale
    }

    val dc    = factors.head
    val ac    = factors.tail
    val part1 = Base83.encode(getSizeFlag(cx, cy), 1)

    val (maximum, part2) = if (ac.nonEmpty) {
      val actualMaximum     = ac.flatMap(_.values).map(_.abs).max(Compatibility.DoubleOrdering) // (Ordering.Double.TotalOrdering)
      val quantisedMaximum  = math.max(0, math.min(82, math.floor(actualMaximum * 166.0 - 0.5))).toInt
      val maximumValue      = (quantisedMaximum + 1).toDouble / 166.0

      (maximumValue, Base83.encode(quantisedMaximum, 1))
    }
    else {
      (1.0d, Base83.encode(0, 1))
    }

    val part3 = Base83.encode(encodeDc(dc), 4)
    val rest  = ac.map(encodeAc(_, maximum)).map(Base83.encode(_, 2))

    (Seq(part1, part2, part3) ++ rest).mkString
  }

  private def getBasis(x: Int, y: Int, i: Int, j: Int, width: Int, height: Int): Double = {
    math.cos(math.Pi * x * i / width) * math.cos(math.Pi * y * j / height)
  }

  private def getSize(input: String): (Int, Int) = {
    val sizeFlag = Base83.decode(input(0).toString)

    (((sizeFlag / 9) + 1).toInt, ((sizeFlag % 9) + 1).toInt)
  }

  private def getSizeFlag(cx: Int, cy: Int): Int = {
    cx - 1 + (cy - 1) * 9
  }

  private def getMaximumValue(input: String): Double = {
    (Base83.decode(input(1).toString) + 1).toDouble / 166.0d
  }

  private def getColors(input: String, y: Int, x: Int, maximum: Double, punch: Double): IndexedSeq[Color] = {
    val first = IndexedSeq(decodeDc(Base83.decode(input.substring(2, 6)).toInt))

    (1 until (x * y)).foldLeft(first) { (result, n) =>
      val value = Base83.decode(input.substring(4 + n * 2, 4 + n * 2 + 2))
      result :+ decodeAc(value, maximum * punch)
    }
  }

  private def decodeDc(input: Int): Color = {
    val red   = input >> 16
    val green = (input >> 8) & 255
    val blue  = input & 255

    Color(SRBG.toLinear(red), SRBG.toLinear(green), SRBG.toLinear(blue))
  }

  private def encodeDc(value: Color): Int = {
    val red   = SRBG.fromLinear(value.red)
    val green = SRBG.fromLinear(value.green)
    val blue  = SRBG.fromLinear(value.blue)

    (red << 16) + (green << 8) + blue
  }

  private def decodeAc(input: Long, maximum: Double): Color = {
    val quantR = input / (19 * 19)
    val quantG = (input / 19) % 19
    val quantB = input % 19

    def convert(v: Long) = signPow((v.toDouble - 9) / 9, 2) * maximum

    Color(convert(quantR), convert(quantG), convert(quantB))
  }

  private def encodeAc(value: Color, maximumValue: Double): Long = {
    def convert(v: Double) = math.max(0, math.min(18, math.floor(signPow(v / maximumValue, 0.5d) * 9.0d + 9.5d))).toLong

    val quantR = convert(value.red)
    val quantG = convert(value.green)
    val quantB = convert(value.blue)

    (quantR * 19 * 19) + (quantG * 19) + quantB
  }

  private def signPow(value: Double, exp: Double): Double = {
    val sign = if (value < 0) -1 else 1
    sign * math.pow(value.abs, exp)
  }
}
