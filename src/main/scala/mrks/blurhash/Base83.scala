package mrks.blurhash


object Base83 {
  private val chars = IndexedSeq(
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
    'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',
    'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd',
    'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
    'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x',
    'y', 'z', '#', '$', '%', '*', '+', ',', '-', '.',
    ':', ';', '=', '?', '@', '[', ']', '^', '_', '{',
    '|', '}', '~'
  )

  def decode(input: String): Long = {
    input.foldLeft(0L) { (result, char) =>
      result * 83 + chars.indexOf(char)
    }
  }

  def encode(value: Long, length: Int): String = {
    def toIndex(n: Int) = ((value / math.pow(83.0, length - n)) % 83.0).toInt

    (1 to length).map(toIndex).map(chars.charAt).mkString
  }
}
