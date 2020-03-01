package mrks.blurhash

import org.scalatest.{MustMatchers, WordSpec}


class Base83Spec extends WordSpec with MustMatchers {
  import mrks.blurhash.Base83._

  "decode" should {
    "work" in {
      decode("asdf") mustBe 20959616L
      decode("12345") mustBe 48622899L
      decode("+Cnz?-w") mustBe 21627693975405L
      decode("#$*.=@]_") mustBe 1703291758829901L
    }
  }

  "encode" should {
    "work" in {
      encode(20959616L, 4) mustBe "asdf"
      encode(48622899L, 5) mustBe "12345"
      encode(21627693975405L, 7) mustBe "+Cnz?-w"
      encode(1703291758829901L, 8) mustBe "#$*.=@]_"
    }
  }
}
