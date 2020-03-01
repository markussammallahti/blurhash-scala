package mrks.blurhash

import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
import java.util.Base64

import javax.imageio.ImageIO
import org.scalatest.{EitherValues, Inspectors, MustMatchers, WordSpec}


class BlurhashSpec extends WordSpec with MustMatchers with EitherValues with Inspectors {
  import better.files._
  import mrks.blurhash.Blurhash._

  private def encodePng(image: BufferedImage): String = {
    using(new ByteArrayOutputStream()) { out =>
      ImageIO.write(image, "PNG", out)
      Base64.getEncoder.encodeToString(out.toByteArray)
    }
  }

  "isValid" when {
    "input length is invalid" should {
      "return false" in {
        forAll(List(
          "",
          "a",
          "abc",
          "abc",
          "LGF5",
          "LGF5]",
        )) { input =>
          isValid(input) mustBe false
        }
      }
    }
    "input length does not match size flag" should {
      "return false" in {
        forAll(List(
          "AGF5]+",
          "GGF5]+Yk^6",
          "ZGF5]+Yk^6#M@-",
          "LGF5]+Yk^6#M@-5c,1J5@[or",
        )) { input =>
          isValid(input) mustBe false
        }
      }
    }
    "input is valid blurhash" should {
      "return true" in {
        forAll(List(
          "0GF5]+",
          "2GF5]+Yk^6",
          "LLHV6nae2ek8lAo0aeR*%fkCMxn%",
          "LGFFaXYk^6#M@-5c,1Ex@@or[j6o",
        )) { input =>
          isValid(input) mustBe true
        }
      }
    }
  }

  "decode" should {
    "return error" when {
      "invalid length" in {
        decode("LGF5]", 10, 10) mustBe Left(LengthError)
      }
      "invalid size flat" in {
        decode("AGF5]+", 10, 10) mustBe Left(SizeMismatchError)
      }
    }
    "return image" when {
      "valid blurhash" in {
        val result = decode("LGF5]+Yk^6#M@-5c,1J5@[or[Q6.", 20, 20)
        result.isRight mustBe true

        val encoded = encodePng(result.getOrElse(fail()))
        encoded mustBe "iVBORw0KGgoAAAANSUhEUgAAABQAAAAUCAIAAAAC64paAAAEaUlEQVR42g3LWW/aBgAAYP+iTd2kvURap1bV2i1tmoWmhCS" +
            "EcF/msPGJbWzAXObGBGPAYEICJIRAAiEJudqMtdOkSXvZ9jLtr2zf+weM0P0R2hlhvSE2HOCTLnapovMKPE97Z6xzjJr7kF5BtWJ4LSW+D" +
            "XdXmOsV6rdV+q+10L8aDjjFOqfYwSnWP8FPj/HJIX7VROd7gbngm4VcY9TSg/QNdKMU0STFVa77lr5apX79iflTw/3zLgKcEIcnRHdAHh+" +
            "R4y550Sava/hNEZnH/TPaPUKth7BBxjdz0XVe1DCHa9RMQy/WuT+0/N8bCaBP9fr0UZceduhJi76SqdsSeSdgN2F4RnqGiKONmMpBfTKmC" +
            "4laYl9LnG0w95v8523hd30e6HCDfe5E5c4a3KzC3RTYhyTzwJG3JHqB+I8DbgWz5hgjl9Cj4jbc2sYGO6GL3eStqfjRIgFK7LQRG8uxqcj" +
            "PM/wDH3mkuY8oc+cnpz6kB3slwhnnrFjK5BaNrroJ7ljYY1v21CmfuVRAEs4qwqSYukyl7rjkIxZfePif7dy9jZ448QMILgU9TNTpTtt3S" +
            "zaTZPfVXZwClpo+tenvA6XcpJC7SOWuuewDklnYhU/6xGKDv99iz83Btg/NUBAa8xiy7ndFt64Euou+cB7eyyCHKWwEZMWpIF5ES3O8+MF" +
            "R+GUj9/l1evEqcbcaGe8wTQ+RoBFPHFrP+n7I+9ayfkcyEOGwKkEcwcEpkJImcWkaqlz79x4M4uJ18dO3ucWScPt9bKTj6iAVpXBHIrCSh" +
            "Z9l4TfJgI3FIgFStlIDHXMJJGtjvnZOyTNP9VZXeXxRXnxdeHySmX+XGK5HZBcTDpLWGLacRp4KyDKP2nAibA/KGmbwPDQDksogpgzpxrm" +
            "ndrVZvXux9+Gr0v2XucunqWNNtOoIhYmgLYqvJNHnCfQNi9kggtsJSi+ZoyehKZBuHSSbXbY5gBrnhtrVsnSzJM6/yU+fCX0NL9nYCEq5W" +
            "HydR5cjqIbCHB6c3SLLL6iDL5gRkFfrGVXhWx1COXbUx9rq9FV5+jw/eil012OShYtBFBTEDSz6PoRs4YgLRJltPP8j2VwK9oByu1hUxXR" +
            "L5ppqoN6zVE+05eFqYbCa7mzEqiYuBVIkgrtIxETAZgT2gAHKiAjv0b3XmALI7ZikJkqtbKpZYeoKVD2wlnv6QlcntLdispHN2oNhL4bCs" +
            "DfgB2Gf3+sjnf6o2Z8xQEWg1SYUlaq2IsWmkKiLTLUOl1VHvm0SWga+ZmBLJjJlR1g3RHi9iB9EIDcOuWk/GPaBMaCjgvuqr9nCqgpXqAv" +
            "xqkiXZTivOAXFxNd3QpUdIr8bSFh8nMsd9DkxxIESdjRox2g7AfRUY1e1dFqgomCVeiQrZaJimczL3lTDEm3oQ/ImUd6EswZP3OZkvXYSs" +
            "wYYiz9i9sTNIHCk6vrq9mHT2lb8tRpdkhJJscDkJChVt/2fmboWl7RQcQsULI6I1xokzHDYCCaM9syu5T/bm3DPm3csXQAAAABJRU5ErkJ" +
            "ggg=="
      }
    }
  }

  "encode" should {
    "return blurhash for given image" in {
      forAll(List(
        "img1.jpg" -> "LLHV6nae2ek8lAo0aeR*%fkCMxn%",
        "img2.jpg" -> "LGFFaXYk^6#M@-5c,1Ex@@or[j6o",
        "img3.jpg" -> "L6Pj0^nh.AyE?vt7t7R**0o#DgR4",
        "img4.jpg" -> "LKO2?V%2Tw=^]~RBVZRi};RPxuwH",
        "img5.jpg" -> "LPPGdFbI?ws.-;ofM|R+OFV@r;xu",
        "img6.png" -> "LlMF%n00%#MwS|WCWEM{R*bbWBbH",
      )) { case (fileName, expected) =>
        using(Resource.getAsStream(fileName)) { file =>
          encode(ImageIO.read(file), 4, 3) mustBe expected
        }
      }
    }
  }
}
