# blurhash-scala

Scala implementation of [blurhash](https://github.com/woltapp/blurhash)

## Usage

build.sbt
```
resolvers += Resolver.bintrayRepo("mrks", "maven")
libraryDependencies += "mrks" %% "blurhash-scala" % "1.0"
```

Example.scala
```scala
import javax.imageio.ImageIO
import java.io.File

import mrks.blurhash.Blurhash

object Example {
  def encode(file: File): String = {
    val image = ImageIO.read(file)
    Blurhash.encode(image, 4, 3)
  }

  def decode(blurhash: String): Either[Blurhash.Error, File] = {
    Blurhash.decode(blurhash, 40, 30).map { image =>
      ImageIO.write(image, "PNG", new File("target.png"))
    }
  }
}
```
