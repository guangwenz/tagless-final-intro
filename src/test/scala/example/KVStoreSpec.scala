package example

import org.scalatest._
import cats.effect.IO

class KVStoreSpec extends FlatSpec with Matchers with OptionValues {
  behavior of "KVStore"

  it should "get by key" in {
    val store = KVStore[IO]
    val p = for {
      _ <- store.put("hello", "CB")
      value <- store.get("hello")
    } yield value
    p.unsafeRunSync().value shouldBe "CB"
  }
}
