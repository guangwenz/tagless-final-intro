package example
import org.scalatest._
import cats.effect.IO

class CachedKVStoreSpec extends FlatSpec with Matchers with OptionValues {

  behavior of "cached kv store"

  it should "get key" in {
    val store = CachedKVStore[IO].transform(KVStore[IO])
    val p = for {
      _ <- store.get("hello")
      _ <- store.get("hello")
      _ <- store.put("hello", "CB")
      _ <- store.get("hello")
      value <- store.get("hello")
    } yield value
    val ret = p.run(Map.empty[String, String]).unsafeRunSync()
    ret._2.value shouldBe "CB"
  }
}
