package example

import cats.implicits._
import cats.data.StateT
import cats.effect._

trait KVStore[F[_]] {
  def get(key: String): F[Option[String]]
  def put(key: String, value: String): F[Unit]
}

object KVStore {
  import cats.effect.Sync
  def apply[F[_]](implicit F: Sync[F]) = new KVStore[F] {
    val map = collection.mutable.Map.empty[String, String]
    def get(key: String): F[Option[String]] =
      for {
        _ <- F.delay { println(s"getting for key $key") }
        ret <- F.delay { map.get(key) }
      } yield ret

    def put(key: String, value: String): F[Unit] = F.delay(map.put(key, value))
  }
}

trait CachedKVStore[F[_]] {
  type Cache = Map[String, String]
  type CachedState[A] = StateT[F, Cache, A]

  def transform(
      proxy: KVStore[F]
  )(implicit F: Sync[F]): KVStore[CachedState] =
    new KVStore[CachedState] {
      def get(key: String): CachedState[Option[String]] = {
        for {
          cache <- StateT.get[F, Cache]
          ret <- cache.get(key) match {
            case s @ Some(_) => s.pure[CachedState]
            case None =>
              StateT
                .liftF[F, Cache, Option[String]](proxy.get(key))
                .flatTap(updateCache(key))
          }
        } yield ret
      }

      def updateCache(key: String)(value: Option[String]): CachedState[Unit] =
        value match {
          case Some(value) => StateT.modify(_.updated(key, value))
          case None        => ().pure[CachedState]
        }

      def put(key: String, value: String): CachedState[Unit] =
        StateT.liftF[F, Cache, Unit](proxy.put(key, value)) *> StateT.modify(
          _.updated(key, value)
        )
    }
}

object CachedKVStore {
  def apply[F[_]]: CachedKVStore[F] = new CachedKVStore[F] {}
}
