package littlejian.utils

import scala.collection.parallel.immutable.{ParHashMap, ParSeq, ParVector}
import collection.parallel.CollectionConverters._

def traverse[T](xs: ParVector[Option[T]]): Option[ParVector[T]] = try {
  Some(xs.seq.map(_.get).par)
} catch {
  case _: NoSuchElementException => None
}

def traverse[T](xs: Vector[Option[T]]): Option[Vector[T]] = try {
  Some(xs.map(_.get))
} catch {
  case _: NoSuchElementException => None
}

def traverse[T](xs: ParSeq[Option[T]]): Option[ParSeq[T]] = try {
  Some(xs.map(_.get))
} catch {
  case _: NoSuchElementException => None
}

def traverse[T](xs: Seq[Option[T]]): Option[Seq[T]] = try {
  Some(xs.map(_.get))
} catch {
  case _: NoSuchElementException => None
}
