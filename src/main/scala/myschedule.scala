package object myschedule {
  implicit def intToLP(i: Int) = new {
    def LP = new LP(i, i)
  }

  implicit def rangeToLP(range: Range) = new {
    def LP = new LP(range.min, range.max)
  }

  implicit def cartesianProduct[A](as: Iterable[A]) = new {
    def * [B](bs: Iterable[B]): Seq[(A, B)] =
      (for (a <- as; b <- bs) yield (a, b)).toSeq
  }

  implicit def addModuleToPartitions(ps: Set[Schedule]) = new {
    def + (module: Module): Set[Schedule] = ps.map(p => p + module).flatten
  }

  def maybe[T](ref: T): Option[T] = if (ref == null) None else Some(ref)
}
