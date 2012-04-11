package myschedule

case class Schedule(VT1: VT, VT2: VT,
  itse: Set[Module],
  vt1: Set[Module],
  vt2: Set[Module],
  ssk: Set[Module],
  mp: Set[Module],
  ma: Set[Module]
) {

  require(VT1 ne VT2)

  def + (module: Module): Set[Schedule] = {
    module.groups.map {
      case ITSE     => this.copy(itse = this.itse + module)
      case VT1      => this.copy(vt1 = this.vt1 + module)
      case VT2      => this.copy(vt2 = this.vt2 + module)
      case MP       => this.copy(mp = this.mp + module)
      case MA       => this.copy(ma = this.ma + module)
      case ssk: SSK => this.copy(ssk = this.ssk + module)
      case _        => this
    }
  }

  def balITSE = Schedule.reqITSE.balance(itse)
  def balVT1 = Schedule.reqVT1(VT1).balance(vt1)
  def balVT2 = Schedule.reqVT2(VT2).balance(vt2)
  def balSSK = Schedule.reqSSK.balance(ssk)
  def balMP = Schedule.reqMP.balance(mp)
  def balMA = Schedule.reqMA.balance(ma)

  lazy val groups = Seq(ITSE, VT1, VT2, SSK, MP, MA)
  lazy val requirements = {
    import Schedule._
    Seq(reqITSE, reqVT1(VT1), reqVT2(VT2), reqSSK, reqMP, reqMA)
  }
  lazy val groupRequirements: Map[ModuleGroup, Requirement] = Map(groups.zip(requirements): _*)

  lazy val partitions = Map(
    ITSE -> itse,
    VT1 -> vt1,
    VT2 -> vt2,
    SSK -> ssk,
    MP -> mp,
    MA -> ma
  )

  /**
   * The smaller (closer to 0) this value the better.
   *
   * @return
   */
  def deviation = requirements.map(req => math.abs(req.balance(partitions(req.moduleGroup)))).sum

  def balanceReport: Map[ModuleGroup, Int] = groupRequirements.map { case (group, req) =>
    group -> req.balance(partitions(group))
  }

  override def toString = {
    val sep = " "
    "Partition(%d | ITSE[%s], %s[%s], %s[%s], SSK[%s], MP[%s], MA[%s])" format (deviation,
      itse.mkString(sep), VT1.toString, vt1.mkString(sep), VT2.toString, vt2.mkString(sep),
      ssk.mkString(sep), mp.mkString(sep), ma.mkString(sep))
  }
}

object Schedule {
  def apply(VT1: VT, VT2: VT) = new Schedule(VT1, VT2, Set(), Set(), Set(), Set(), Set(), Set())

  val reqITSE = Requirement(ITSE, 24 LP)
  def reqVT1(vt: VT) = Requirement(vt, 24 LP)
  def reqVT2(vt: VT) = Requirement(vt, 15 LP)
  val reqSSK = Requirement(SSK, 18 LP)
  val reqMP = Requirement(MP, 9 LP)
  val reqMA = Requirement(MA, 30 LP)
}
