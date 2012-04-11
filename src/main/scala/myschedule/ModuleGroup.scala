package myschedule

case class LP(min: Int, max: Int) {
  def contains(lp: Int) = min <= lp && lp <= max

  override def toString =
    if (min == max) "%d LP".format(min)
    else "%d to %d LP".format(min, max)
}

trait ModuleGroup {
  val id: String
  val name: String

  def includes(group: ModuleGroup) = group eq this
}

object ModuleGroup {
  val all = Set(ITSE, BPET, HCT, IST, OSIS, SAMT, SSK.MA, SSK.RE, SSK.KO, SSK.DT, SSK.SK, MP, MA)

  def find(id: String) = all.find(_.id == id)
}

sealed class ITSE extends ModuleGroup {
  val id = "ITSE"
  val name = "IT-Systems Engineering"
}
case object ITSE extends ITSE

sealed trait VT extends ModuleGroup

sealed class BPET extends VT {
  val id = "BPET"
  val name = "Business Process & Enterprise Technologies"
}
case object BPET extends BPET

sealed class HCT extends VT {
  val id = "HCT"
  val name = "Human Computer Interaction & Graphics Technology"
}
case object HCT extends HCT

sealed class IST extends VT {
  val id = "IST"
  val name = "Internet & Security Technolgy"
}
case object IST extends IST

sealed class OSIS extends VT {
  val id = "OSIS"
  val name = "Operating Systems & Information Systems Technology"
}
case object OSIS extends OSIS

sealed class SAMT extends VT {
  val id = "SAMT"
  val name = "Software Architecture & Modeling Technology"
}
case object SAMT extends SAMT


sealed trait SSK extends ModuleGroup

case object SSK extends ModuleGroup {
  val id = "SSK"
  val name = "Softskills"

  override def includes(group: ModuleGroup) = group match {
    case SSK    => true
    case SSK.MA => true
    case SSK.RE => true
    case SSK.KO => true
    case SSK.DT => true
    case SSK.SK => true
    case _      => false
  }

  sealed class MA extends SSK {
    val id = "SSK-MA"
    val name = "Softskills: Management"
  }
  case object MA extends MA

  sealed class RE extends SSK {
    val id = "SSK-RE"
    val name = "Softskills: Recht"
  }
  case object RE extends RE

  sealed class KO extends SSK {
    val id = "SSK-KO"
    val name = "Softskills: Kommunikation"
  }
  case object KO extends KO

  sealed class DT extends SSK {
    val id = "SSK-DT"
    val name = "Softskills: Design Thinking"
  }
  case object DT extends DT

  sealed class SK extends SSK {
    val id = "SSK-SK"
    val name = "Softskills: Schlüsselkompetenzen"
  }
  case object SK extends SK
}

sealed class MP extends ModuleGroup with Module {
  val id = "MP"
  val name = "Masterprojekt"

  val lp = 9 LP
  val groups = Set(this.asInstanceOf[ModuleGroup])
}
case object MP extends MP

sealed class MA extends ModuleGroup with Module {
  val id = "MA"
  val name = "Masterarbeit"

  val lp = 30 LP
  val groups = Set(this.asInstanceOf[ModuleGroup])
}
case object MA extends MA
