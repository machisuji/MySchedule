package myschedule

case class Requirement(moduleGroup: ModuleGroup, lp: LP) {
  def balance(modules: Set[Module]) =
    modules.filter(_.groups.exists(moduleGroup.includes)).toSeq.map(_.lp.max).sum - lp.min
}
