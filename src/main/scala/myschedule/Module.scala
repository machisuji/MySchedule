package myschedule

import org.yaml.snakeyaml.Yaml

trait Module {
  val id: String
  val name: String
  val lp: LP
  val groups: Set[ModuleGroup]
}

class DefaultModule(val id: String, val name: String, val lp: LP, val groups: Set[ModuleGroup]) extends Module {
  override def toString = id
}

object Module {

  def apply(id: String, name: String, lp: LP, groups: Set[ModuleGroup]) =
    new DefaultModule(id, name, lp, groups)

  lazy val all: Set[Module] = {
    def asScalaSet[T](set: java.util.Set[T]): Set[T] = // the default conversion fails at runtime for some reason
      Set(collection.JavaConversions.iterableAsScalaIterable(set).toSeq: _*)
    val source = scala.io.Source.fromFile("src/main/scala/modules/hpi-master.yml").mkString
    val map = (new Yaml).loadAs(source, classOf[java.util.Map[String, java.util.Map[String, java.util.Map[String, _]]]])
    val modules = map.get("modules")
    maybe(modules).map { mods =>
      asScalaSet(mods.keySet).flatMap { modId =>
        val mod = mods.get(modId)
        val name = maybe(mod.get("name").asInstanceOf[String]).getOrElse(modId)
        val javaGroupList = maybe(mod.get("groups").asInstanceOf[java.util.List[String]]).getOrElse(
          new java.util.LinkedList[String])
        val groups = scala.collection.JavaConversions.asScalaBuffer(javaGroupList).flatMap { id =>
          val grp = ModuleGroup.find(id)
          if (!grp.isDefined) println("[warning] unknown ModuleGroup '%s' (Module %s)".format(id, modId))
          grp
        }
        val lp = maybe(mod.get("lp")).flatMap {
          case num: Int => Some(num LP)
          case str: String => {
            val LPR = """(\d+)(\s*\-\s*(\d+))?""".r
            str.trim match {
              case LPR(min, _, max) => {
                if (min != null) {
                  if (max != null) Some(min.toInt to max.toInt LP)
                  else Some(min.toInt LP)
                } else None
              }
              case invalid => {
                println("[warning] Invalid value for LP: \"" + invalid + "\" -- skipping module " + modId)
                None
              }
            }
          }
          case invalid => {
            println("[warning] Invalid value for LP: \"" + invalid + "\" -- skipping module " + modId)
            None
          }
        }
        lp.map(lp => Module(modId, name, lp, groups.toSet))
      }.asInstanceOf[Set[Module]]
    }.getOrElse(Set())
  }

  def find(id: String) = all.find(_.id == id)
}
