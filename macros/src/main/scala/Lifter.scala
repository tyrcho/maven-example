
import scala.reflect.runtime.universe._

object lifter extends App {

  val classdef = q"""class Foo extends Liftable {
    def bar[Baz](faz: Int) = faz * 2
    def fuz(caz: Int)(shmaz: Int): Int = caz * shmaz
  }"""

  def lift(tree: Tree) = {
    val q"class $name extends Liftable { ..$body }" = tree

    val newdefs = body collect {
      case q"def $fname[..$tparams](...$argss): $tresult = $fbody" =>
        val newresult = if (tresult.nonEmpty) tq"Future[$tresult]" else tresult
        val newname = TermName("async" + fname.toString.capitalize)
        val argss0 = argss.map(a0 => a0.map(a1 => a1.asInstanceOf[ValDef]))
        val tparams0 = tparams.map(_.asInstanceOf[TypeDef]).toList
        q"def $newname[..$tparams0](...$argss0): $newresult = future { $fbody }"
    }

    val name0 = name.asInstanceOf[TypeName]
    q"class $name0 extends AnyRef { ..${(body ++ newdefs).toList} }"
  }

  println(lift(classdef))
}