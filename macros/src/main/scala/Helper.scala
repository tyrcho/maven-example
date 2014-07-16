import scala.reflect.macros.Context

// uses the technique described in:
// http://docs.scala-lang.org/overviews/macros/overview.html#writing_bigger_macros
class Helper[C <: Context](val c: C) extends QuasiquoteCompat {
  import c.universe._

  // to learn more about quasiquotes, visit:
  // http://docs.scala-lang.org/overviews/macros/quasiquotes.html
  def hello = q"""
    println("hello world!")
  """

  //inspired from https://gist.github.com/anonymous/7ab617d054f28d68901b
  
  val classdef = q"""class Foo extends Liftable {
    def bar[Baz](faz: Int) = faz * 2
    def fuz(caz: Int)(shmaz: Int): Int = caz * shmaz
  }"""

  def lift(tree: Tree) = {
    val q"class $name extends Liftable { ..$body }" = tree

    val newdefs = body collect {
      case q"def $fname[..$tparams](...$argss): $tresult = $fbody" =>
        val newresult = if (tresult.isEmpty) tresult else tq"Future[$tresult]" 
        val newname = quasiquoteCompat.TermName("async" + fname.toString.capitalize)
        val argss0 = argss.map(a0 => a0.map(a1 => a1.asInstanceOf[ValDef]))
        val tparams0 = tparams.map(_.asInstanceOf[TypeDef]).toList
        q"def $newname[..$tparams0](...$argss0): $newresult = future { $fbody }"
    }

    val name0 = name.asInstanceOf[TypeName]
    q"class $name0 extends AnyRef { ..${(body ++ newdefs).toList} }"
  }

  println(lift(classdef))
}

