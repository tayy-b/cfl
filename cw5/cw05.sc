import $file.fun_tokens, fun_tokens._
import $file.fun_parser, fun_parser._

// CW5

//to do:
//update lexer and parser
//from previous coursework
//to work with the typed version of the Fun language.
// = 5% of grade.
//done!!!!

//goal: lex and parse 5 fun programs
//generate code for the LLVM IR
// deal with three types: Int, double and void (i32, double, void)
//extend lexer and parser to work with these.

//global contstants: val Ymin, val Maxiters.
// start with capital letter
//all other identifiers start with lower case.

//function definitions can have arguments of Int or Double
//need to specify a return type which can be void.

//idea: record all typing information given in the Fun program
// delay any further typing inference to after the CPS translation.
// parser should generate ASTs given by scala dataypes.

// typing
type Ty = String
type TyEnv = Map[String, Ty]

// initial typing environment
var initialEnv = Map[String, Ty](
  "skip" -> "Void",
  "print_int" -> "Void",
  "print_char" -> "Void",
  "print_space" -> "Void",
  "print_star" -> "Void",
  "new_line" -> "Void"
)

val typeConversion = Map("Int" -> "i32", "Double" -> "double", "Void" -> "void")

//typing-environment that updates the information about what type each
//variable, operation and so on receives.
def typ_val(v: KVal, ts: TyEnv): KVal = v match {
  case KVar(s, ty) => {
    val typeOfS = ts.getOrElse(s, "UNDEF")
    // if (thingy == "UNDEF") { println("Couldnt find for: " + s) }
    KVar(s, typeOfS)
  }
  case KNum(i)     => KNum(i)
  case KFNum(d)    => KFNum(d)
  case KChConst(c) => KChConst(c)
  case KConst(o)   => KConst(o)
  case KFConst(o)  => KFConst(o)
  case Kop(o, v1, v2) => {
    typ_val(v1, ts)
  }
  case KCall(o, vrs) => {
    val typedO = ts.getOrElse(o, "UNDEF")
    val argTypes = vrs.map(arg => {
      typ_val(arg, ts)
    })

    // println("Kcall args untyped:" + vrs + " typed: " + argTypes)
    KCall(typedO, argTypes)
  }
}

// Internal CPS language for FUN
abstract class KExp
abstract class KVal

case class KVar(s: String, ty: Ty = "UNDEF") extends KVal
case class KNum(i: Int) extends KVal
case class KFNum(d: Double) extends KVal
case class KChConst(c: Int) extends KVal
case class KConst(s: String) extends KVal
case class KFConst(o: String) extends KVal
case class Kop(o: String, v1: KVal, v2: KVal) extends KVal
case class KCall(o: String, vrs: List[KVal]) extends KVal
case class KWrite(v: KVal) extends KVal

case class KLet(x: String, e1: KVal, e2: KExp) extends KExp {
  override def toString = s"LET $x = $e1 in \n$e2"
}

case class KIf(x1: String, e1: KExp, e2: KExp) extends KExp {
  def pad(e: KExp) = e.toString.replaceAll("(?m)^", "  ")

  override def toString =
    s"IF $x1\nTHEN\n${pad(e1)}\nELSE\n${pad(e2)}"
}
case class KReturn(v: KVal) extends KExp

// for generating new labels
var counter = -1

def Fresh(x: String) = {
  counter += 1
  x ++ "_" ++ counter.toString()
}

def CPS(e: Exp)(k: KVal => KExp): KExp = e match {
  case Var(s) => {
    // for global variables:
    if (s.head.isUpper) {
      initialEnv(s) match {
        case "Int" => {
          val z = Fresh("tmp")
          initialEnv = initialEnv + (z -> "Int")
          KLet(z, KConst(s), k(KVar(z, "Int")))
        }
        case "Double" => {
          val z = Fresh("tmp")
          initialEnv = initialEnv + (z -> "Double")
          KLet(z, KFConst(s), k(KVar(z, "Double")))
        }
      }
    } else {
      k(KVar(s))
    }
  }
  case Num(i)         => k(KNum(i))
  case Fnum(i)        => k(KFNum(i))
  case ChConst(ascii) => k(KChConst(ascii))
  case Aop(o, e1, e2) => {
    val z = Fresh("tmp")
    CPS(e1)(y1 => CPS(e2)(y2 => KLet(z, Kop(o, y1, y2), k(KVar(z)))))
  }
  case If(Bop(o, b1, b2), e1, e2) => {
    val z = Fresh("tmp")
    CPS(b1)(y1 =>
      CPS(b2)(y2 => KLet(z, Kop(o, y1, y2), KIf(z, CPS(e1)(k), CPS(e2)(k))))
    )
  }

  case Call(name, args) => {
    def aux(args: List[Exp], vs: List[KVal]): KExp = args match {
      case Nil => {

        val z = Fresh("tmp")
        KLet(z, KCall(name, vs), k(KVar(z)))
      }
      case e :: es => CPS(e)(y => aux(es, vs ::: List(y)))
    }
    aux(args, Nil)
  }
  case Sequence(e1, e2) =>
    CPS(e1)(_ => CPS(e2)(y2 => k(y2)))
  case Write(e) => {
    val z = Fresh("tmp")
    CPS(e)(y => KLet(z, KWrite(y), k(KVar(z))))
  }
}

//initial continuation
def CPSi(e: Exp) = CPS(e)(KReturn)

// prelude
val prelude = """
declare i32 @printf(i8*, ...)

@.str_nl = private constant [2 x i8] c"\0A\00"
@.str_star = private constant [2 x i8] c"*\00"
@.str_space = private constant [2 x i8] c" \00"
@.str_int = private constant [3 x i8] c"%d\00"
@.str_c = private constant [3 x i8] c"%c\00"

define void @new_line() #0 {
  %t0 = getelementptr [2 x i8], [2 x i8]* @.str_nl, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %t0)
  ret void
}

define void @print_star() #0 {
  %t0 = getelementptr [2 x i8], [2 x i8]* @.str_star, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %t0)
  ret void
}

define void @print_space() #0 {
  %t0 = getelementptr [2 x i8], [2 x i8]* @.str_space, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %t0)
  ret void
}

define void @print_int(i32 %x) {
  %t0 = getelementptr [3 x i8], [3 x i8]* @.str_int, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %t0, i32 %x) 
  ret void
}

define void @print_char(i32 %x) {
  %t0 = getelementptr [3 x i8], [3 x i8]* @.str_c, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %t0, i32 %x)
  ret void
}

define void @skip() #0 {
  ret void
}

; END OF BUILT-IN FUNCTIONS (prelude)
"""

// convenient string interpolations
// for instructions, labels and methods
import scala.language.implicitConversions
import scala.language.reflectiveCalls

extension (sc: StringContext) {
  def i(args: Any*): String = "   " ++ sc.s(args: _*) ++ "\n"
  def l(args: Any*): String = sc.s(args: _*) ++ ":\n"
  def m(args: Any*): String = sc.s(args: _*) ++ "\n"
}

// mathematical and boolean operations
def compile_op(op: String) = op match {
  case "+"  => "add i32 "
  case "*"  => "mul i32 "
  case "-"  => "sub i32 "
  case "/"  => "sdiv i32 "
  case "%"  => "srem i32 "
  case "==" => "icmp eq i32 "
  case "!=" => "icmp ne i32"
  case "<=" => "icmp sle i32 " // signed less or equal
  case "<"  => "icmp slt i32 " // signed less than
}

def compile_dop(op: String) = op match {
  case "+"  => "fadd double "
  case "*"  => "fmul double "
  case "-"  => "fsub double "
  case "/"  => "fdiv double "
  case "%"  => "frem double "
  case "==" => "fcmp oeq double "
  case "!=" => "fcmp one double "
  case "<=" => "fcmp ole double "
  case "<"  => "fcmp olt double "
}

// compile K values
def compile_val(v: KVal): String = v match {
  case KNum(i)     => s"$i"
  case KVar(s, _)  => s"%$s"
  case KFNum(d)    => s"$d"
  case KChConst(c) => s"$c"
  case KConst(s)   => s"load i32, i32* @$s"
  case KFConst(o)  => s"load double, double* @$o"
  case Kop(op, x1, x2) => {
    val x1Type = typ_val(x1, initialEnv);
    val x2Type = typ_val(x2, initialEnv);

    (x1Type, x2Type) match {
      case (KFNum(x), _) => {
        s"${compile_dop(op)} ${compile_val(x1)}, ${compile_val(x2)}"
      }
      case (_, KFNum(x)) => {
        s"${compile_dop(op)} ${compile_val(x1)}, ${compile_val(x2)}"
      }
      case (KVar(x, "Double"), _) => {
        s"${compile_dop(op)} ${compile_val(x1)}, ${compile_val(x2)}"
      }
      case (_, KVar(x, "Double")) => {

        s"${compile_dop(op)} ${compile_val(x1)}, ${compile_val(x2)}"
      }

      case _ => s"${compile_op(op)} ${compile_val(x1)}, ${compile_val(x2)}"
    }
  }

  case KCall(x1, args) => {
    val x1Type = initialEnv(x1)
    val typedArgs = args.map(arg => typ_val(arg, initialEnv))
    val argsString = typedArgs
      .foldLeft("") { (acc, arg) =>
        arg match {
          case KVar(argName, argType) if argType != "UNDEF" => {
            acc + s"${typeConversion(argType)} ${compile_val(KVar(argName, argType))}, "

          }
          case KNum(i) =>
            acc + s"i32 $i, "
          case KFNum(d)        => acc + s"double $d, "
          case KChConst(ascii) => acc + s"i32 $ascii, "
          case _ =>
            acc
        }
      }
      .dropRight(2)
    s"call ${typeConversion(x1Type)} @$x1 ($argsString)"

  }

  case KWrite(x1) =>
    s"call i32 @printInt (i32 ${compile_val(x1)})"
}

// compile K expressions
def compile_exp(a: KExp): String = a match {
  case KReturn(v) => {
    val typed = typ_val(v, initialEnv)
    // println("KReturn untyped is: " + v + " typed is:" + typed)
    typed match {
      case KVar(s, "Int") => {
        i"ret i32 ${compile_val(v)}"
      }
      case KVar(s, "Double") => {
        i"ret double ${compile_val(v)}"
      }
      case KVar(s, "Void") => {
        i"ret void"
      }
      case KNum(_) =>
        i"ret i32 ${compile_val(v)}"
    }
  }

  case KLet(x: String, v: KVal, e: KExp) => {

    val typedValue = v match {
      case KCall("print_string", args) =>
        v // dont want to add print string to typing environment
      case _ => typ_val(v, initialEnv)
    }
    typedValue match {
      case KVar(_, ty) if (ty != "UNDEF") => {
        initialEnv = initialEnv + (x -> ty)
      }
      case KCall(ty, _) if (ty != "UNDEF") => {
        initialEnv = initialEnv + (x -> ty)
      }
      case KChConst(i) => {
        initialEnv = initialEnv + (x -> "Int")
      }
      case KFNum(i) => {
        initialEnv = initialEnv + (x -> "Double")
      }
      case _ => { println("Wasn't kvar: " + typedValue) }
    }

    typedValue match {
      case KCall("print_string", args) => {
        val string: String = args.headOption match {
          case Some(KVar(name, _)) => name.replaceAll("^\"|\"$", "")
        }

        val charCalls: String = string.toList
          .map(char =>
            compile_val(KCall("print_char", List(KChConst(char.toInt))))
          )
          .mkString("\n")

        i"$charCalls" ++ compile_exp(e)
      }
      case KCall("Void", _) => i"${compile_val(v)}" ++ compile_exp(e)
      case _ => {
        i"%$x = ${compile_val(v)}" ++ compile_exp(e)
      }

    }
  }
  case KIf(x, e1, e2) => {
    val if_br = Fresh("if_branch")
    val else_br = Fresh("else_branch")
    i"br i1 %$x, label %$if_br, label %$else_br" ++
      l"\n$if_br" ++
      compile_exp(e1) ++
      l"\n$else_br" ++
      compile_exp(e2)
  }
}

def compile_decl(d: Decl): String = d match {
  case Def(name, args, ty, body) => {
    // adds the function name and its type to the environment
    initialEnv = initialEnv + (name -> ty)
    // now we need to add the types of each argument to the environment
    initialEnv = initialEnv ++ args.map { case (argName, argType) =>
      argName -> argType
    }
    // println("the intial environment is:" + initialEnv)
    m"define ${typeConversion(ty)} @$name (${args
        .map({ case (x, y) => s"${typeConversion(y)} %$x" })
        .mkString(", ")}) {" ++
      compile_exp(CPSi(body)) ++
      m"}\n"
  }

  case Main(body) => {
    m"define i32 @main() {" ++
      compile_exp(CPS(body)(_ => KReturn(KNum(0)))) ++
      m"}\n"
  }
  case Const(name, v) => {
    initialEnv = initialEnv + (name -> "Int")
    s"@$name = global i32 $v\n\n"
  }
  case FConst(name, x) => {
    initialEnv = initialEnv + (name -> "Double")
    s"@$name = global double $x\n\n"
  }

}

// main compiler functions
def fun_compile(prog: List[Decl]): String =
  prelude ++ (prog.map(compile_decl).mkString)

@main
def main(fname: String) = {
  val path = os.pwd / fname
  val file = fname.stripSuffix("." ++ path.ext)
  val tks = tokenise(os.read(path))
  val ast = parse_tks(tks)
  println(fun_compile(ast))
}

@main
def write(fname: String) = {
  val path = os.pwd / fname
  val file = fname.stripSuffix("." ++ path.ext)
  val tks = tokenise(os.read(path))
  val ast = parse_tks(tks)
  println(ast)
  val code = fun_compile(ast)
  os.write.over(os.pwd / (file ++ ".ll"), code)
}

// @main
// def run(fname: String) = {
//   val path = os.pwd / fname
//   val file = fname.stripSuffix("." ++ path.ext)
//   write(fname)
//   os.proc("llc", "-filetype=obj", file ++ ".ll").call()
//   os.proc("gcc", file ++ ".o", "-o", file ++ ".bin").call()
//   os.proc(os.pwd / (file ++ ".bin")).call(stdout = os.Inherit)
//   println(s"done.")
// }

@main
def run(fname: String) = {
  val path = os.pwd / fname
  val file = fname.stripSuffix("." ++ path.ext)
  write(fname)
  os.proc("clang", file ++ ".ll").call()
  os.proc("./a.exe").call()
}
