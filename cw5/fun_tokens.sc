// CW 2
//======

// Rexp

abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp
case class STAR(r: Rexp) extends Rexp
case class RECD(x: String, r: Rexp) extends Rexp

//extended:
case class RANGE(s: Set[Char]) extends Rexp
case class PLUS(r: Rexp) extends Rexp
case class OPTIONAL(r: Rexp) extends Rexp
case class NTIMES(r: Rexp, n: Int) extends Rexp

// Values, you might have to extend them
// according to which values you want to create
abstract class Val
case object Empty extends Val
case class Chr(c: Char) extends Val
case class Sequ(v1: Val, v2: Val) extends Val
case class Left(v: Val) extends Val
case class Right(v: Val) extends Val
case class Stars(vs: List[Val]) extends Val
case class Rec(x: String, v: Val) extends Val
case class PlusV(vs: List[Val]) extends Val //1 or more times
case class OptionalV(v: Val) extends Val //0 or 1 times
case class NTimesV(vs: List[Val]) extends Val
case class RangeV(c: Char) extends Val

// Convenience for typing
def charlist2rexp(s: List[Char]): Rexp = s match {
  case Nil      => ONE
  case c :: Nil => CHAR(c)
  case c :: s   => SEQ(CHAR(c), charlist2rexp(s))
}

implicit def string2rexp(s: String): Rexp =
  charlist2rexp(s.toList)

extension (s: String) {
  def |(r: Rexp) = ALT(s, r)
  def |(r: String) = ALT(s, r)
  def % = STAR(s)
  def ~(r: Rexp) = SEQ(s, r)
  def ~(r: String) = SEQ(s, r)
  def $(r: Rexp) = RECD(s, r)
}

extension (r: Rexp) {
  def ~(s: Rexp) = SEQ(r, s)
  def % = STAR(r)
  def |(s: Rexp) = ALT(r, s)
}

// nullable
def nullable(r: Rexp): Boolean = r match {
  case ZERO        => false
  case ONE         => true
  case CHAR(_)     => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_)     => true
  // extended
  case RECD(x, r)   => nullable(r)
  case RANGE(cs)    => false
  case PLUS(r)      => nullable(r)
  case OPTIONAL(r)  => true
  case NTIMES(r, n) => if (n == 0) true else nullable(r)
}

// der
def der(c: Char, r: Rexp): Rexp = r match {
  case ZERO        => ZERO
  case ONE         => ZERO
  case CHAR(d)     => if (c == d) ONE else ZERO
  case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
  case SEQ(r1, r2) =>
    if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
    else SEQ(der(c, r1), r2)
  case STAR(r) => SEQ(der(c, r), STAR(r))

  // extended
  case RANGE(characters) => if (characters.contains(c)) ONE else ZERO
  case PLUS(r)           => SEQ(der(c, r), STAR(r))
  case OPTIONAL(r)       => der(c, r)
  case NTIMES(r, i) => if (i == 0) ZERO else SEQ(der(c, r), NTIMES(r, i - 1))
  case RECD(x, r)   => der(c, r)

}

// Flatten
def flatten(v: Val): String = v match {
  case Empty        => ""
  case Chr(c)       => c.toString
  case Left(v)      => flatten(v)
  case Right(v)     => flatten(v)
  case Sequ(v1, v2) => flatten(v1) + flatten(v2)
  case Stars(vs)    => vs.map(flatten).mkString
  case PlusV(vs)    => vs.map(flatten).mkString
  case OptionalV(v) => flatten(v)
  case NTimesV(vs)  => vs.map(flatten).mkString
  case RangeV(cs)   => cs.toString()
}

// Env
def env(v: Val): List[(String, String)] = v match {
  case Empty        => Nil
  case Chr(c)       => Nil
  case Left(v)      => env(v)
  case Right(v)     => env(v)
  case Sequ(v1, v2) => env(v1) ::: env(v2)
  case Stars(vs)    => vs.flatMap(env)
  case Rec(x, v)    => (x, flatten(v)) :: env(v)
  // extended
  case RangeV(cs)   => Nil
  case PlusV(vs)    => vs.flatMap(env)
  case OptionalV(v) => env(v)
  case NTimesV(vs)  => vs.flatMap(env)

}

// Mkeps
def mkeps(r: Rexp): Val = r match {
  case ONE => Empty
  case ALT(r1, r2) =>
    if (nullable(r1)) Left(mkeps(r1)) else Right(mkeps(r2))
  case SEQ(r1, r2) => Sequ(mkeps(r1), mkeps(r2))
  case STAR(r)     => Stars(Nil)
  // extended:
  // case RANGE(characters) => cant ever be nullable so this wont be called.
  case RECD(x, r) => Rec(x, mkeps(r))

  case PLUS(r) =>
    PlusV(List(mkeps(r))) // one or more times so we need to check each value
  case OPTIONAL(r) =>
    OptionalV(
      Empty
    )
  case NTIMES(r, n) =>
    if (n == 0) NTimesV(Nil)
    else
      NTimesV(
        List.fill(n)(mkeps(r))
      ) // if n is 0 then it wasn't present so we get Empty, but if n>0 then
}

// Inj
def inj(r: Rexp, c: Char, v: Val): Val = (r, v) match {
  case (CHAR(d), Empty)                  => Chr(c)
  case (ALT(r1, r2), Left(v1))           => Left(inj(r1, c, v1))
  case (ALT(r1, r2), Right(v2))          => Right(inj(r2, c, v2))
  case (SEQ(r1, r2), Sequ(v1, v2))       => Sequ(inj(r1, c, v1), v2)
  case (SEQ(r1, r2), Left(Sequ(v1, v2))) => Sequ(inj(r1, c, v1), v2)
  case (SEQ(r1, r2), Right(v2))          => Sequ(mkeps(r1), inj(r2, c, v2))
  case (STAR(r), Sequ(v1, Stars(vs)))    => Stars(inj(r, c, v1) :: vs)
  // extended:
  case (RECD(x, r), _)                       => Rec(x, inj(r, c, v))
  case (PLUS(r), Sequ(v1, Stars(vs)))        => PlusV(inj(r, c, v1) :: vs)
  case (OPTIONAL(r), _)                      => OptionalV(inj(r, c, v))
  case (NTIMES(r, n), Sequ(v1, NTimesV(vs))) => NTimesV(inj(r, c, v1) :: vs)
  case (NTIMES(r, 0), _)                     => NTimesV(Nil)
  case (RANGE(cs), Empty)                    => RangeV(c)
}

// Rectification functions
def F_ID(v: Val): Val = v
def F_RIGHT(f: Val => Val) = (v: Val) => Right(f(v))
def F_LEFT(f: Val => Val) = (v: Val) => Left(f(v))
def F_ALT(f1: Val => Val, f2: Val => Val) = (v: Val) =>
  v match {
    case Right(v) => Right(f2(v))
    case Left(v)  => Left(f1(v))
  }
def F_SEQ(f1: Val => Val, f2: Val => Val) = (v: Val) =>
  v match {
    case Sequ(v1, v2) => Sequ(f1(v1), f2(v2))
  }
def F_SEQ_Empty1(f1: Val => Val, f2: Val => Val) =
  (v: Val) => Sequ(f1(Empty), f2(v))
def F_SEQ_Empty2(f1: Val => Val, f2: Val => Val) =
  (v: Val) => Sequ(f1(v), f2(Empty))
def F_RECD(f: Val => Val) = (v: Val) =>
  v match {
    case Rec(x, v) => Rec(x, f(v))
  }
def F_ERROR(v: Val): Val = throw new Exception("error")

// Simp
def simp(r: Rexp): (Rexp, Val => Val) = r match {
  case ALT(r1, r2) => {
    val (r1s, f1s) = simp(r1)
    val (r2s, f2s) = simp(r2)
    (r1s, r2s) match {
      case (ZERO, _) => (r2s, F_RIGHT(f2s))
      case (_, ZERO) => (r1s, F_LEFT(f1s))
      case _ =>
        if (r1s == r2s) (r1s, F_LEFT(f1s))
        else (ALT(r1s, r2s), F_ALT(f1s, f2s))
    }
  }
  case SEQ(r1, r2) => {
    val (r1s, f1s) = simp(r1)
    val (r2s, f2s) = simp(r2)
    (r1s, r2s) match {
      case (ZERO, _) => (ZERO, F_ERROR)
      case (_, ZERO) => (ZERO, F_ERROR)
      case (ONE, _)  => (r2s, F_SEQ_Empty1(f1s, f2s))
      case (_, ONE)  => (r1s, F_SEQ_Empty2(f1s, f2s))
      case _         => (SEQ(r1s, r2s), F_SEQ(f1s, f2s))
    }
  }
  case r => (r, F_ID)
}

// Lex
def lex_simp(r: Rexp, s: List[Char]): Val = s match {
  case Nil =>
    if (nullable(r)) mkeps(r) else { throw new Exception("lexing error") }
  case c :: cs => {
    val (r_simp, f_simp) = simp(der(c, r))
    inj(r, c, f_simp(lex_simp(r_simp, cs)))
  }
}

def ders_simp(cs: List[Char], r: Rexp): Rexp = cs match {
  case Nil     => r
  case c :: cs => ders_simp(cs, simp(der(c, r))._1)
}

def lexing_simp(r: Rexp, s: String) = env(lex_simp(r, s.toList))

val lowercaseLetters = ('a' to 'z').toSet
val upercaseLetters = ('A' to 'Z').toSet
val allLetters = lowercaseLetters ++ upercaseLetters
val additionalSymbols = ("._><=;,\\:").toSet
val digits = ('0' to '9').toSet
val digitsExcludingZero = ('1' to '9').toSet

// Language specific code
val KEYWORD: Rexp = "if" | "then" | "else" | "write" | "def" | "val"
val OP: Rexp =
  "=" | "==" | "-" | "+" | "*" | "!=" | "<" | ">" | "<=" | ">=" | "%" | "/"

val LET: Rexp = RANGE(allLetters)
val SYM: Rexp = RANGE(
  allLetters ++ additionalSymbols
)
val RPAREN: Rexp = ")"
val LPAREN: Rexp = "("
val CURLYLEFT: Rexp = "{"
val CURLYRIGHT: Rexp = "}"
val PARENS = RPAREN | LPAREN | CURLYLEFT | CURLYRIGHT

val DIGIT: Rexp = RANGE(digits)
val ID: Rexp = LET ~ ("_" | LET | DIGIT).%

val NUM: Rexp = PLUS(DIGIT)

val DOUBLE: Rexp = OPTIONAL("-") ~ NUM ~ "." ~ (DIGIT).%

val SEMI: Rexp = ";"
val WHITESPACE: Rexp =
  PLUS(" ") | "\n" | "\t" | "\r" // one or more of all of them?

val COMMA: Rexp = ","
val ALL = SYM | DIGIT | OP | " " | ":" | ";" | "\"" | "=" | "," | "(" | ")"
val ALL2 = ALL | "\n"
val CHARACTER: Rexp = "'" ~ ALL ~ "'" | "'\\n'"

val STRING: Rexp = "\"" ~ (SYM | WHITESPACE | DIGIT).% ~ "\""
val EOL: Rexp = "\n" | "\r\n"
val COMMENT = ("/*" ~ ALL2.% ~ "*/") | ("//" ~ ALL.% ~ EOL)
val TYPE: Rexp = "Int" | "Double" | "Void"
val COLON: Rexp = ":"
// val APOST: Rexp = "'"

val FUN_REGS = (("k" $ KEYWORD) |
  ("type" $ TYPE) |
  ("o" $ OP) |
  ("ch" $ CHARACTER) |
  ("str" $ STRING) |
  ("dbl" $ DOUBLE) |
  ("s" $ SEMI) |
  ("c" $ COMMA) |
  ("colon" $ COLON) |
  ("pl" $ LPAREN) |
  ("pr" $ RPAREN) |
  ("cl" $ CURLYLEFT) |
  ("cr" $ CURLYRIGHT) |
  // ("d" $ DIGIT) |
  ("n" $ NUM) |
  ("i" $ ID) |
  ("comm" $ COMMENT) |
  ("w" $ (WHITESPACE | COMMENT))).%

def esc(raw: String): String = {
  import scala.reflect.runtime.universe._
  Literal(Constant(raw)).toString
}

def escape(tks: List[(String, String)]) =
  tks.map { case (s1, s2) => (s1, esc(s2)) }

// Token
abstract class Token extends Serializable
case class T_KWD(s: String) extends Token
case class T_OP(s: String) extends Token
case class T_STRING(s: String) extends Token
case object T_SEMI extends Token
case class T_ID(s: String) extends Token
case class T_TYPE(s: String) extends Token

case class T_NUM(n: Int) extends Token
case class T_DOUBLE(n: Double) extends Token

//extended
case class T_CHARACTER(ASCII: Int) extends Token

case object T_LPAREN extends Token
case object T_RPAREN extends Token
case object T_CURLYL extends Token
case object T_CURLYR extends Token
case object T_COLON extends Token
case object T_COMMA extends Token

case class T_COMMENTANDWHITESPACE(s: String) extends Token

val token: PartialFunction[(String, String), Token] = {
  case ("k", s)   => T_KWD(s)
  case ("o", s)   => T_OP(s)
  case ("str", s) => T_STRING(s)

  case ("pl", _) => T_LPAREN
  case ("pr", _) => T_RPAREN
  case ("cl", _) => T_CURLYL
  case ("cr", _) => T_CURLYR

  case ("s", _)     => T_SEMI
  case ("colon", _) => T_COLON
  case ("c", _)     => T_COMMA

  case ("i", s)    => T_ID(s)
  case ("type", s) => T_TYPE(s)

  case ("dbl", s) => T_DOUBLE(s.toDouble)
  case ("n", s)   => T_NUM(s.toInt)
  case ("w", s)   => T_COMMENTANDWHITESPACE(s)
  case ("ch", s) => {
    if (s == "'\\n'") {
      T_CHARACTER('\n'.toInt)
    } else if (s.length == 3) {
      T_CHARACTER(s.charAt(1).toInt)
    } else {
      throw new RuntimeException("Invalid character")
    }
  }

  // else if (s.length == 4) {}

  // if (char == '\\') {
  //   s.charAt(2) match {
  //     case 'n' => T_CHARACTER('\n'.toInt)
  //     case _   => T_CHARACTER(char.toInt)
  //   }
  // } else {
  //   T_CHARACTER(char.toInt)
  // }
}
// T_CHARACTER(s.charAt(1).toInt)

def tokenise(s: String): List[Token] = {
  val tks = lexing_simp(FUN_REGS, s).collect(token)
  val filtered = tks.filter {
    // case T_WHITESPACE(_) => false
    case T_COMMENTANDWHITESPACE(_) => false
    case _                         => true
  }
  if (filtered.length != 0) filtered
  else { println(s"Tokenise Error"); sys.exit(-1) }
}

@main
def main(fname: String) = {
  println(tokenise(os.read(os.pwd / fname)))
}

println("Imported tokeniser")

// val tokens = tokenise(os.read(os.pwd / "loops.while"))
// println(tokens)
