// a simple factorial program
// (including a tail recursive version)

val Ymin: Double = -1.3;
val Ymax: Double =  1.3;
val Ystep: Double = 0.05;  //0.025;

val Xmin: Double = -2.1;
val Xmax: Double =  1.1;
val Xstep: Double = 0.02;  //0.01;
def m_iter(m: Int, x: Double, y: Double,
                   zr: Double, zi: Double) : Void = {
  if Maxiters <= m
  then print_star() 
  else {
    if 4.0 <= zi*zi+zr*zr then print_space() 
    else m_iter(m + 1, x, y, x+zr*zr-zi*zi, 2.0*zr*zi+y) 
  }
};


def fact(n: Int) : Int =
  if n == 0 then 1 else n * fact(n - 1);

def facT(n: Int, acc: Int) : Int =
  if n == 0 then acc else facT(n - 1, n * acc);

def facTi(n: Int) : Int = facT(n, 1);

def top() : Void = {
  print_int(fact(6));
  print_char(',');
  print_int(facTi(6));
  print_char('\n')
};

top()