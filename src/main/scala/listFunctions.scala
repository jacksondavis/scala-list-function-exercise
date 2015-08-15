package listMethods

sealed trait List[+A] {
  override def toString = {
    def toScalaList(t: List[A]): scala.List[A] = t match {
      case Empty => Nil
      case Cons(h, t) => h :: toScalaList(t)
    }
    toScalaList(this).toString
  }
}

final case object Empty extends List[Nothing]
final case class Cons[A](h: A, t: List[A]) extends List[A]

object List {
  def foldRight[A, B](as: List[A], b: B, f: (A, B) => B): B = as match {
    case Empty => b
    case Cons(h, t) => f(h, foldRight(t, b, f))
  }

  def foldLeft[A, B](as: List[A], b: B, f: (B, A) => B): B = as match {
    case Empty => b
    case Cons(h, t) => foldLeft(t, f(b, h), f)
  }

  def reduceRight[A](as: List[A], f: (A, A) => A): A = as match {
    case Empty => error("bzzt. reduceRight on empty list")
    case Cons(h, t) => foldRight(t, h, f)
  }

  def reduceLeft[A](as: List[A], f: (A, A) => A): A = as match {
    case Empty => error("bzzt. reduceLeft on empty list")
    case Cons(h, t) => foldLeft(t, h, f)
  }

  def unfold[A, B](b: B, f: B => Option[(A, B)]): List[A] = f(b) match {
    case Some((a, b)) => Cons(a, unfold(b, f))
    case scala.None => Empty
  }
}

sealed trait Natural {
  override def toString = {
    def toInt(n: Natural): Int = n match {
      case Zero => 0
      case Succ(x) => 1 + toInt(x)
    }
    toInt(this).toString
  }
}
final case object Zero extends Natural
final case class Succ(c: Natural) extends Natural

object Exercises extends App {

  // Exercise 1
  // Relative Difficulty: 1
  // Correctness: 2.0 marks
  // Performance: 0.5 mark
  // Elegance: 0.5 marks
  // Total: 3
  def add(x: Natural, y: Natural): Natural = {
    y match {
      case Zero => x
      case Succ(z) => add(Succ(x), z)
    }
  }

  // Exercise 2
  // Relative Difficulty: 2
  // Correctness: 2.5 marks
  // Performance: 1 mark
  // Elegance: 0.5 marks
  // Total: 4
  def sum(is: List[Int]): Int = List.foldLeft(is, 0, (ans: Int, in: Int) => ans + in)
   
  // Exercise 3
  // Relative Difficulty: 2
  // Correctness: 2.5 marks
  // Performance: 1 mark
  // Elegance: 0.5 marks
  // Total: 4
  def length[A](as: List[A]): Int = List.foldLeft(as, 0, (ans: Int, in: A) => ans + 1) 

  // Exercise 4
  // Relative Difficulty: 5
  // Correctness: 4.5 marks
  // Performance: 1.0 mark
  // Elegance: 1.5 marks
  // Total: 7
  def map[A, B](as: List[A], f: A => B): List[B] = List.foldLeft(as, Empty, (ans: List[B], in: A) => Cons(f(in), ans)) 

  // Exercise 5
  // Relative Difficulty: 5
  // Correctness: 4.5 marks
  // Performance: 1.5 marks
  // Elegance: 1 mark
  // Total: 7
  def filter[A](as: List[A], f: A => Boolean): List[A] = List.foldLeft(as, Empty, (ans: List[A], in: A) => {
    if(f(in)) Cons(in, ans)
    else ans
  })
    
  // Exercise 6
  // Relative Difficulty: 5
  // Correctness: 4.5 marks
  // Performance: 1.5 marks
  // Elegance: 1 mark
  // Total: 7
  def append[A](x: List[A], y: List[A]): List[A] = List.foldRight(y, x, (r:A, c:List[A]) => Cons(r, c))

  // Exercise 7
  // Relative Difficulty: 5
  // Correctness: 4.5 marks
  // Performance: 1.5 marks
  // Elegance: 1 mark
  // Total: 7
  def flatten[A](as: List[List[A]]): List[A] = List.foldLeft(as, Empty, (r:List[A], c:List[A]) => append(r,c))

  // Exercise 8
  // Relative Difficulty: 7
  // Correctness: 5.0 marks
  // Performance: 1.5 marks
  // Elegance: 1.5 mark
  // Total: 8
  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] = flatten(map(as, f))

  // Exercise 9
  // Relative Difficulty: 8
  // Correctness: 3.5 marks
  // Performance: 3.0 marks
  // Elegance: 2.5 marks
  // Total: 9
  def maximum(is: List[Int]): Int = List.foldLeft(is, 0, (ans: Int, in: Int) => {
    if(in > ans) in
    else ans
  })

  // Exercise 10
  // Relative Difficulty: 10
  // Correctness: 5.0 marks
  // Performance: 2.5 marks
  // Elegance: 2.5 marks
  // Total: 10
  def reverse[A](as: List[A]): List[A] = List.foldLeft(as, Empty, (ans: List[A], in: A) => Cons(in, ans)) 

  val testList = Cons(1,Cons(2,Cons(3,Cons(4,Empty))))
  val secList = Cons(5, Cons(6, Cons(8, Cons(7, Empty))))
  val flatList = Cons(testList,Cons(secList,Empty))
  
  def testFilt(in: Int): Boolean = {
    if(in > 2) true
    else false
  }

  def testMap(in: Int): Int = {
    in + 2
  }

  println(add(Succ(Zero),Succ(Succ(Zero))))
  println(sum(testList))
  println(length(testList))
  println(length(testList))
  println(filter(testList, testFilt))
  println(map(testList,testMap))
  println(append(secList, testList))
  println(flatten(flatList))
  println(reverse(testList))
  println(maximum(testList))
  println(maximum(secList))
}