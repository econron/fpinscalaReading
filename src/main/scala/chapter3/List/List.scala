package chapter3.List

// traitをsealedすることでこのファイル内でしかListを宣言できなくする
sealed trait List[+A]

// ListでNilを宣言できるようにする
case object Nil extends List[Nothing]

// headにA型のデータ、tailにA型のリストをもつデータ構造
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    // 再帰関数としてsum関数を繰り返し利用することで
    // 渡されたリストを繰り返し足し算している
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 0
    case Cons(0.0, _) => 0.0
    // 自分を再帰的に呼び出すことで、List内の関数をループなしで
    // 掛け算している
    case Cons(x, xs) => x * product(xs)
  }

  // 単方向連結リストに要素を追加する処理
  def apply[A](as: A*): List[A] =
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}


