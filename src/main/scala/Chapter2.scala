object Chapter2{
  // EX1
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, next: Int): Int =
      if (n == 0) prev
      else loop(n - 1, next, prev + next)

    loop(n, 0, 1)
  }

  // 型の抽象化
  // 配列内で文字列を検索する関数
  def searchString(words: Array[String], key: String): Int = {
    @annotation.tailrec
    // {}をつけるとコンパイルエラーになる。付けないのが正解らしい。なぜ？
    def loop(n: Int): Int =
      if(n >= words.length) - 1
      else if (words(n) == key) n
      else loop(n + 1)
    loop(0)
  }

  // 共通型検索
  // 引数を抽象化する場合、メソッド名直後にAなどつける
  // p: A => Boolean　これなんだ？
  def commonSearch[A](any: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if(n >= any.length) -1
      else if (p(any(n))) n
      else loop(n + 1)
    loop(0)
  }

  // EX2
  // 指定された比較関数にしたがってArray[A]がソートされているか確認する関数
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n : Int): Boolean =
      if(as.length < 2) true
      else if (ordered(as(n), as(n + 1))) false
      else loop(n + 1)
    loop(0)
  }

  // Ex3
  // カリー化の練習
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  // EX4
  // アンカリー
  // これ何・・・・？
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a,b) => f(a)(b)

  // EX5
  // 合成関数
  // これはできた
  // Bの型として再生されたgの関数を引数としてfに代入するだけ
  // 結果、そのままaだけ引数としてとれば良いと推理できた
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}