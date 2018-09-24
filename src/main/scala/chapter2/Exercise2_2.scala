object Exercise2_2 {

  @annotation.tailrec
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    if (as.length <= 1)
        true
    else
        ordered(as(0),as(1)) && isSorted(as.drop(1), ordered)
  }

  def main(args: Array[String]): Unit = {
    println(isSorted(Array("aap", "boom", "chocolade"), (a:String, b:String) => a < b ))
    println(isSorted(Array(1, 7, 4), (a:Int, b:Int) => a < b))
  }
}
