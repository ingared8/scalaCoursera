
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
      "Text: " + text + " [" + retweets + "]"
}

abstract  class IntSet {

  def incl(x:Int): IntSet
  def contains(x:Int): Boolean
  def union(x:IntSet):IntSet

}

class Empty extends  IntSet {

  def incl(x: Int): IntSet =  new NonEmpty(x,new Empty, new Empty)

  def contains(x: Int): Boolean = false

  override def toString: String = "."

  def union(x:IntSet):IntSet =  x
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends  IntSet {

  def incl(x: Int): IntSet = {
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this
  }

  def contains(x: Int): Boolean = {
    if (x < elem) left.contains(x)
    else if (x > elem) right.contains(x)
    else true
  }

  def union(x:IntSet):IntSet = {
    (left union right) union x incl elem
  }

  override def toString:String = {
    "{" + left + elem + right + "}"
  }

}

object IntSetA {

}

val tweet = new Tweet("Ganga","Hello World ", 10)
val t1 = new NonEmpty(4,new Empty, new Empty)
def error(mess:String) = throw  new Exception(mess)




