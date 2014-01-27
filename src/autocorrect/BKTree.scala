package autocorrect
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer

class BKTree(val str: String) {
  var children: Map[Int, BKTree] = Map[Int, BKTree]()
  
  def add(word: String): Unit = {
    val dist = Utils.levenshtein(str, word)
    if (children contains dist) children(dist) add word
    else children(dist) = new BKTree(word)
  }
  
  def find(word: String, tolerance: Int, results: ArrayBuffer[(Int, String)]): ArrayBuffer[(Int, String)] = {
    val dist = Utils.levenshtein(str, word)
    if (dist <= tolerance) results.append((dist, str))
    for(i <- (dist - tolerance) to (dist + tolerance) if children contains i) {
      children(i).find(word, tolerance, results)
    }
    results
  }
  
  def suggest(word: String, tolerance: Int):ArrayBuffer[String] = {
	val results = find(word, tolerance, ArrayBuffer[(Int, String)]()).sorted
	for((_, word) <- results) yield word
  }
  
  val += :(String) => Unit = add
}