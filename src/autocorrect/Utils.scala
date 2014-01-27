package autocorrect

import java.sql.Time

object Utils {
	def levenshtein(word1: String, word2: String): Int = {
	  val (m, n) = (word1.size, word2.size)
	  var arr = Array.fill(m+1, n+1)(0)
	  for(i <- 0 to n) arr(0)(i) = i
	  for(i <- 0 to m) arr(i)(0) = i
	  for(i <- 1 to m; j <- 1 to n) {
	    val flag = if (word1(i-1) == word2(j-1)) 0 else 1
	    arr(i)(j) = min3(arr(i-1)(j) + 1, arr(i)(j-1) + 1, arr(i-1)(j-1) + flag)
	  }
	  arr(m)(n)
	}
	
	def min3(a: Int, b: Int, c: Int) : Int = {
	  Math.min(a, Math.min(b, c))
	}
	
	def makeBKTree(filename: String): BKTree = {
	  val lines = scala.io.Source.fromFile(filename).getLines
	  val root = lines.next.trim
	  var tree = new BKTree(root)
	  for(line <- lines) {
	    tree += line.trim
	  }
	  tree
	}
	
	def main(args: Array[String]): Unit = {
	  val start = System.nanoTime()
	  var tree = makeBKTree("/usr/share/dict/words")
	  println(tree.suggest("spell", 1))
	  val end = System.nanoTime()
	  println(1e-9 * (end - start))
	}
	
}