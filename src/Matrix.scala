/**
 * Created by Tank on 4/20/2016.
 */
class Matrix(val matrix: Array[Array[Double]]){

  def*(otherMatrix: Matrix): Matrix = {
    if(otherMatrix.matrix.length != matrix(0).length) throw new Exception("Inner Dimensions Mismatch")
    val product = Array.ofDim[Double](matrix.length, otherMatrix.matrix(0).length)
    for(i <- 0 until product.length){
      for(j <- 0 until product(i).length){
        product(i)(j) = (for(v <- (matrix(i) zip otherMatrix.T.matrix(j))) yield {v._1*v._2}).sum
      }
    }
    new Matrix(product)
  }
  def T(): Matrix = {
    val rows = matrix.length
    val cols = matrix(0).length
    new Matrix((for(i <- 0 until cols) yield {
      val y = (for(j <- 0 until rows)
        yield matrix(j)(i)
        ).toArray
      y
    }).toArray)
  }

override def toString(): String ={
  var s = ""
  val rows = matrix.length
  val cols = matrix(0).length
  for(i <- 0 until rows){
    for(j <- 0 until cols){
      s = s+matrix(i)(j) + " "
      }
    s = s + "\n"
  }
  s
}






}
object test {
  def main(args: Array[String]) {
    val v = new Matrix(Array(Array(1, 2, 3, 4, 5), Array(6, 7, 8, 9, 0)))
    val w = new Matrix(Array(Array(1, 0, 5), Array(0, 1), Array(0, 0), Array(0, 0), Array(0, 0)))
    print(v * w)
  }
}
