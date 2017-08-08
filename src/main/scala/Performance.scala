/**
  * Created by fadhilurrizki on 08/08/17.
  */
object Performance {
  def accuracy(output : List[Int], target : List[Int]) : Double = {
    var result : Double = 0.0
    var count = 0
    for (i <- 0 to output.size-1) {
      if(output(i)==target(i)) {
        count += 1
      }
    }
    count * 100 / output.size
  }
}
