/**
  * Created by fadhilurrizki on 08/08/17.
  */
object Main {
  def main(args : Array[String]): Unit = {
    val (content, label) = Loader.loadTxt(args(0))
    val num_data_class = 1000
    val num_training = (0.7 * num_data_class).toInt

    val (data_pos, data_neg) = content.splitAt(num_data_class)
    val (label_pos, label_neg) = label.splitAt(num_data_class)

    val (data_pos_train, data_pos_test) = data_pos.splitAt(num_training)
    val (data_neg_train, data_neg_test) = data_neg.splitAt(num_training)

    val (label_pos_train, label_pos_test) = label_pos.splitAt(num_training)
    val (label_neg_train, label_neg_test) = label_neg.splitAt(num_training)

    val data_train = data_pos_train ::: data_neg_train
    val label_train = label_pos_train ::: label_neg_train
    val data_test = data_pos_test ::: data_neg_test
    val label_test = label_pos_test ::: label_neg_test

    val token_train = data_train.map(sent => Tokenizer.tokenize(sent))
    val token_test = data_test.map(sent => Tokenizer.tokenize(sent))

    NaiveBayes.fit(token_train, label_train)

    val pred = NaiveBayes.predictAll(token_test)

    val acc = Performance.accuracy(pred, label_test)

    println(acc)
  }
}
