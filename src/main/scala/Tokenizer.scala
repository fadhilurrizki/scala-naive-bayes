/**
  * Created by fadhilurrizki on 08/08/17.
  */
object Tokenizer {
  def tokenize(sentence: String): List[String] = {
    val sentenceBuffer: String = sentence.toLowerCase()
    var tokenBuffer = pretokenize(sentenceBuffer)
    tokenBuffer.filter(word => isAlphaNumeric(word) && !word.startsWith("http") && !(word.length<=1 && !word.equals("a") && !word.equals("i")))
  }

  def pretokenize(sentence: String): List[String] = {
    sentence
      .replaceAll("([^a-zA-Z0-9])", "|$1|")
      .replaceAll("(\\d)\\|([\\.\\/])\\|(\\d)", "$1$2$3")
      .replaceAll("([^\\s])\\|([\\-])\\|([^\\s])", "$1$2$3")
      .replaceAll("([A-Za-z])\\1+", "$1")
      .split("\\|")
      .toList
      .filter(!List(" ", "\r", "\n", "\t", "").contains(_))
  }

  def isASCII(text: String): Boolean = {
    """^[\x00-\x7F]+$""".r.pattern.matcher(text).matches
  }

  def isAlphaNumeric(text: String): Boolean = {
    """^[a-zA-Z0-9]+""".r.pattern.matcher(text).matches
  }
}
