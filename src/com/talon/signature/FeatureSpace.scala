package com.talon.signature
import com.talon.Constants
object FeatureSpace {
  
  def features(sender : String = "") : Array[String => Int] = {
    Array(
      // This one isn't from paper.
      // Meant to match companies names, sender's names, address.
      Helpers.many_capitalized_words,
      // This one is not from paper.
      // Line is too long.
      // This one is less aggressive than `Line is too short`
      (line : String) => if(line.size > Constants.TOO_LONG_SIGNATURE_LINE) 1 else 0,
      // Line contains email pattern.
      Helpers.binary_regex_search(Constants.RE_EMAIL),
      // Line contains url.
      Helpers.binary_regex_search(Constants.RE_URL),
      // Line contains phone number pattern.
      Helpers.binary_regex_search(Constants.RE_RELAX_PHONE),
      // Line matches the regular expression "^[\s]*---*[\s]*$".
      Helpers.binary_regex_match(Constants.RE_SEPARATOR),
      // Line has a sequence of 10 or more special characters.
      Helpers.binary_regex_search(Constants.RE_SPECIAL_CHARS),
      // Line contains any typical signature words.
      Helpers.binary_regex_search(Constants.RE_SIGNATURE_WORDS),
      // Line contains a pattern like Vitor R. Carvalho or William W. Cohen.
      Helpers.binary_regex_search(Constants.RE_NAME),
      // Percentage of punctuation symbols in the line is larger than 50%
      (line : String) => if(Helpers.punctuation_percent(line) > 50) 1 else 0,
      // Percentage of punctuation symbols in the line is larger than 90%
      (line : String) => if(Helpers.punctuation_percent(line) > 90) 1 else 0,
      Helpers.contains_sender_names(sender)
    )
  }
  
  
  def apply_features(body : String,features : Array[String => Int]) : Array[Int] = {
    /*
     * Applies features to message body lines.

    Returns list of lists. Each of the lists corresponds to the body line
    and is constituted by the numbers of features occurrences (0 or 1).
    E.g. if element j of list i equals 1 this means that
    feature j occurred in line i (counting from the last line of the body).
     */
    //collect all non empty lines
//     val lines : Array[String] = body.split("\n").filter(e=> e.size>0)
     
    //take the last SIGNATURE_MAX_LINES
//     val last_lines : Array[String] = lines.slice(lines.size-Constants.SIGNATURE_MAX_LINES,lines.size)
//     println("Featurespace apply_features : "+features.map(e=> e(body)).mkString(",")+" "+body)
     if(body.size > 0) features.map(e=> e(body)) else Array.fill[Int](features.size)(0)
     
  }

  
  def build_pattern(body : String,features : Array[String => Int]) : Array[Int] = {
    /*
     * Converts body into a pattern i.e. a point in the features space.

    Applies features to the body lines and sums up the results.
    Elements of the pattern indicate how many times a certain feature occurred
    in the last lines of the body.
     */
   
    val line_patterns : Array[Int] = apply_features(body, features)
    line_patterns   
  }
}