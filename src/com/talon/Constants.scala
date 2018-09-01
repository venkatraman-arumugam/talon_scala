package com.talon

import scala.util.matching.Regex
import java.util.regex.Pattern

object Constants {
  final val rc  = (x : String) => x.r
  final val RE_DELIMITER : Regex = rc("\\r?\\n")
  final val SIGNATURE_MAX_LINES : Int = 11
  final val TOO_LONG_SIGNATURE_LINE : Int  = 60
  final val RE_SIGNATURE_CANDIDATE : Pattern = Pattern.compile("(?<candidate>c+d)[^d]|(?<candidate1>c+d)$|(?<candidate2>c+)|(?<candidate3>d)[^d]|(?<candidate4>d)$")
  //regex signature pattern for reversed lines
  //assumes that all long lines have been excluded
  //RE_REVERSE_SIGNATURE = re.compile(r'''
  //signature should consists of blocks like this
  final val RE_REVERSE_SIGNATURE : Regex = rc("(?:e*(te*){0,2}s)+")
  
  final val RE_EMAIL : Regex= rc("\\S@\\S")
  final val RE_RELAX_PHONE : Regex = rc("(\\(? ?[\\d]{2,3} ?\\)?.{0,3}?){2,}")
  final val RE_URL : Regex = rc("https?://|www\\.[\\S]+\\.[\\S]")
  //Taken from:
  //http://www.cs.cmu.edu/~vitor/papers/sigFilePaper_finalversion.pdf
  //Line matches the regular expression "^[\s]*---*[\s]*$".
  final val RE_SEPARATOR : Pattern = Pattern.compile("^[\\s]*---*[\\s]*$")
  //Taken from:
  //http://www.cs.cmu.edu/~vitor/papers/sigFilePaper_finalversion.pdf
  //Line has a sequence of 10 or more special characters.
  final val RE_SPECIAL_CHARS : Regex = rc(("^[\\s]*([\\*]|#|[\\+]|[\\^]|-|[\\~]|[\\&]|[\\$]|_|[\\!]|" +
                                           "[\\/]|[\\%]|[\\:]|[\\=]){10,}[\\s]*$"))

  final val RE_SIGNATURE_WORDS : Regex = rc(("(T|t)hank.*,|(B|b)est|(R|r)egards|"+
                                             "^sent[ ]{1}from[ ]{1}my[\\s,!\\w]*$|BR|(S|s)incerely|"+
                                             "(C|c)orporation|Group"))

  //Taken from:
  //http://www.cs.cmu.edu/~vitor/papers/sigFilePaper_finalversion.pdf
  //Line contains a pattern like Vitor R. Carvalho or William W. Cohen.
  final val RE_NAME : Regex = rc("[A-Z][a-z]+\\s\\s?[A-Z][\\.]?\\s\\s?[A-Z][a-z]+")

  final val INVALID_WORD_START : Regex = rc("\\(|\\+|[\\d]")

  final val BAD_SENDER_NAMES : Array[String] = Array(
  //known mail domains
    "hotmail", "gmail", "yandex", "mail", "yahoo", "mailgun", "mailgunhq",
    "example",
  //first level domains
    "com", "org", "net", "ru",
  //bad words
    "mailto"
  )
}