package com.talon.signature

import com.talon.Constants
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import java.util.regex.Pattern
import util.control.Breaks._

object Helpers {

  def binary_regex_search(prog : Regex): (String => Int) = {
    /*
     * Returns a function that returns 1 or 0 depending on regex search result.

    If regular expression compiled into prog is present in a string
    the result of calling the returned function with the string will be 1
    and 0 otherwise.

    >>> import regex as re
    >>> binary_regex_search(re.compile("12"))("12")
    1
    >>> binary_regex_search(re.compile("12"))("34")
    0
     */
    (x : String) => prog.findFirstIn(x) match {
                    case None => 0
                    case _ => 1
                  }
  }
  
  def binary_regex_match(prog : Pattern): (String => Int) = {
    /*
     * Returns a function that returns 1 or 0 depending on regex match result.

    If a string matches regular expression compiled into prog
    the result of calling the returned function with the string will be 1
    and 0 otherwise.

    >>> import regex as re
    >>> binary_regex_match(re.compile("12"))("12 3")
    1
    >>> binary_regex_match(re.compile("12"))("3 12")
    0
     */
    (x : String) => if(prog.matcher(x).matches()) 1 else 0
  }
  
  def capitalized_words_percent(s : String) : Double = {
    //Returns capitalized words percent
    val words : ListBuffer[String] =  new ListBuffer()
    s split("\\s+") foreach(e=>{
        if(e.trim.size > 2){
          words += (e)
        }
    })
    
    words --= words filter(_.trim.size < 0)
    words --= words filter(_.trim.size < 2)    
    var capitalized_words_counter = 0
    var valid_words_counter = 0
    words.map( word => {
       
        if(Constants.INVALID_WORD_START.findFirstIn(word).size == 0){
          valid_words_counter += 1
          if(word.charAt(0).isUpper && !word.charAt(1).isUpper)
                capitalized_words_counter += 1
        }           
    })
    
    if(valid_words_counter > 0 && words.size > 1){
       100.toDouble * (capitalized_words_counter.toDouble / valid_words_counter.toDouble)
    }else{
      val zero = 0
      zero.toDouble
    }
      
  }
  
  def many_capitalized_words : (String => Int) = {
    /*
     * Returns a function to check percentage of capitalized words.
			The function returns 1 if percentage greater then 65% and 0 otherwise.
     */
     
    (s : String) => if(capitalized_words_percent(s) > 66) 1 else 0
  }
  
  def has_signature(body : String,sender :String) : Boolean = {
    val non_empty : Array[String] = body.split('\n').filter(e=>e.length()!=0).toArray
    val non_empty_size : Int = non_empty.size
    val candidate : Array[String] = non_empty.slice(non_empty_size-Constants.SIGNATURE_MAX_LINES,non_empty_size)
    var upvotes : Int = 0
    var contains_sender_name : Boolean = false
//    println("Has Signature candidate :"+"["+candidate.mkString(",")+"]")
    candidate foreach {e =>
      if(e.trim.size < 27){
        if ((binary_regex_search(Constants.RE_RELAX_PHONE)(e) +
                     binary_regex_search(Constants.RE_EMAIL)(e) +
                     binary_regex_search(Constants.RE_URL)(e) == 1 ) || contains_sender_names(sender)(e) == 1){
          contains_sender_name = true      
        }
        else if((binary_regex_search(Constants.RE_RELAX_PHONE)(e) + binary_regex_search(Constants.RE_EMAIL)(e) + binary_regex_search(Constants.RE_URL)(e)) == 1){
              upvotes += 1
        }
//        println("Has Signature Upvotes : "+upvotes)
        
      }
    }
    
    if(contains_sender_name || upvotes > 1) true else false
  }
  
  def get_signature_candidate(lines : Array[String]) : Array[String] = {
    /*
     *
   	Return lines that could hold signature
    The lines should:
    * be among last SIGNATURE_MAX_LINES non-empty lines.
    * not include first line
    * be shorter than TOO_LONG_SIGNATURE_LINE
    * not include more than one line that starts with dashes
     */
//     println("helpers getCandidate lines : "+lines.mkString(",")+ " "+lines.size)
     val non_empty : Array[Int] = lines.zipWithIndex.
                                  filter(e=>e._1.trim.size > 0).
                                  map(e=>e._2).toArray 
//     println("Helpers getCandidate non_empty :"+non_empty.mkString(","))
     if(non_empty.size <= 1){
       
        Array[String]()        
     }
     val candidate : Array[Int]= non_empty.slice(1,non_empty.size).slice(
                         non_empty.size-Constants.SIGNATURE_MAX_LINES,non_empty.size
                     )
//     println("Helpers getCandidate : "+ candidate.mkString(","))
     val markers : String =  mark_candidate_indexes(lines, candidate)
     
     val candidates = process_marked_candidate_indexes(candidate, markers)
     
     if(candidates.size > 0){
       
      val candidate: Array[String] = lines.slice(candidates(0), lines.size)
        candidate
     }
     else{
       Array[String]() 
     }

    
  }
  
   def mark_candidate_indexes(lines : Array[String],candidate : Array[Int]) : String = {
      /*
       * Mark candidate indexes with markers

    Markers:

    * c - line that could be a signature line
    * l - long line
    * d - line that starts with dashes but has other chars as well

    >>> _mark_candidate_lines(['Some text', '', '-', 'Bob'], [0, 2, 3])
    'cdc'
       */
     
     val markers : ListBuffer[String] = ListBuffer.fill(candidate.size)("c")
     candidate.zipWithIndex foreach(e=>
         if(lines(e._1).trim.size > Constants.TOO_LONG_SIGNATURE_LINE)
           markers.update(e._2, "l")
         else{
        	 val line = lines(e._1).trim
        	 if(line.startsWith("-") && line.replaceAll("^-|-$","").size > 0){
        	   markers.update(e._2, "d")
        	 }
         }
     )
//    println("Helpers marker_lines : "+markers.mkString(""))
    markers.mkString("")
   }
   
   def process_marked_candidate_indexes(candidate : Array[Int],markers : String) : Array[Int] = {
     /*
      *  Run regexes against candidate's marked indexes to strip
    		signature candidate.

        >>> _process_marked_candidate_indexes([9, 12, 14, 15, 17], 'clddc')
        [15, 17]
    */
    val matcher = Constants.RE_SIGNATURE_CANDIDATE.matcher(markers.reverse)
    if(matcher.lookingAt()){
      val matches : Int = matcher.end()
      candidate.slice(candidate.size-matches, candidate.size)
    }else{
      Array[Int]()
    }
   } 
   
   
   def flaten_list(list_to_flatten : List[List[java.io.Serializable]]) : List[java.io.Serializable] = {
     list_to_flatten.flatten
   }
   
   def contains_sender_names(sender : String) : (String => Int) = {
     /*
      * Returns a functions to search sender\'s name or it\'s part.

        >>> feature = contains_sender_names("Sergey N.  Obukhov <xxx@example.com>")
        >>> feature("Sergey Obukhov")
        1
        >>> feature("BR, Sergey N.")
        1
        >>> feature("Sergey")
        1
        >>> contains_sender_names("<serobnic@mail.ru>")("Serobnic")
        1
        >>> contains_sender_names("<serobnic@mail.ru>")("serobnic")
        1
      */ 
     val arr : ListBuffer[String] = new ListBuffer()
     extract_names(sender).map(e=>arr.+=(e,e.capitalize)).mkString("( |$)|")
     val names : String = arr.mkString("( |$)|")
     
     val names_ : String = if(names.size > 0) names else sender
     if(names_ != ""){
       binary_regex_search(names_.r)
     }else{
       (s : String) => 0
     }
       
       //'( |$)|'.join(flatten_list([[e, e.capitalize()]
                                        
     
   }
      
   def extract_names(sender : String) : Array[String] = {
     /*
      * Tries to extract sender's names from `From:` header.

    It could extract not only the actual names but e.g.
    the name of the company, parts of email, etc.

    >>> extract_names('Sergey N.  Obukhov <serobnic@mail.ru>')
    ['Sergey', 'Obukhov', 'serobnic']
    >>> extract_names('')
    */
     val sender_ : StringBuffer = new StringBuffer(sender)
     sender.zipWithIndex foreach(e=>
       if(!e._1.toString().matches("^[a-zA-Z0-9]*$"))
         sender_.setCharAt(e._2,' ')
     )

    //Remove too short words and words from "black" list i.e.
    //words like `ru`, `gmail`, `com`, `org`, etc.
    val names = sender_.toString().split(" ").filter(e=> (e.size>1 && !Constants.BAD_SENDER_NAMES.contains(e)))
   
    names.distinct
   }
   def categories_percent(s : String, categories : Array[Int]) : Int = {
     /*
      * Returns category characters percent.

        >>> categories_percent("qqq ggg hhh", [24])
        0.0
        >>> categories_percent("q,w.", 24)
        50.0
        >>> categories_percent("qqq ggg hhh", [9])
        0.0
        >>> categories_percent("q5", 9)
        50.0
        >>> categories_percent("s.s,5s", [24, 9])
        50.0  
      */
     
     var count = 0
     s foreach (e=>
      if(categories contains(e.getType))
        count += 1
     )
     
     if(s.size > 0) (100 * (count) / s.size)  else 0
   }
   
   def punctuation_percent(s : String) : Int = {
     /*
      * Returns punctuation percent.

        >>> punctuation_percent("qqq ggg hhh")
        0.0
        >>> punctuation_percent("q,w.")
        50.0
      */
     categories_percent(s, Array(24))
   }
  
}