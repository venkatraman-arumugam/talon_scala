package com.talon

import smile.classification.SVM
import com.talon.signature.Helpers
import scala.util.control.Exception.Catch
import scala.collection.mutable.ListBuffer
import com.talon.signature.FeatureSpace
import java.util.Arrays

class Extraction(var extractor : SVM[Array[Double]]){

  
    
    
    /*
     * Checks if the line belongs to signature. Returns True or False
     */
    def isSignatureLine(line :String, sender : String, classifier : SVM[Array[Double]]) : Boolean =  {
      //Checks if the line belongs to signature. Returns True or False
      
      val data : Array[Double] = FeatureSpace.build_pattern(line, FeatureSpace.features(sender)).map(_.toDouble)
//      println ("Extraction IsSignature : "+classifier.predict(data)+" " + data.mkString(",")+" "+line)
      if(classifier.predict(data) == 1) true else false
    }
    
  /*
   *Strips signature from the body of the message.
  Returns stripped body and signature as a tuple.
  If no signature is found the corresponding returned value is None.
   */
  def extract(body : String,sender : String) : (String,String) = {
    try{
        val delimiter : String = Utils.get_delimiter(body.trim)
        val body_part = body.trim
        if(Helpers.has_signature(body_part : String, sender : String)){
          val lines : Array[String] = body.split("\n")
          val markers : String = mark_lines(lines, sender)
          val textAndSignature = process_marked_lines(lines, markers)
          val text : Array[String] = textAndSignature._1
          val signature : String = textAndSignature._2.mkString(delimiter)
          val body_text : String = text.mkString(delimiter)
          if(signature.size > 0 && body_text.size > 0){
            (body_text, signature)
          }else{
            (body_text, null)
          }
        }else{
            (body, null)
        }
    }catch {
       case e: Exception =>
         e.printStackTrace()
         (body,null)
     }
        
  }
  
  def taggedData(body : String,sender : String) : String = {
    try{
        val delimiter : String = Utils.get_delimiter(body)
    
        val body_part = body.trim
        
        if(Helpers.has_signature(body_part : String, sender : String)){
//          println("HI")
          val lines : Array[String] = body.split("\n")
          val markers : String = mark_lines(lines, sender)
          markers
        }else{
          Array.fill(body.split("\n").size-(body.split("\n").size-11))("t").mkString("")
        }
         
    }catch {
       case e: Exception =>
         e.printStackTrace()
         Array.fill(body.split("\n").size-(body.split("\n").size-11))("t").mkString("")
     }
        
  }
  
  
  def mark_lines(lines : Array[String],sender : String) : String = {
    /*
     * Mark message lines with markers to distinguish signature lines.

    Markers:

    * e - empty line
    * s - line identified as signature
    * t - other i.e. ordinary text line

    >>> mark_message_lines(['Some text', '', 'Bob'], 'Bob')
    'tes'
     */
    val candidate : Array[String] = Helpers.get_signature_candidate(lines)
    // at first consider everything to be text no signature
    val markers : ListBuffer[String] = ListBuffer.fill(lines.size)("t")
    //mark lines starting from bottom up
    //mark only lines that belong to candidate
    //no need to mark all lines of the message
    candidate.zipWithIndex.foreach(e=> {
     val j : Int = lines.size - candidate.size + e._2
     if(e._1.size <= 0)
       markers.update(j,"e")
     
     else if(isSignatureLine(e._1, sender, extractor))
       markers.update(j,"s")
    })
//    println(markers.mkString("")+" Extraction Marked lines")
    markers.mkString("")
    
  }
 
  def process_marked_lines(lines :Array[String], markers : String) : (Array[String],Array[String]) = {
    /*
     *  Run regexes against message's marked lines to strip signature.

    >>> _process_marked_lines(['Some text', '', 'Bob'], 'tes')
    (['Some text', ''], ['Bob'])
     */
    val signature : List[Int] = Constants.RE_REVERSE_SIGNATURE.
                              findFirstMatchIn(markers.reverse).
                              map(_.end).toList
     
    if(signature.size != 0){
      (lines.slice(0,lines.size-signature(0)),lines.slice(lines.size-signature(0),lines.size))
    }else{
      (lines,Array[String]())
    }
  }
}