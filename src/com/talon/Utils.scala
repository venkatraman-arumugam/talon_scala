package com.talon
object Utils{
  
  
  
  def get_delimiter(msg_body : String) : String = {
    val delimiter = Constants.RE_DELIMITER findFirstIn(msg_body)
    delimiter match {
      case Some(i) => i
      case None => "\n"
    }
  }
}