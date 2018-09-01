package com.talon

import com.talon.signature.Classifier
import smile.classification.SVM



object Talon {
  val DATA_DIR : String = "/resource"

  val EXTRACTOR_FILENAME : String  = DATA_DIR + "/signature_classifier.bin"
  
  def init : SVM[Array[Double]] = {
    val classifier = new Classifier()
    val extractor : SVM[Array[Double]] = classifier.load(EXTRACTOR_FILENAME)                                            
    extractor
  }
}