package com.talon.signature

import smile.classification.SVM
import smile.math.kernel.GaussianKernel
import java.io.File
import java.util.Arrays
import com.thoughtworks.xstream.XStream
import java.io.FileOutputStream
import java.io.ObjectOutputStream
import java.io.FileInputStream
import java.io.ObjectInputStream
import smile.math.kernel.MercerKernel
import smile.math.kernel.LinearKernel
import smile.math.kernel.PearsonKernel

class Classifier {
  private final val BASEDIR : String = new java.io.File(".").getCanonicalPath
  def init : SVM[Array[Double]] = {
    /*Inits classifier with optimal options.*/
    val svm = new SVM[Array[Double]](new GaussianKernel(0.01),10.0,134)
    svm
  }
  
  
   def train(classifier : SVM[Array[Double]],train_data_filename : String,save_classifier_filename : String) : SVM[Array[Double]] = {
     val trainFile = new File(BASEDIR + train_data_filename)
     val file_data : (Array[Array[Double]],Array[Int])= getDataFromCSV(trainFile)
     val train_data : Array[Array[Double]] = file_data._1
     val labels : Array[Int] = file_data._2
     classifier.learn(train_data, labels)
     
     
     if(save_classifier_filename != null){
      val fileOut = new FileOutputStream(BASEDIR+save_classifier_filename)
      val out = new ObjectOutputStream(fileOut);
      out.writeObject(classifier);
      out.close();
      fileOut.close();
     }
     classifier.finish()
     return classifier
   }
   
  
   def load(saved_classifier_filename : String ) : SVM[Array[Double]] = {
     try{
        val fileIn = new FileInputStream(BASEDIR + saved_classifier_filename)
        val read = new ObjectInputStream(fileIn)
        val classifier : SVM[Array[Double]] = read.readObject().asInstanceOf[SVM[Array[Double]]]
        read.close()
        fileIn.close()
        classifier
     }
     catch {
       case e: Exception =>
         e.printStackTrace()
       null
     }
   }
   
   def getDataFromCSV(file: File): (Array[Array[Double]], Array[Int]) = {
    val source = scala.io.Source.fromFile(file)
    val data = source
        .getLines()
        .map(x => getDataFromString(x))
        .toArray
    println(data.size)    
    source.close()
    
    val dataPoints = data.map(x => x._1)
    val classifierArray = data.map(x => x._2)
    println(dataPoints.size + " " + classifierArray.size)
    return (dataPoints, classifierArray)
  }
   
   def getDataFromString(dataString: String): (Array[Double], Int) = {
    //Split the comma separated value string into an array of strings
    val dataArray: Array[String] = dataString.split(',')
    val arraySize: Int = dataArray.size-1
    //Extract the values from the strings
    val coordinates  = dataArray.dropRight(1).map(x => x.toDouble).toArray
    val classifier: Int = dataArray(arraySize).toInt

    //And return the result in a format that can later 
    //easily be used to feed to Smile
    return (coordinates, classifier)
  }
  
   def test(data : Array[Double],classifier : SVM[Array[Double]]) : Int = {
     println("Result :"+classifier.predict(data))
     return classifier.predict(data)
   }
   
   def trainAndTest(classifier : SVM[Array[Double]],train_data_filename : String,saved_classifier_filename: String,data : Array[Double]) = {
     val classifiers_ = train(classifier,train_data_filename,saved_classifier_filename)
     test(data,classifiers_)
     test(Array(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0),classifiers_)
   }
 
}