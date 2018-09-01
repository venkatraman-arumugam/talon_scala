import com.talon.signature.Classifier
import com.talon.Utils
import smile.classification.SVM
import com.talon.Extraction
import com.talon.Talon
import scala.io.Source


object Main extends App{
  
  val classifier_dispatch = new Classifier()
    
  val cla =  classifier_dispatch.train(classifier_dispatch.init,"/resource/train.data", "/resource/signature_classifier.bin")
    
  val DATA_DIR = new java.io.File("Talon/resource").getCanonicalPath

  val text = """Thanks Sasha, I can't go any higher and is why I limited it to the
homepage.

John Doe
via mobile  
  """
  val preprocessed : String = text.trim().split("\n").map(e=>
        e.replaceAll(",|\\.$","")
  ).toArray.mkString("\n")

  val classifier : SVM[Array[Double]] = Talon.init
  val extractor = new Extraction(classifier)

  val extracted_mail : (String,String) = extractor.extract(preprocessed,"john.doe@example.com")
  println("Text "+extracted_mail._1)
  println("Signature "+extracted_mail._2)

}