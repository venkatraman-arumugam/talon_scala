# Talon-scala

Scala version of [Mailgun library](https://github.com/mailgun/talon)
Mailgun library to extact message quotations and signatures

If you ever tried to parse message quotations or signatures you know that absence of any formatting standards in this area could make this task a nightmare. Hopefully this library will make your life much easier. The name of the project is inspired by TALON - multipurpose robot designed to perform missions ranging from reconnaissance to combat and operate in a number of hostile environments. That’s what a good quotations and signature parser should be like :smile:


## Usage

For loading the trained model file name is mentioned in Talon.scala

```
object Main extends App{
  
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
  //text == "Thanks Sasha, I can't go any higher and is why I limited it to the\nhomepage."
  //signature == "John Doe\nvia mobile"

}

```

## Training

Data (train.data) is in resource folder.
```
val classifier : SVM[Array[Double]] = Talon.init
/*arguments:
	classifier : SVM
	traning_file_name : /resource/train.data
	ml_weights_file : /resource/classifier.bin 
*/
Classifier.trainAndTest(classifier, "/resource/train.data", "/resource/classifier.bin")

```

## Test
coming soon :)


## Requirements

[smile](https://haifengl.github.io/smile/)

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details



