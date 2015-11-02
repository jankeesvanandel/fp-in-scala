package fp_in_scala.chapter_04

import org.scalatest.{Matchers, FlatSpec}

class OptionSpec extends FlatSpec with Matchers {

  it should "implement map for Some" in {
    Some("hello").map(_.length) should be (Some(5))
  }

  it should "implement map for None" in {
    None.asInstanceOf[Option[String]].map(_.length) should be (None)
  }

  it should "implement flatMap for Some" in {
    Some("hello").flatMap(s => Some(s.length)) should be (Some(5))
  }

  it should "implement flatMap for None" in {
    None.asInstanceOf[Option[String]].flatMap(s => Some(s.length)) should be (None)
  }

  it should "implement filter for Some" in {
    Some("hello").filter(_.length == 5) should be (Some("hello"))
    Some("hello").filter(_.length == 4) should be (None)
  }

  it should "implement filter for None" in {
    None.asInstanceOf[Option[String]].filter(_.length == 5) should be (None)
  }

  it should "implement getOrElse for Some" in {
    Some("hello").getOrElse("Nooo") should be ("hello")
  }

  it should "implement getOrElse for None" in {
    None.asInstanceOf[Option[String]].getOrElse("Nooo") should be ("Nooo")
  }

  it should "implement orElse for Some" in {
    Some("hello").orElse(Some("Nooo")) should be (Some("hello"))
  }

  it should "implement orElse for None" in {
    None.asInstanceOf[Option[String]].orElse(Some("Nooo")) should be (Some("Nooo"))
  }
}
