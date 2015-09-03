package fp_in_scala.chapter_02

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by jankeesvanandel on 31/08/15.
 */
class FibonacciSpec extends FlatSpec with Matchers {

  it should "error if given a value lower than 0" in {

    def runChecks(fibFn: Int => Either[String, _]): Unit = {
      fibFn(-1) should be (Left("value -1 is lower than 0 and not allowed for fibonacci"))
      fibFn(-10) should be (Left("value -10 is lower than 0 and not allowed for fibonacci"))
      fibFn(-100) should be (Left("value -100 is lower than 0 and not allowed for fibonacci"))
      fibFn(-1000) should be (Left("value -1000 is lower than 0 and not allowed for fibonacci"))
      fibFn(Integer.MIN_VALUE) should be (Left(s"value ${Integer.MIN_VALUE} is lower than 0 and not allowed for fibonacci"))
    }
    runChecks(Fibonacci.fib)
    runChecks(Fibonacci.fibBig)
  }

  it should "return 0 if given 1" in {
    Fibonacci.fib(1) should be (Right(0))
  }

  it should "return 1 if given 2" in {
    Fibonacci.fib(2) should be (Right(1))
  }

  it should "return 1 if given 3" in {
    Fibonacci.fib(3) should be (Right(1))
  }

  it should "return 2 if given 4" in {
    Fibonacci.fib(4) should be (Right(2))
  }

  it should "return 3 if given 5" in {
    Fibonacci.fib(5) should be (Right(3))
  }

  it should "return 5 if given 6" in {
    Fibonacci.fib(6) should be (Right(5))
  }

  it should "return 6 if given 7" in {
    Fibonacci.fib(7) should be (Right(8))
  }

  it should "return 5 if given 8" in {
    Fibonacci.fib(8) should be (Right(13))
  }

  it should "return 34 if given 9" in {
    Fibonacci.fib(9) should be (Right(21))
  }

  it should "work for bigger numbers as well" in {
    Fibonacci.fib(10) should be (Right(34))
    Fibonacci.fib(20) should be (Right(4181))
    Fibonacci.fib(30) should be (Right(514229))
    Fibonacci.fib(40) should be (Right(63245986))
  }

  it should "work for really big numbers using BigInt" in {
    Fibonacci.fibBig(50) should be (Right(BigInt("7778742049")))
    Fibonacci.fibBig(100) should be (Right(BigInt("218922995834555169026")))
    Fibonacci.fibBig(1000) should be (Right(BigInt("26863810024485359386146727202142923967616609318986952340123175997617981700247881689338369654483356564191827856161443356312976673642210350324634850410377680367334151172899169723197082763985615764450078474174626")))
    Fibonacci.fibBig(10000) should be (Right(BigInt("20793608237133498072112648988642836825087036094015903119682945866528501423455686648927456034305226515591757343297190158010624794267250973176133810179902738038231789748346235556483191431591924532394420028067810320408724414693462849062668387083308048250920654493340878733226377580847446324873797603734794648258113858631550404081017260381202919943892370942852601647398213554479081823593715429566945149312993664846779090437799284773675379284270660175134664833266377698642012106891355791141872776934080803504956794094648292880566056364718187662668970758537383352677420835574155945658542003634765324541006121012446785689171494803262408602693091211601973938229446636049901531963286159699077880427720289235539329671877182915643419079186525118678856821600897520171070499437657067342400871083908811800976259727431820539554256869460815355918458253398234382360435762759823179896116748424269545924633204614137992850814352018738480923581553988990897151469406131695614497783720743461373756218685106856826090696339815490921253714537241866911604250597353747823733268178182198509240226955826416016690084749816072843582488613184829905383150180047844353751554201573833105521980998123833253261228689824051777846588461079790807828367132384798451794011076569057522158680378961532160858387223882974380483931929541222100800313580688585002598879566463221427820448492565073106595808837401648996423563386109782045634122467872921845606409174360635618216883812562321664442822952537577492715365321134204530686742435454505103269768144370118494906390254934942358904031509877369722437053383165360388595116980245927935225901537634925654872380877183008301074569444002426436414756905094535072804764684492105680024739914490555904391369218696387092918189246157103450387050229300603241611410707453960080170928277951834763216705242485820801423866526633816082921442883095463259080471819329201710147828025221385656340207489796317663278872207607791034431700112753558813478888727503825389066823098683355695718137867882982111710796422706778536913192342733364556727928018953989153106047379741280794091639429908796650294603536651238230626")))
  }

}
