object Lab2Template extends App {

  println(numValid(creditcards))

  def eval(list: List[Long]) = list.foldLeft(0L)((x: Long, y: Long) => y + (10L * x))
  def evalRev(list: List[Long]) = list.foldRight(0L)((x: Long, y: Long) => x + (10L * y))

  def toDigits(l: Long): List[Long] = l.toString.toList.map(x => x.asDigit.toLong)

  def toDigitsRev(l: Long): List[Long] = toDigits(l).reverse

  def doubleSecond(list: List[Long]): List[Long] = list match {
	case Nil => Nil
	case head :: Nil => head :: Nil
	case x :: y :: xs => x :: 2*y :: doubleSecond(xs)
  }
def sum(xs: List[Long]): Long = {
  xs match {
    case x :: tail => x + sum(tail) // if there is an element, add it to the sum of the tail
    case Nil => 0 // if there are no elements, then the sum is 0
  }
}
  def sumDigits(list: List[Long]): Long = sum(list.map(x => sum(toDigits(x))))

  def isValid(l: Long): Boolean = sumDigits(doubleSecond(toDigitsRev(l))) % 10 == 0

  def numValid(list: List[Long]): Long = list filter (isValid) map (_ => 1L) sum

  def creditcards: List[Long] = List(4716347184862961L, 4532899082537349L,
    4485429517622493L, 4320635998241421L, 4929778869082405L, 5256283618614517L,
    5507514403575522L, 5191806267524120L, 5396452857080331L, 5567798501168013L,
    6011798764103720L, 6011970953092861L, 6011486447384806L, 6011337752144550L,
    6011442159205994L, 4916188093226163L, 4916699537435624L, 4024607115319476L,
    4556945538735693L, 4532818294886666L, 5349308918130507L, 5156469512589415L,
    5210896944802939L, 5442782486960998L, 5385907818416901L, 6011920409800508L,
    6011978316213975L, 6011221666280064L, 6011285399268094L, 6011111757787451L,
    4024007106747875L, 4916148692391990L, 4916918116659358L, 4024007109091313L,
    4716815014741522L, 5370975221279675L, 5586822747605880L, 5446122675080587L,
    5361718970369004L, 5543878863367027L, 6011996932510178L, 6011475323876084L,
    6011358905586117L, 6011672107152563L, 6011660634944997L, 4532917110736356L,
    4485548499291791L, 4532098581822262L, 4018626753711468L, 4454290525773941L,
    5593710059099297L, 5275213041261476L, 5244162726358685L, 5583726743957726L,
    5108718020905086L, 6011887079002610L, 6011119104045333L, 6011296087222376L,
    6011183539053619L, 6011067418196187L, 4532462702719400L, 4420029044272063L,
    4716494048062261L, 4916853817750471L, 4327554795485824L, 5138477489321723L,
    5452898762612993L, 5246310677063212L, 5211257116158320L, 5230793016257272L,
    6011265295282522L, 6011034443437754L, 6011582769987164L, 6011821695998586L,
    6011420220198992L, 4716625186530516L, 4485290399115271L, 4556449305907296L,
    4532036228186543L, 4916950537496300L, 5188481717181072L, 5535021441100707L,
    5331217916806887L, 5212754109160056L, 5580039541241472L, 6011450326200252L,
    6011141461689343L, 6011886911067144L, 6011835735645726L, 6011063209139742L,
    379517444387209L, 377250784667541L, 347171902952673L, 379852678889749L,
    345449316207827L, 349968440887576L, 347727987370269L, 370147776002793L,
    374465794689268L, 340860752032008L, 349569393937707L, 379610201376008L,
    346590844560212L, 376638943222680L, 378753384029375L, 348159548355291L,
    345714137642682L, 347556554119626L, 370919740116903L, 375059255910682L,
    373129538038460L, 346734548488728L, 370697814213115L, 377968192654740L,
    379127496780069L, 375213257576161L, 379055805946370L, 345835454524671L,
    377851536227201L, 345763240913232L);
}