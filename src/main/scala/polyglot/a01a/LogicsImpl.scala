package polyglot.a01a

import polyglot.a01a.Logics
import polyglot.a01a.Logics.Result
import util.Sequences.Sequence.*
import util.Sequences.*
import scala.util.Random

trait Logic:
  def hit(row: Int, col: Int): Result

object Logic:
  def apply(size: Int, boat: Int): Logic = LogicsImpl(size,boat)

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01a/sol2/ */
private class LogicsImpl(private val size: Int, private val boat: Int) extends Logic:
  private val FAILURES = 5
  private var countFailures = 0
  private val boatSize = boat
  private var rand: Random = Random()
  private val boatRow = rand.nextInt(size)
  private val boatLeftCol = rand.nextInt(size - boatSize + 1)
  private var hit: Sequence[(Int, Int)] = Nil()

  def hit(row: Int, col: Int) =
    if row == boatRow && col >= boatLeftCol && col < (boatLeftCol+boatSize) then
      hit = hit.concat(Sequence.apply((row,col)))
      if hit.lenght() == boatSize then
        Result.WON
      else Result.HIT
    else
      countFailures = countFailures + 1
      if countFailures == FAILURES then
        Result.LOST
      else
        Result.MISS



