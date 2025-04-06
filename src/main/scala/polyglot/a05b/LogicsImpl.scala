package polyglot.a05b

import polyglot.a05b.Logics
import scala.util.Random
import scala.math.abs

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a05b/sol2/
 * */
class LogicsImpl(private val size: Int) extends Logics:
  private var tickCount: Int = 0
  private val rand: Random = Random()
  private val initialX: Int = rand.nextInt(size - 2) + 1
  private val initialY: Int = rand.nextInt(size - 2) + 1

  override def tick(): Unit = tickCount = tickCount + 1

  override def isOver: Boolean = (initialY - tickCount) < 0 || (initialY + tickCount) >= size
    || (initialX - tickCount) < 0 || (initialX + tickCount) >= size

  override def hasElement(x: Int, y: Int): Boolean =
    (x == initialX && abs(y - initialY) <= tickCount) ||
    (y == initialY && abs(x - initialX) <= tickCount) ||
    (x - y == initialX - initialY && abs(x - initialX) <= tickCount) ||
    (x + y == initialX + initialY && abs(x - initialX) <= tickCount)
