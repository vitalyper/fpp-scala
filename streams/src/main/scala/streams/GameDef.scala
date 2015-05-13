package streams

import common._
import scala.language.implicitConversions

/**
 * This trait represents the layout and building blocks of the game
 *
 */
trait GameDef {

  /**
   * The case class `Pos` encodes positions in the terrain.
   * 
   * IMPORTANT NOTE
   *  - The `row` coordinate denotes the position on the vertical axis
   *  - The `column` coordinate is used for the horizontal axis
   *  - The coordinates increase when moving down and right
   * 
   * Illustration:
   *
   *     0 1 2 3   <- y axis
   *   0 o o o o
   *   1 o o o o
   *   2 o # o o    # is at position Pos(2, 1)
   *   3 o o o o
   *  
   *   ^
   *   |
   *  
   *   x axis
   */
  case class Pos(row: Int, column: Int) {
    /** The position obtained by changing the `row` coordinate by `d` */
    def drow(d: Int) = copy(row = row + d)

    /** The position obtained by changing the `column` coordinate by `d` */
    def dcolumn(d: Int) = copy(column = column + d)
  }

  /**
   * The position where the block is located initially.
   *
   * This value is left abstract, it will be defined in concrete
   * instances of the game.
   */
  val startPos: Pos

  /**
   * The target position where the block has to go.
   * This value is left abstract.
   */
  val goal: Pos

  /**
   * The terrain is represented as a function from positions to
   * booleans. The function returns `true` for every position that
   * is inside the terrain.
   *
   * As explained in the documentation of class `Pos`, the `x` axis
   * is the vertical one and increases from top to bottom.
   */
  type Terrain = Pos => Boolean

  
  /**
   * The terrain of this game. This value is left abstract.
   */
  val terrain: Terrain


  /**
   * In Bloxorz, we can move left, right, Up or down.
   * These moves are encoded as case objects.
   */
  sealed abstract class Move
  case object Left  extends Move
  case object Right extends Move
  case object Up    extends Move
  case object Down  extends Move

  /**
   * All possible moves
   */
  lazy val moves = List(Left, Right, Up, Down)

  /** Implicit conversion from Pos to Block gives us a standing block at Pos */
  implicit def blockFromPos(p: Pos) = new Block(p, p)

  /**
   * This function returns the block at the start position of
   * the game.
   */
  def startBlock: Block = startPos

  /**
   * A block is represented by the position of the two cubes that
   * it consists of. We make sure that `b1` is lexicographically
   * smaller than `b2`.
   */
  case class Block(b1: Pos, b2: Pos) {

    // checks the requirement mentioned above
    require(b1.row <= b2.row && b1.column <= b2.column, "Invalid block position: b1=" + b1 + ", b2=" + b2)

    /**
     * Returns a block where the `row` coordinates of `b1` and `b2` are
     * changed by `d1` and `d2`, respectively.
     */
    def drow(d1: Int, d2: Int) = Block(b1.drow(d1), b2.drow(d2))

    /**
     * Returns a block where the `y` coordinates of `b1` and `b2` are
     * changed by `d1` and `d2`, respectively.
     */
    def dcolumn(d1: Int, d2: Int) = Block(b1.dcolumn(d1), b2.dcolumn(d2))


    /** The block obtained by moving left */
    def left = if (isStanding)         dcolumn(-2, -1)
               else if (b1.row == b2.row)  dcolumn(-1, -2)
               else                    dcolumn(-1, -1)

    /** The block obtained by moving right */
    def right = if (isStanding)        dcolumn(1, 2)
                else if (b1.row == b2.row) dcolumn(2, 1)
                else                   dcolumn(1, 1)

    /** The block obtained by moving up */
    def up = if (isStanding)           drow(-2, -1)
             else if (b1.row == b2.row)    drow(-1, -1)
             else                      drow(-1, -2)

    /** The block obtained by moving down */
    def down = if (isStanding)         drow(1, 2)
               else if (b1.row == b2.row)  drow(1, 1)
               else                    drow(2, 1)

    def blockAfterMove(m: Move): Block = m match {
      case Left => left
      case Right => right
      case Up => up
      case Down => down
    }

    /**
     * Returns the list of blocks that can be obtained by moving
     * the current block, together with the corresponding move.
     */
    def neighbors: List[(Block, Move)] = for (m <- moves) yield ((blockAfterMove(m),m))

    /**
     * Returns the list of positions reachable from the current block
     * which are inside the terrain.
     */
    def legalNeighbors: List[(Block, Move)] = neighbors filter (p => p._1.isLegal)

    /**
     * Returns `true` if the block is standing.
     */
    def isStanding: Boolean = b1.row == b2.row && b1.column == b2.column

    /**
     * Returns `true` if the block is entirely inside the terrain.
     */
    def isLegal: Boolean = terrain(b1) && terrain(b2)

  }
}