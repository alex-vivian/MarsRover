case class Rover(width: Int, height: Int, position: Position, facing: Direction, obstacles: Vector[Position]) {


  //move rover into square it was facing
  def move(): Rover = {
    val f = getFacing()
    this.copy(position = f, facing = updateFacing())

  }

  //updates facing in same direction when rover moves
  def updateFacing(): Direction = {
    facing match {
      case North(Position(x, y)) => North(findNewPosition(Position(x, y), Position(0, 1)))
      case East(Position(x, y)) => East(findNewPosition(Position(x, y), Position(1, 0)))
      case South(Position(x, y)) => South(findNewPosition(Position(x, y), Position(0, -1)))
      case West(Position(x, y)) => West(findNewPosition(Position(x, y), Position(-1, 0)))
    }
  }

  //gets position value from a Direction
  def getFacing(): Position = {
    facing match {
      case North(Position(a, b)) => Position(a, b)
      case South(Position(a, b)) => Position(a, b)
      case East(Position(a, b)) => Position(a, b)
      case West(Position(a, b)) => Position(a, b)
    }
  }

  //updates facing value by rotating 90 degrees clockwise
  def rotateClockwise(): Rover = {

    facing match {
      case North(Position(x, y)) => this.copy(facing = East(findNewPosition(position, Position(1, 0))))
      case East(Position(x, y)) => this.copy(facing = South(findNewPosition(position, Position(0, -1))))
      case South(Position(x, y)) => this.copy(facing = West(findNewPosition(position, Position(-1, 0))))
      case West(Position(x, y)) => this.copy(facing = North(findNewPosition(position, Position(0, 1))))
    }

  }

  //updates facing value by rotating 90 degrees anti clockwise
  def rotateAntiClockwise(): Rover = {

    facing match {
      case North(Position(x, y)) => this.copy(facing = West(findNewPosition(position, Position(-1, 0))))
      case East(Position(x, y)) => this.copy(facing = North(findNewPosition(position, Position(0, 1))))
      case South(Position(x, y)) => this.copy(facing = East(findNewPosition(position, Position(1, 0))))
      case West(Position(x, y)) => this.copy(facing = South(findNewPosition(position, Position(0, -1))))
    }

  }

  //calculates new position value by combining current value with given value, or moving to other side of the grid if an edge is reaches
  def findNewPosition(p: Position, r: Position): Position = {
    val summed = p + r

    summed match {
      case Position(x, y) if x > width => Position(0, y)
      case Position(x, y) if x == -1 => Position(width - 1, y)
      case Position(x, y) if y > height => Position(x, 0)
      case Position(x, y) if y == -1 => Position(x, height - 1)
      case Position(x, y) => Position(x, y)
    }
  }

  //moves to specified location using shortest path
  def moveTo(dest: Position): Rover = {
    val pos = findShortestPath(dest)
    val r0 = moveInX(pos.x, this)
    moveInY(pos.y, r0)

  }

  //moves rover in x direction a specified number of steps
  def moveInX(dest: Int, r0: Rover): Rover = {

    val r = dest match {
      case x if (x >= 0) => r0.copy(facing = East(findNewPosition(r0.position, Position(1, 0))))
      case x if (x < 0) => r0.copy(facing = West(findNewPosition(r0.position, Position(-1, 0))))
    }

    def loop(i: Int, j: Int, r: Rover): Rover = {
      if (i < j) loop(i + 1, j, r.move())
      else r
    }

    loop(0, math.abs(dest), r)

  }

  //moves rover in y direction a specified number of steps
  def moveInY(dest: Int, r0: Rover): Rover = {

    val r = dest match {
      case y if (y >= 0) => r0.copy(facing = North(findNewPosition(r0.position, Position(0, 1))))
      case y if (y < 0) => r0.copy(facing = South(findNewPosition(r0.position, Position(0, -1))))
    }


    def loop(i: Int, j: Int, r: Rover): Rover = {
      if (i < j) loop(i + 1, j, r.move())
      else r
    }

    loop(0, math.abs(dest), r)

  }


  //returns a Position that represents the shortest path to a given location
  def findShortestPath(dest: Position): Position = {
    Position(getShortest(position.x, dest.x, width), getShortest(position.y, dest.y, height))

  }

  //returns smallest number of steps to get to destination for a given axis
  def getShortest(start: Int, end: Int, max: Int): Int = {

    val diff = end - start

    diff match {
      case d if math.abs(d) > max / 2 && d > 0 => -(max - d)
      case d if math.abs(d) > max / 2 => max + d
      case d => d
    }

  }

  //adding a position to the obstacle list
  def addObstacle(pos: Position): Rover = {
    if(0 <= pos.x && pos.x < width && 0 <= pos.y && pos.y < height) this.copy(obstacles = pos +: obstacles)
    else this

  }

  //removing position from the obstacle list
  def removeObstacle(pos: Position): Rover = {
    this.copy(obstacles = obstacles.filter(_ != pos))
  }

  //returns an option containing a position in a given direction if position isn't blocked
  def getDirection(pos: Position): Option[Position] = {
    if(obstacles.contains(position+pos)) None
    else Some((position+pos))

  }
}


object Rover {
  def apply(): Rover = new Rover(3, 3, Position(0, 0), North(Position(0, 1)), Vector())
}

case class Position(x: Int, y: Int) {
  def +(other: Position): Position = {
    Position.combine(this, other)
  }


}

object Position {
  val zero: Position = Position(0, 0)

  def combine(a: Position, b: Position): Position = {
    Position(a.x + b.x, a.y + b.y)
  }

}

sealed trait Direction

case class North(location: Position) extends Direction

case class East(location: Position) extends Direction

case class South(location: Position) extends Direction

case class West(location: Position) extends Direction

