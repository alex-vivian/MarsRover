import org.scalatest.{FunSpec, Matchers}


class RoverSpec extends FunSpec with Matchers {

  describe("moving from one cell to another") {
    it("should occupy one cell and free up the previously occupied ones") {
      val r = new Rover(10, 10, Position(0, 0), North(Position(0, 1)), Vector())
      r.position shouldEqual Position(0, 0)
      val r1 = r.move()
      r1.position shouldEqual Position(0, 1)
      r1.facing shouldEqual North(Position(0, 2))
    }

    it("should move forwards 5 cells") {
      val r = new Rover(10, 10, Position(0, 0), North(Position(0, 1)), Vector())
      r.position shouldEqual Position(0, 0)
      val r1 = r.move().move().move().move().move()
      r1.position shouldEqual Position(0, 5)
      r1.facing shouldEqual North(Position(0, 6))
    }

    it("should move backwards 3 cells") {
      val r = new Rover(10, 10, Position(0, 0), North(Position(0, 1)), Vector()).rotateClockwise().rotateClockwise()
      r.position shouldEqual Position(0, 0)
      val r1 = r.move().move().move()
      r1.position shouldEqual Position(0, 7)
      r1.facing shouldEqual South(Position(0, 6))
    }

    it("should move left 4 cells") {
      val r = new Rover(10, 10, Position(0, 0), North(Position(0, 1)), Vector()).rotateAntiClockwise()
      r.position shouldEqual Position(0, 0)
      val r1 = r.move().move().move()
      r1.position shouldEqual Position(7, 0)
      r1.facing shouldEqual West(Position(6, 0))
    }


    it("should move right 3 cells and up 2 cells") {
      val r = new Rover(10, 10, Position(0, 0), North(Position(0, 1)), Vector()).move().move().rotateClockwise().move().move().move()
      r.position shouldEqual Position(3, 2)
      r.facing shouldEqual East(Position(4, 2))
    }

    it("should move left 2 cells and down 2 cells") {
      val r = new Rover(10, 10, Position(0, 0), North(Position(0, 1)), Vector()).rotateAntiClockwise().move().move().rotateAntiClockwise().move().move()
      r.position shouldEqual Position(8, 8)
    }


  }

  describe("rotating clockwise") {
    it("should rotate from facing (0,1) to (1,0)") {
      val r = new Rover(3, 3, Position(0, 0), North(Position(0, 1)), Vector())
      val r1 = r.rotateClockwise()
      r1.facing shouldEqual East(Position(1, 0))
    }

    it("should rotate from facing (1, 0) to (0,2") {
      val r = Rover.apply()
      val r1 = r.rotateClockwise().rotateClockwise()
      r1.facing shouldEqual South(Position(0, 2))
    }

    it("should rotate from facing(0,2) to (2,0") {
      val r = Rover.apply()
      val r1 = r.rotateClockwise().rotateClockwise().rotateClockwise()
      r1.facing shouldEqual West(Position(2, 0))
    }

    it("should rotate from facing (2,0) to (0,1)") {
      var r = Rover.apply()
      var r1 = r.rotateClockwise().rotateClockwise().rotateClockwise().rotateClockwise()
      r1.facing shouldEqual North(Position(0, 1))
    }

  }

  describe("rotating anticlockwise") {
    it("should rotate from facing (0,1) to (2,0)") {
      var r = Rover.apply()
      var r1 = r.rotateAntiClockwise()
      r1.facing shouldEqual West(Position(2, 0))
    }

    it("should rotate from facing (2,0) to (0,2") {
      var r = Rover.apply()
      var r1 = r.rotateAntiClockwise().rotateAntiClockwise
      r1.facing shouldEqual South(Position(0, 2))
    }

    it("should rotate from facing (0,-1) to (1,0") {
      var r = Rover.apply()
      var r1 = r.rotateAntiClockwise().rotateAntiClockwise().rotateAntiClockwise()
      r1.facing shouldEqual East(Position(1, 0))
    }

    it("should rotate from facing (1,0) to (0,1)") {
      var r = Rover.apply()
      var r1 = r.rotateAntiClockwise().rotateAntiClockwise().rotateAntiClockwise().rotateAntiClockwise()
      r1.facing shouldEqual North(Position(0, 1))
    }
  }

  describe("finding shortest path between two positions") {
    it("should find the shortest path between (0,0) and (3, 4)") {
      var r = new Rover(10, 10, Position(0, 0), North(Position(0, 1)), Vector())
      r.findShortestPath(Position(3, 4)) shouldEqual Position(3, 4)
    }

    it("should find shortest path between (0,0) and(7,8)") {
      val r = new Rover(10, 10, Position(0, 0), North(Position(0, 1)), Vector())
      r.findShortestPath(Position(7, 8)) shouldEqual Position(-3, -2)
    }

    it("should find the shortest path between (0, 0) and (1, 7)") {
      val r = new Rover(10, 10, Position(0, 0), North(Position(0, 1)), Vector())
      r.findShortestPath(Position(1, 7)) shouldEqual Position(1, -3)
    }

    it("should find the shortest path between (0, 0) and (8, 3)") {
      val r = new Rover(10, 10, Position(0, 0), North(Position(0, 1)), Vector())
      r.findShortestPath(Position(8, 3)) shouldEqual Position(-2, 3)
    }
  }


  describe("autopilot x axis") {
    it("should move from (0, 0) to (4,0)") {
      val r = new Rover(10, 10, Position(0, 0), North(Position(0, 1)), Vector())
      r.moveInX(4, r).position shouldEqual Position(4, 0)

    }

    it("should move from(0,0) to (0,6)") {
      val r = new Rover(10, 10, Position(0, 0), North(Position(0, 1)), Vector())
      r.moveInX(-4, r).position shouldEqual Position(6, 0)
    }
  }

  describe("autopilot y axis") {
    it("should move from (0, 0) to (0,4)") {
      val r = new Rover(10, 10, Position(0, 0), North(Position(0, 1)), Vector())
      r.moveInY(4, r).position shouldEqual Position(0, 4)

    }

    it("should move from(0,0) to (6,0)") {
      val r = new Rover(10, 10, Position(0, 0), North(Position(0, 1)), Vector())
      r.moveInY(-4, r).position shouldEqual Position(0, 6)
    }
  }

  describe("autopilot whole") {
    it("should move from (0,0) to (1,7") {
      val r = new Rover(10, 10, Position(0, 0), North(Position(0, 1)), Vector())
      r.moveTo(Position(1, 7)).position shouldEqual Position(1, 7)
    }

    it("should move from (0,0) to (3,4)") {
      val r = new Rover(10, 10, Position(0, 0), North(Position(0, 1)), Vector())
      r.moveTo(Position(3, 4)).position shouldEqual Position(3, 4)
    }

    it("should move from (0,0) to (7,8") {
      val r = new Rover(10, 10, Position(0, 0), North(Position(0, 1)), Vector())
      r.moveTo(Position(7, 8)).position shouldEqual Position(7, 8)
    }

    it("should move from (0,0) to (8,3") {
      val r = new Rover(10, 10, Position(0, 0), North(Position(0, 1)), Vector())
      r.moveTo(Position(8, 3)).position shouldEqual Position(8, 3)
    }
  }

  describe("add obstacle to obstacle Vector") {
    it("should add a given position to obstacle Vector") {
      val r = Rover.apply()
      r.obstacles shouldEqual Vector()
      r.addObstacle(Position(0, 1)).obstacles shouldEqual Vector(Position(0, 1))
    }

    it("shouldn't add invalid positions to obstacle Vector") {
      val r = Rover.apply()
      r.obstacles shouldEqual Vector()
      r.addObstacle(Position(0, -1)).obstacles shouldEqual Vector()

      r.addObstacle(Position(1, 4)).obstacles shouldEqual Vector()
    }
  }

  describe("remove obstacle from obstacle Vector") {
    it("should remove a given position from the Vector") {
      val r = Rover.apply()
      val r1 = r.addObstacle(Position(0, 1))
      r1.removeObstacle(Position(0, 1)).obstacles shouldEqual Vector()

    }

    it("should remain unchanged if trying to remove a vector not on the list") {
      val r = Rover.apply()
      val r1 = r.addObstacle(Position(0, 1))
      r1.removeObstacle(Position(0, 2)).obstacles shouldEqual Vector(Position(0,1))
    }

  }

}

