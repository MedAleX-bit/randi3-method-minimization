package org.randi3.randomization

import org.junit.runner.RunWith
import org.scalatest.matchers.MustMatchers
import org.apache.commons.math3.random.MersenneTwister
import org.randi3.model._
import collection.mutable.ListBuffer
import org.randi3.model.criterion.{IntegerCriterion, OrdinalCriterion}
import org.randi3.model.criterion.constraint.{IntegerConstraint, OrdinalConstraint}
import collection.mutable
import scala.Some
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSpec

@RunWith(classOf[JUnitRunner])
class MinimizationTest extends FunSpec with MustMatchers {

  import org.randi3.utility.TestingEnvironmentMinimization._

  describe("Minimization method") {

    it("should be ...") ('pending)

  }
}
