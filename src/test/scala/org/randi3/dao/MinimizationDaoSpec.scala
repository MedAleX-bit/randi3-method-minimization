package org.randi3.dao

import org.apache.commons.math3.random.MersenneTwister

import org.junit.runner.RunWith

import org.scalaquery.ql._
import org.scalaquery.session.Database.threadLocalSession


import org.scalatest.matchers.{ShouldMatchers, MustMatchers}
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner
import scala.Left
import org.randi3.randomization.Minimization
import scala.Right
import scala.Some


@RunWith(classOf[JUnitRunner])
class MinimizationDaoSpec extends FunSpec with MustMatchers with ShouldMatchers {

  import org.randi3.utility.TestingEnvironmentMinimization._

  import driver.Implicit._

  import schemaUrn._

  describe("MinimizationDao create method") {

    it("should be able to create a new block minimization method with parameter and without tmp data")(pending)


  }
}
