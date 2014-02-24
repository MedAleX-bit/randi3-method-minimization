package org.randi3.dao

import org.apache.commons.math3.random.{JDKRandomGenerator, MersenneTwister}

import org.junit.runner.RunWith

import scala.slick.session.Database.threadLocalSession


import org.scalatest.matchers.{ShouldMatchers, MustMatchers}
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner
import scala.Left
import org.randi3.randomization.Minimization
import scala.Right
import scala.Some
import org.randi3.utility.TestingEnvironmentMinimization
import org.randi3.model.criterion.constraint.OrdinalConstraint
import org.randi3.model.criterion.OrdinalCriterion
import java.util.Random
import org.randi3.model.{TrialSubject, SubjectProperty}
import java.io.{ObjectOutputStream, ByteArrayOutputStream}
import scala.slick.lifted.Query


@RunWith(classOf[JUnitRunner])
class MinimizationDaoSpec extends FunSpec with MustMatchers {

  import org.randi3.utility.TestingEnvironmentMinimization._

  import driver.Implicit._

  import schemaUrn._

  describe("MinimizationDao create method") {

    it("should be able to create a new minimization method with parameter and without tmp data") {
      val randomizationMethod: Minimization = new Minimization(p = 0.78, seedRandomEqualScore = 1234)(random = new MersenneTwister, randomEqualScore = new MersenneTwister(1234))
      val trialDB = trialDao.get(trialDao.create(createTrial.copy(randomizationMethod = None)).toOption.get).toOption.get.get
      val id = minimizationDao.create(randomizationMethod, trialDB.id).toEither match {
        case Left(x) => fail(x)
        case Right(id) => id
      }

      database withSession {
        val allMinimizations = Query(Minimizations).list
        allMinimizations.size must be(1)
        allMinimizations.head._3 must be(Some(id))
        allMinimizations.head._4 must be(randomizationMethod.p)
        allMinimizations.head._5 must be(randomizationMethod.seedRandomEqualScore)
        allMinimizations.head._6 must not be (null)

        Query(MinimizationConstraints).list must be('empty)
        Query(MinimizationConstraintTreatments).list must be('empty)
        Query(MinimizationTrialSites).list must be('empty)
        Query(MinimizationSiteTreatments).list must be('empty)

      }
    }

  }

  describe("MinimizationDao get method") {

    it("should be able to get a minimization method with parameters and without tmp data (no subject randomized)") {
      val randomizationMethod: Minimization = new Minimization(p = 0.78, seedRandomEqualScore = 1234)(random = new MersenneTwister, randomEqualScore = new MersenneTwister(1234))
      val trialDB = trialDao.get(trialDao.create(createTrial.copy(randomizationMethod = None)).toOption.get).toOption.get.get
      val id = minimizationDao.create(randomizationMethod, trialDB.id).toEither match {
        case Left(x) => fail(x)
        case Right(id) => id
      }

      val randomizationMethodDB = minimizationDao.get(id)
      randomizationMethodDB.isSuccess must be(true)
      randomizationMethodDB.toOption.get.get.seedRandomEqualScore must be(randomizationMethod.seedRandomEqualScore)
      randomizationMethodDB.toOption.get.get.p must be(randomizationMethod.p)
      randomizationMethodDB.toOption.get.get.randomEqualScore.nextDouble() must be(randomizationMethod.randomEqualScore.nextDouble())
      randomizationMethodDB.toOption.get.get.random.nextDouble() must be(randomizationMethod.random.nextDouble())
    }

    it("should be able to get a minimization method with parameter and with tmp data (dependency with update method)") {
      val allocationSequence = List("dummy:3", "dummy:2", "dummy:3", "dummy:1", "dummy:3",
        "dummy:1", "dummy:3", "dummy:3", "dummy:3", "dummy:3",
        "dummy:2", "dummy:2", "dummy:1", "dummy:3", "dummy:2",
        "dummy:2", "dummy:3", "dummy:2", "dummy:2", "dummy:2",
        "dummy:1", "dummy:3", "dummy:1", "dummy:3", "dummy:3",
        "dummy:2", "dummy:1", "dummy:3", "dummy:3", "dummy:2",
        "dummy:2", "dummy:1", "dummy:1", "dummy:2", "dummy:2",
        "dummy:3", "dummy:3", "dummy:2", "dummy:3", "dummy:3",
        "dummy:3", "dummy:2", "dummy:3", "dummy:1", "dummy:2",
        "dummy:3", "dummy:2", "dummy:3", "dummy:3", "dummy:3",
        "dummy:2", "dummy:3", "dummy:3", "dummy:3", "dummy:3",
        "dummy:3", "dummy:2", "dummy:3", "dummy:3", "dummy:3")

      val arm1 = createTreatmentArm.copy(name = "dummy:1", plannedSize = 10)
      val arm2 = createTreatmentArm.copy(name = "dummy:2", plannedSize = 20)
      val arm3 = createTreatmentArm.copy(name = "dummy:3", plannedSize = 30)

      val random = new JDKRandomGenerator()
      random.setSeed(1)
      val randomEqualScore = new JDKRandomGenerator()
      random.setSeed(1)

      val minimization = new Minimization(p = 0.7, seedRandomEqualScore = 1)(random = random, randomEqualScore = randomEqualScore)

      val strataCR1_M = OrdinalConstraint(configurations = List(Some("M"))).toOption.get

      val strataCR1_F = OrdinalConstraint(configurations = List(Some("F"))).toOption.get

      val cr1 = OrdinalCriterion(id = 1, name = "SEX", description = "description", values = Set("M", "F"), inclusionConstraint = None, strata = List(strataCR1_M, strataCR1_F)).toOption.get
      val strataCR2_1 = OrdinalConstraint(configurations = List(Some("1"))).toOption.get

      val strataCR2_2 = OrdinalConstraint(configurations = List(Some("2"))).toOption.get

      val strataCR2_3 = OrdinalConstraint(configurations = List(Some("3"))).toOption.get
      val strataCR2_4 = OrdinalConstraint(configurations = List(Some("4"))).toOption.get
      val strataCR2_5 = OrdinalConstraint(configurations = List(Some("5"))).toOption.get
      val cr2 = OrdinalCriterion(name = "TrialSites", description = "description", values = Set("4", "5", "1", "2", "3"), inclusionConstraint = None, strata = List(strataCR2_1, strataCR2_2, strataCR2_3, strataCR2_4, strataCR2_5)).toOption.get

      val trialId = trialDao.create(createTrial.copy(treatmentArms = List(arm1, arm2, arm3), criterions = List(cr1, cr2), randomizationMethod = None)).toOption.get

      val trialDBWithOutRM = trialDao.get(trialId).toOption.get.get

      val minimizationId = minimizationDao.create(randomizationMethod = minimization, trialId = trialId).toOption.get


      def generateBlob(ob: Any): Array[Byte] = {
        val bos: ByteArrayOutputStream = new ByteArrayOutputStream()
        val oos: ObjectOutputStream = new ObjectOutputStream(bos)
        oos.writeObject(ob)
        oos.flush()
        oos.close()
        bos.close()

        bos.toByteArray
      }
      random.setSeed(1)
      database withSession {
        threadLocalSession withTransaction {
          Query(TestingEnvironmentMinimization.schema.RandomizationMethods).filter(rm => rm.id in Query(Minimizations).filter(mini => mini.id is minimizationId).map(_.randomizationMethodId)).mutate {
            r =>
              r.row = r.row.copy(_3 = generateBlob(random))
          }
        }
      }

      val minimizationDB = minimizationDao.get(minimizationId).toOption.get

      val trialDB = trialDBWithOutRM.copy(randomizationMethod = minimizationDB)

      val cr1DB = trialDB.criterions.find(entry => entry.name == "SEX").get.asInstanceOf[OrdinalCriterion]
      val cr2DB = trialDB.criterions.find(entry => entry.name == "TrialSites").get.asInstanceOf[OrdinalCriterion]

      //Test randomization sequence

      val randomCR1 = new Random(1)
      val randomCR2 = new Random(1)

      val cr2Values = List("1", "2", "3", "4", "5")

      for (i <- 0 until 60) {

        val properties = List(SubjectProperty(criterion = cr1DB, value = cr1DB.values.toList(randomCR1.nextInt(cr1DB.values.toList.size))).toOption.get,
          SubjectProperty(criterion = cr2DB, value = cr2Values(randomCR2.nextInt(cr2DB.values.toList.size))).toOption.get)

        val subject = TrialSubject(identifier = "subject" + i, investigatorUserName = "investigator", trialSite = trialDB.participatingSites.head, properties = properties).toOption.get

        val assignedArm = trialDB.randomize(subject)

        allocationSequence(i) must be(assignedArm.toOption.get.name)

        val randomizationMethodDB = minimizationDao.update(trialDB.randomizationMethod.get.asInstanceOf[Minimization])
        randomizationMethodDB.isSuccess must be(true)
        randomizationMethodDB.toOption.get.seedRandomEqualScore must be(minimizationDB.get.seedRandomEqualScore)
        randomizationMethodDB.toOption.get.p must be(minimizationDB.get.p)

      }

      trialDB.getSubjects.size must be(trialDB.plannedSubjects)
    }
  }


  describe("MinimizationDao getFromTrial method") {

    it("should be able to create a new block minimization method with parameter and without tmp data")(pending)


  }

  describe("MinimizationDao update method") {

    it("should be able to update the p value of minimization method ")(pending)

    it("should be able to update the randomEqualScore seed value of minimization method ")(pending)

    it("should be able to update the randomEqualScore value of minimization method ")(pending)

    it("should be able to update a minimization method with count trial site data")(pending)

    it("should be able to update a minimization method with count constraints data") {
      val allocationSequence = List("dummy:3", "dummy:2", "dummy:3", "dummy:1", "dummy:3",
        "dummy:1", "dummy:3", "dummy:3", "dummy:3", "dummy:3",
        "dummy:2", "dummy:2", "dummy:1", "dummy:3", "dummy:2",
        "dummy:2", "dummy:3", "dummy:2", "dummy:2", "dummy:2",
        "dummy:1", "dummy:3", "dummy:1", "dummy:3", "dummy:3",
        "dummy:2", "dummy:1", "dummy:3", "dummy:3", "dummy:2",
        "dummy:2", "dummy:1", "dummy:1", "dummy:2", "dummy:2",
        "dummy:3", "dummy:3", "dummy:2", "dummy:3", "dummy:3",
        "dummy:3", "dummy:2", "dummy:3", "dummy:1", "dummy:2",
        "dummy:3", "dummy:2", "dummy:3", "dummy:3", "dummy:3",
        "dummy:2", "dummy:3", "dummy:3", "dummy:3", "dummy:3",
        "dummy:3", "dummy:2", "dummy:3", "dummy:3", "dummy:3")

      val arm1 = createTreatmentArm.copy(name = "dummy:1", plannedSize = 10)
      val arm2 = createTreatmentArm.copy(name = "dummy:2", plannedSize = 20)
      val arm3 = createTreatmentArm.copy(name = "dummy:3", plannedSize = 30)

      val random = new JDKRandomGenerator()
      random.setSeed(1)
      val randomEqualScore = new JDKRandomGenerator()
      random.setSeed(1)

      val minimization = new Minimization(p = 0.7, seedRandomEqualScore = 1)(random = random, randomEqualScore = randomEqualScore)

      val strataCR1_M = OrdinalConstraint(configurations = List(Some("M"))).toOption.get

      val strataCR1_F = OrdinalConstraint(configurations = List(Some("F"))).toOption.get

      val cr1 = OrdinalCriterion(id = 1, name = "SEX", description = "description", values = Set("M", "F"), inclusionConstraint = None, strata = List(strataCR1_M, strataCR1_F)).toOption.get
      val strataCR2_1 = OrdinalConstraint(configurations = List(Some("1"))).toOption.get

      val strataCR2_2 = OrdinalConstraint(configurations = List(Some("2"))).toOption.get

      val strataCR2_3 = OrdinalConstraint(configurations = List(Some("3"))).toOption.get
      val strataCR2_4 = OrdinalConstraint(configurations = List(Some("4"))).toOption.get
      val strataCR2_5 = OrdinalConstraint(configurations = List(Some("5"))).toOption.get
      val cr2 = OrdinalCriterion(name = "TrialSites", description = "description", values = Set("4", "5", "1", "2", "3"), inclusionConstraint = None, strata = List(strataCR2_1, strataCR2_2, strataCR2_3, strataCR2_4, strataCR2_5)).toOption.get

      val trialId = trialDao.create(createTrial.copy(treatmentArms = List(arm1, arm2, arm3), criterions = List(cr1, cr2), randomizationMethod = None)).toOption.get

      val trialDBWithOutRM = trialDao.get(trialId).toOption.get.get

      val minimizationId = minimizationDao.create(randomizationMethod = minimization, trialId = trialId).toOption.get


      def generateBlob(ob: Any): Array[Byte] = {
        val bos: ByteArrayOutputStream = new ByteArrayOutputStream()
        val oos: ObjectOutputStream = new ObjectOutputStream(bos)
        oos.writeObject(ob)
        oos.flush()
        oos.close()
        bos.close()

        bos.toByteArray
      }
      random.setSeed(1)
      database withSession {
        threadLocalSession withTransaction {
          Query(TestingEnvironmentMinimization.schema.RandomizationMethods).filter(rm => rm.id in Query(Minimizations).filter(mini => mini.id is minimizationId).map(_.randomizationMethodId)).mutate {
            r =>
              r.row = r.row.copy(_3 = generateBlob(random))
          }
        }
      }

      val minimizationDB = minimizationDao.get(minimizationId).toOption.get

      val trialDB = trialDBWithOutRM.copy(randomizationMethod = minimizationDB)

      val cr1DB = trialDB.criterions.find(entry => entry.name == "SEX").get.asInstanceOf[OrdinalCriterion]
      val cr2DB = trialDB.criterions.find(entry => entry.name == "TrialSites").get.asInstanceOf[OrdinalCriterion]

      //Test randomization sequence

      val randomCR1 = new Random(1)
      val randomCR2 = new Random(1)

      val cr2Values = List("1", "2", "3", "4", "5")

      for (i <- 0 until 60) {

        val properties = List(SubjectProperty(criterion = cr1DB, value = cr1DB.values.toList(randomCR1.nextInt(cr1DB.values.toList.size))).toOption.get,
          SubjectProperty(criterion = cr2DB, value = cr2Values(randomCR2.nextInt(cr2DB.values.toList.size))).toOption.get)

        val subject = TrialSubject(identifier = "subject" + i, investigatorUserName = "investigator", trialSite = trialDB.participatingSites.head, properties = properties).toOption.get

        val assignedArm = trialDB.randomize(subject)

        allocationSequence(i) must be(assignedArm.toOption.get.name)

        val randomizationMethodDB = minimizationDao.update(trialDB.randomizationMethod.get.asInstanceOf[Minimization])
        randomizationMethodDB.isSuccess must be(true)
        randomizationMethodDB.toOption.get.seedRandomEqualScore must be(minimizationDB.get.seedRandomEqualScore)
        randomizationMethodDB.toOption.get.p must be(minimizationDB.get.p)

      }

      trialDB.getSubjects.size must be(trialDB.plannedSubjects)
    }
  }

}
