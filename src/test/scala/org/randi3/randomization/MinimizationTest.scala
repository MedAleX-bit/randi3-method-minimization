package org.randi3.randomization

import org.junit.runner.RunWith
import org.scalatest.matchers.MustMatchers
import org.apache.commons.math3.random.{JDKRandomGenerator, MersenneTwister}
import org.randi3.model._
import org.randi3.model.criterion.OrdinalCriterion
import org.randi3.model.criterion.constraint.OrdinalConstraint
import scala.Some
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSpec
import java.util.Random

@RunWith(classOf[JUnitRunner])
class MinimizationTest extends FunSpec with MustMatchers {

  import org.randi3.utility.TestingEnvironmentMinimization._


  describe("The biased coin minimization method") {

    it("should be able to randomize subject in the same order like in file Minimization_BiasedCoinExample") {
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
        "dummy:3", "dummy:2", "dummy:3", "dummy:3", "dummy:3");

      val arm1 = createTreatmentArm.copy(id = 1, name = "dummy:1", plannedSize = 10)
      val arm2 = createTreatmentArm.copy(id = 2, name = "dummy:2", plannedSize = 20)
      val arm3 = createTreatmentArm.copy(id = 3, name = "dummy:3", plannedSize = 30)

      val random = new JDKRandomGenerator()
      random.setSeed(1)
      val randomEqualScore = new JDKRandomGenerator()
      random.setSeed(1)
      val minimization = new Minimization(p = 0.7, seedRandomEqualScore = 1)(random = random, randomEqualScore = randomEqualScore)

      val strataCR1_M = OrdinalConstraint(id = 2, configurations = List(Some("M"))).toOption.get

      val strataCR1_F = OrdinalConstraint(id = 1, configurations = List(Some("F"))).toOption.get

      val cr1 = OrdinalCriterion(id = 1, name = "SEX", description = "description", values = Set("M", "F"), inclusionConstraint = None, strata = List(strataCR1_M, strataCR1_F)).toOption.get
      val strataCR2_1 = OrdinalConstraint(id = 3, configurations = List(Some("1"))).toOption.get

      val strataCR2_2 = OrdinalConstraint(id = 4, configurations = List(Some("2"))).toOption.get

      val strataCR2_3 = OrdinalConstraint(id = 5, configurations = List(Some("3"))).toOption.get
      val strataCR2_4 = OrdinalConstraint(id = 6, configurations = List(Some("4"))).toOption.get
      val strataCR2_5 = OrdinalConstraint(id = 7, configurations = List(Some("5"))).toOption.get
      val cr2 = OrdinalCriterion(id = 2, name = "TrialSites", description = "description", values = Set("4", "5", "1", "2", "3"), inclusionConstraint = None, strata = List(strataCR2_1, strataCR2_2, strataCR2_3, strataCR2_4, strataCR2_5)).toOption.get

      val trial = createTrial.copy(treatmentArms = List(arm1, arm2, arm3), randomizationMethod = Some(minimization), criterions = List(cr1, cr2))


      //Test calculated probabilities
      val internalCalculatedProbabilities = minimization.getProbabilitiesPerPreferredTreatment(trial)
      val externalCalculatedProbabilities = Map(
        arm1 -> Map(arm1.id -> 0.7, arm2.id -> 0.12, arm3.id -> 0.18),
        arm2 -> Map(arm1.id -> 0.06, arm2.id -> 0.76, arm3.id -> 0.18),
        arm3 -> Map(arm1.id -> 0.06, arm2.id -> 0.12, arm3.id -> 0.82)
      )


      checkProbabilitiesEquals(internalCalculatedProbabilities.get(arm1.id).get.toMap, externalCalculatedProbabilities.get(arm1).get) must be(true)
      checkProbabilitiesEquals(internalCalculatedProbabilities.get(arm2.id).get.toMap, externalCalculatedProbabilities.get(arm2).get) must be(true)
      checkProbabilitiesEquals(internalCalculatedProbabilities.get(arm3.id).get.toMap, externalCalculatedProbabilities.get(arm3).get) must be(true)

      //Test randomization sequence

      val randomCR1 = new Random(1)
      val randomCR2 = new Random(1)
      val cr2Values = List("1", "2", "3", "4", "5")

      for (i <- 0 until 60) {
        val properties = List(SubjectProperty(criterion = cr1, value = cr1.values.toList(randomCR1.nextInt(cr1.values.toList.size))).toOption.get,
          SubjectProperty(criterion = cr2, value = cr2Values(randomCR2.nextInt(cr2.values.toList.size))).toOption.get)

        val subject = TrialSubject(identifier = "subject" + i, investigatorUserName = "investigator", trialSite = trial.participatingSites.head, properties = properties).toOption.get

        val assignedArm = trial.randomize(subject).toOption.get

        assignedArm.name must be(allocationSequence(i))
      }

      trial.getSubjects.size must be(trial.plannedSubjects)
    }


    it("should be able to calculate the same probabilities like in Minimization_Prop_Table (1).pdf") {

      val arm1 = createTreatmentArm.copy(id = 1, name = "dummy:1", plannedSize = 10)
      val arm2 = createTreatmentArm.copy(id = 2, name = "dummy:2", plannedSize = 20)
      val arm3 = createTreatmentArm.copy(id = 3, name = "dummy:3", plannedSize = 30)


      val minimization = new Minimization(p = 0.7, seedRandomEqualScore = 1)(random = new MersenneTwister(), randomEqualScore = new MersenneTwister(1))


      val trial = createTrial.copy(treatmentArms = List(arm1, arm2, arm3), randomizationMethod = Some(minimization))


      //Test calculated probabilities
      val internalCalculatedProbabilities = minimization.getProbabilitiesPerPreferredTreatment(trial)
      val externalCalculatedProbabilities = Map(
        arm1 -> Map(arm1.id -> 0.7, arm2.id -> 0.12, arm3.id -> 0.18),
        arm2 -> Map(arm1.id -> 0.06, arm2.id -> 0.76, arm3.id -> 0.18),
        arm3 -> Map(arm1.id -> 0.06, arm2.id -> 0.12, arm3.id -> 0.82)
      )


      checkProbabilitiesEquals(internalCalculatedProbabilities.get(arm1.id).get.toMap, externalCalculatedProbabilities.get(arm1).get) must be(true)
      checkProbabilitiesEquals(internalCalculatedProbabilities.get(arm2.id).get.toMap, externalCalculatedProbabilities.get(arm2).get) must be(true)
      checkProbabilitiesEquals(internalCalculatedProbabilities.get(arm3.id).get.toMap, externalCalculatedProbabilities.get(arm3).get) must be(true)

    }

    it("should be able to calculate the same probabilities like in Minimization_Prop_Table (2).pdf") {

      val arm1 = createTreatmentArm.copy(id = 1, name = "dummy:1", plannedSize = 10)
      val arm2 = createTreatmentArm.copy(id = 2, name = "dummy:2", plannedSize = 20)
      val arm3 = createTreatmentArm.copy(id = 3, name = "dummy:3", plannedSize = 30)


      val minimization = new Minimization(p = 0.8, seedRandomEqualScore = 1)(random = new MersenneTwister(), randomEqualScore = new MersenneTwister(1))


      val trial = createTrial.copy(treatmentArms = List(arm1, arm2, arm3), randomizationMethod = Some(minimization))


      //Test calculated probabilities
      val internalCalculatedProbabilities = minimization.getProbabilitiesPerPreferredTreatment(trial)
      val externalCalculatedProbabilities = Map(
        arm1 -> Map(arm1.id -> 0.8, arm2.id -> 0.08, arm3.id -> 0.12),
        arm2 -> Map(arm1.id -> 0.04, arm2.id -> 0.84, arm3.id -> 0.12),
        arm3 -> Map(arm1.id -> 0.04, arm2.id -> 0.08, arm3.id -> 0.88)
      )


      checkProbabilitiesEquals(internalCalculatedProbabilities.get(arm1.id).get.toMap, externalCalculatedProbabilities.get(arm1).get) must be(true)
      checkProbabilitiesEquals(internalCalculatedProbabilities.get(arm2.id).get.toMap, externalCalculatedProbabilities.get(arm2).get) must be(true)
      checkProbabilitiesEquals(internalCalculatedProbabilities.get(arm3.id).get.toMap, externalCalculatedProbabilities.get(arm3).get) must be(true)

    }


    it("should be able to calculate the same probabilities like in Minimization_Prop_Table (3).pdf") {

      val arm1 = createTreatmentArm.copy(id = 1, name = "dummy:1", plannedSize = 10)
      val arm2 = createTreatmentArm.copy(id = 2, name = "dummy:2", plannedSize = 20)
      val arm3 = createTreatmentArm.copy(id = 3, name = "dummy:3", plannedSize = 30)


      val minimization = new Minimization(p = 0.6, seedRandomEqualScore = 1)(random = new MersenneTwister(), randomEqualScore = new MersenneTwister(1))


      val trial = createTrial.copy(treatmentArms = List(arm1, arm2, arm3), randomizationMethod = Some(minimization))


      //Test calculated probabilities
      val internalCalculatedProbabilities = minimization.getProbabilitiesPerPreferredTreatment(trial)
      val externalCalculatedProbabilities = Map(
        arm1 -> Map(arm1.id -> 0.6, arm2.id -> 0.16, arm3.id -> 0.24),
        arm2 -> Map(arm1.id -> 0.08, arm2.id -> 0.68, arm3.id -> 0.24),
        arm3 -> Map(arm1.id -> 0.08, arm2.id -> 0.16, arm3.id -> 0.76)
      )


      checkProbabilitiesEquals(internalCalculatedProbabilities.get(arm1.id).get.toMap, externalCalculatedProbabilities.get(arm1).get) must be(true)
      checkProbabilitiesEquals(internalCalculatedProbabilities.get(arm2.id).get.toMap, externalCalculatedProbabilities.get(arm2).get) must be(true)
      checkProbabilitiesEquals(internalCalculatedProbabilities.get(arm3.id).get.toMap, externalCalculatedProbabilities.get(arm3).get) must be(true)

    }

    it("should be able to calculate the same probabilities like in Minimization_Prop_Table (4).pdf") {

      val arm1 = createTreatmentArm.copy(id = 1, name = "dummy:1", plannedSize = 10)
      val arm2 = createTreatmentArm.copy(id = 2, name = "dummy:2", plannedSize = 10)
      val arm3 = createTreatmentArm.copy(id = 3, name = "dummy:3", plannedSize = 10)


      val minimization = new Minimization(p = 0.6, seedRandomEqualScore = 1)(random = new MersenneTwister(), randomEqualScore = new MersenneTwister(1))


      val trial = createTrial.copy(treatmentArms = List(arm1, arm2, arm3), randomizationMethod = Some(minimization))


      //Test calculated probabilities
      val internalCalculatedProbabilities = minimization.getProbabilitiesPerPreferredTreatment(trial)
      val externalCalculatedProbabilities = Map(
        arm1 -> Map(arm1.id -> 0.6, arm2.id -> 0.2, arm3.id -> 0.2),
        arm2 -> Map(arm1.id -> 0.2, arm2.id -> 0.6, arm3.id -> 0.2),
        arm3 -> Map(arm1.id -> 0.2, arm2.id -> 0.2, arm3.id -> 0.6)
      )


      checkProbabilitiesEquals(internalCalculatedProbabilities.get(arm1.id).get.toMap, externalCalculatedProbabilities.get(arm1).get) must be(true)
      checkProbabilitiesEquals(internalCalculatedProbabilities.get(arm2.id).get.toMap, externalCalculatedProbabilities.get(arm2).get) must be(true)
      checkProbabilitiesEquals(internalCalculatedProbabilities.get(arm3.id).get.toMap, externalCalculatedProbabilities.get(arm3).get) must be(true)

    }

    it("should be able to calculate the same probabilities like in Minimization_Prop_Table (5).pdf") {

      val arm1 = createTreatmentArm.copy(id = 1, name = "dummy:1", plannedSize = 1)
      val arm2 = createTreatmentArm.copy(id = 2, name = "dummy:2", plannedSize = 2)
      val arm3 = createTreatmentArm.copy(id = 3, name = "dummy:3", plannedSize = 2)
      val arm4 = createTreatmentArm.copy(id = 4, name = "dummy:4", plannedSize = 4)


      val minimization = new Minimization(p = 0.8, seedRandomEqualScore = 1)(random = new MersenneTwister(), randomEqualScore = new MersenneTwister(1))


      val trial = createTrial.copy(treatmentArms = List(arm1, arm2, arm3, arm4), randomizationMethod = Some(minimization))


      //Test calculated probabilities
      val internalCalculatedProbabilities = minimization.getProbabilitiesPerPreferredTreatment(trial)
      val externalCalculatedProbabilities = Map(
        arm1 -> Map(arm1.id -> 0.8, arm2.id -> 0.05, arm3.id -> 0.05, arm4.id -> 0.1),
        arm2 -> Map(arm1.id -> 0.025, arm2.id -> 0.825, arm3.id -> 0.05, arm4.id -> 0.1),
        arm3 -> Map(arm1.id -> 0.025, arm2.id -> 0.05, arm3.id -> 0.825, arm4.id -> 0.1),
        arm4 -> Map(arm1.id -> 0.025, arm2.id -> 0.05, arm3.id -> 0.05, arm4.id -> 0.875)
      )


      checkProbabilitiesEquals(internalCalculatedProbabilities.get(arm1.id).get.toMap, externalCalculatedProbabilities.get(arm1).get) must be(true)
      checkProbabilitiesEquals(internalCalculatedProbabilities.get(arm2.id).get.toMap, externalCalculatedProbabilities.get(arm2).get) must be(true)
      checkProbabilitiesEquals(internalCalculatedProbabilities.get(arm3.id).get.toMap, externalCalculatedProbabilities.get(arm3).get) must be(true)
      checkProbabilitiesEquals(internalCalculatedProbabilities.get(arm4.id).get.toMap, externalCalculatedProbabilities.get(arm4).get) must be(true)

    }

  }

  private def checkProbabilitiesEquals(map1: Map[Int, Double], map2: Map[Int, Double]): Boolean = {
    if (map1.size != map2.size) return false

    map1.map(entry => {
      val value1 = BigDecimal(entry._2).setScale(5, BigDecimal.RoundingMode.HALF_UP).toDouble
      val value2 = BigDecimal(map2.get(entry._1).get).setScale(5, BigDecimal.RoundingMode.HALF_UP).toDouble
      value1 == value2
    }).reduce((acc, entry) => acc && entry)

  }

}



