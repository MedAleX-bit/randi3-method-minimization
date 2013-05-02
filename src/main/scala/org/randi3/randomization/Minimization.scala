package org.randi3.randomization

import org.randi3.model._
import org.apache.commons.math3.random._

import scala.collection.mutable


import org.randi3.model.criterion.constraint.Constraint
import org.randi3.model.criterion.Criterion


case class Minimization(id: Int = Int.MinValue, version: Int = 0, p: Double, seedRandomEqualScore: Long)(val random: RandomGenerator, val  randomEqualScore: RandomGenerator) extends RandomizationMethod {


  private var probabilitiesPerPreferredTreatment: mutable.Map[Int, mutable.Map[Int, Double]] = null

  var countConstraints: Map[Constraint[Any], mutable.Map[Int, Double]] = null

  var countTrialSites: Map[TrialSite, mutable.Map[Int, Double]] = null


  def randomize(trial: Trial, subject: TrialSubject): TreatmentArm = {

    if (probabilitiesPerPreferredTreatment == null) initProbabilitiesPerPreferredTreatment(trial)
    if (countConstraints == null || countConstraints.isEmpty) initCountPerConstraint(trial)
    if ((countTrialSites == null || countTrialSites.isEmpty) && trial.isStratifiedByTrialSite) initCountTrialSite(trial)

    doRandomizeBiasedCoinMinimization(trial, subject)

  }

  private def doRandomizeBiasedCoinMinimization(trial: Trial, subject: TrialSubject): TreatmentArm = {


    val relevantConstraints: Map[Constraint[Any], mutable.Map[Int, Double]] = countConstraints.filter(entry => {
      val constraintIds = subject.properties.map(prop => prop.criterion.asInstanceOf[Criterion[Any, Constraint[Any]]].stratify(prop.value).get).map(constraint => constraint.id)

      constraintIds.contains(entry._1.id)
    })



    val relevantTrialSite: Map[Int, Double] = if (trial.isStratifiedByTrialSite) {
      countTrialSites.get(subject.trialSite).get.toMap
    } else Map()


    val listAllRelevantValues: List[Map[Int, Double]] = if (relevantTrialSite.isEmpty) {
      relevantConstraints.map(value => value._2.toMap).toList
    } else relevantTrialSite :: relevantConstraints.map(value => value._2.toMap).toList



    val imbalanceScores: Map[Int, Double] = trial.treatmentArms.map(arm => {
      def marginalBalanceNumerator(adjustedCounts: List[Double]): Double = {
        if (adjustedCounts.size <= 1) 0.0
        else {
          (adjustedCounts.tail.map(nextValue => scala.math.abs(adjustedCounts.head - nextValue)).sum
            + marginalBalanceNumerator(adjustedCounts.tail))
        }
      }

      val marginalBalances = listAllRelevantValues.map(relevantValue => {

        val adjustedCounts = relevantValue.map(entry => {
          val adjustedCount = if (entry._1 == arm.id) (entry._2 + 1.0) else entry._2
          val normalizedCount = adjustedCount / trial.treatmentArms.find(tmpArm => tmpArm.id == entry._1).get.plannedSize
          entry._1 -> normalizedCount
        })

        val numerator = marginalBalanceNumerator(adjustedCounts.toList.map(entry => entry._2))
        val denominator = (trial.treatmentArms.size - 1.0) * adjustedCounts.toList.map(entry => entry._2).sum
        numerator / denominator
      })

      arm.id -> marginalBalances.sum
    }).toMap

    val minValue = imbalanceScores.map(entry => entry._2).min

    val armsWithSameScore = imbalanceScores.filter(entry => entry._2 == minValue).map(entry => entry._1).toList


    //get all with min value
    val a: List[(TreatmentArm, Double)] = {
      //==1 Default case one treatment arm with smallest imbalance score
      //all treatment with same score, calculate probability with ratio
      //other cases take randomly one treatment
      if (armsWithSameScore.size == trial.treatmentArms.size) {

        trial.treatmentArms.map(arm => arm -> ((arm.plannedSize * 1.0) / (trial.plannedSubjects * 1.0)))

      } else {
        val preferredArm = if (armsWithSameScore.size == 1) {
          trial.treatmentArms.find(arm => arm.id == armsWithSameScore(0)).get
        } else {
          val armObjectsWithSameScore = trial.treatmentArms.filter(arm => armsWithSameScore.contains(arm.id))
          armObjectsWithSameScore(randomEqualScore.nextInt(armObjectsWithSameScore.size))
        }

        trial.treatmentArms.map(arm => arm -> probabilitiesPerPreferredTreatment.get(preferredArm.id).get.get(arm.id).get)

      }
    }

    //get Treatment arm with calculated p-values
    val randomNumber = random.nextDouble()

    var sum: Double = 0.0
    var i: Int = 0
    var arm: TreatmentArm = null

    while (i < a.size && arm == null) {
      sum += a(i)._2
      if (randomNumber < sum) {
        arm = a(i)._1
      }
      i += 1
    }

    //increase counter
    for (constraint <- relevantConstraints.keySet) {
     countConstraints.get(constraint).get.put(arm.id, (countConstraints.get(constraint).get.get(arm.id).get + 1.0))
    }
    if (trial.isStratifiedByTrialSite) {
      countTrialSites.get(subject.trialSite).get.put(arm.id, countTrialSites.get(subject.trialSite).get.get(arm.id).get + 1.0)
    }

        arm
  }


  private def initCountPerConstraint(trial: Trial) {

    countConstraints = trial.criterions.flatMap(criterion => criterion.strata.toSeq).map(constraint => {
      constraint.asInstanceOf[Constraint[Any]] -> mutable.Map(trial.treatmentArms.map(arm => arm.id -> 0.0).toSeq: _*)
    }).toMap
  }

  private def initCountTrialSite(trial: Trial) {

    countTrialSites = trial.participatingSites.map(site => {
      site -> mutable.Map(trial.treatmentArms.map(arm => arm.id -> 0.0).toSeq: _*)
    }).toMap
  }

  /**
   * Calculate the probabilities per preferred treatment arm (Biased Coin Minimization)
   */
  private def initProbabilitiesPerPreferredTreatment(trial: Trial) {

    probabilitiesPerPreferredTreatment = new mutable.HashMap()


    val minArm = trial.treatmentArms.min(Ordering.by((_: TreatmentArm).plannedSize))

    for (prefArm <- trial.treatmentArms) {
      val probabilities: mutable.Map[Int, Double] = new mutable.HashMap()

      var denominatorPH_pref = 0.0
      var numeratorPH_pref = 0.0

      for (arm <- trial.treatmentArms) {
        if (!arm.equals(prefArm)) {
          denominatorPH_pref += arm.plannedSize
        }
        if (!arm.equals(trial.treatmentArms.head)) {
          numeratorPH_pref += arm.plannedSize
        }
      }

      val pH_pref = if (prefArm.equals(minArm)) {
        p
      } else {
        1.0 - (denominatorPH_pref / numeratorPH_pref) * (1 - p)
      }

      val numerator: Double = trial.treatmentArms.filter(arm => !arm.equals(prefArm))
        .map(arm => arm.plannedSize)
        .reduce((acc, entry) => acc + entry)

      val pL_without_ri = (1 - pH_pref) / numerator

      for (arm <- trial.treatmentArms) {
        if (arm.equals(prefArm)) {
          probabilities.put(arm.id, pH_pref)
        } else {
          probabilities.put(arm.id, (pL_without_ri * arm.plannedSize))
        }
      }
      probabilitiesPerPreferredTreatment.put(prefArm.id, probabilities)
    }
  }


  /**
   * Necessary to test the algorithm, returns the probabilities per preferred treatment arm
   */
  def getProbabilitiesPerPreferredTreatment(trial: Trial): mutable.Map[Int, mutable.Map[Int, Double]] = {
    if (probabilitiesPerPreferredTreatment == null) initProbabilitiesPerPreferredTreatment(trial)
    probabilitiesPerPreferredTreatment
  }

  //
  //        /**
  //        * Necessary to reset the algorithm for simulation.
  //        */
  //        public void clear(){
  //        MinimizationTempData tempData = (MinimizationTempData) configuration.getTempData();
  //        tempData.setCountConstraints(null);
  //        tempData.setCountTrialSites( null);
  //        }


}
