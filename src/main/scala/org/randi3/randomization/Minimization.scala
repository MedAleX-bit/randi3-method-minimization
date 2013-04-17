package org.randi3.randomization

import org.randi3.model._
import org.apache.commons.math3.random._
import scala.collection.mutable.ListBuffer


import scala.collection.mutable


import org.randi3.model.criterion.constraint.Constraint
import org.randi3.model.criterion.Criterion


case class Minimization(id: Int = Int.MinValue, version: Int = 0, p: Double, withRandomizedSubjects: Boolean = false, biasedCoinMinimization: Boolean = false)(val random: RandomGenerator) extends RandomizationMethod {



  var probabilitiesPerPreferredTreatment: mutable.Map[TreatmentArm, mutable.Map[TreatmentArm, Double]] = null

  var countConstraints: mutable.Map[Constraint[Any], mutable.Map[TreatmentArm, Double]] = null

  var countTrialSites: mutable.Map[TrialSite, mutable.Map[TreatmentArm, Double]] = null


  def randomize(trial: Trial, subject: TrialSubject): TreatmentArm = {
    if (biasedCoinMinimization){
      if(probabilitiesPerPreferredTreatment == null) initProbabilitiesPerPreferredTreatment(trial)
      doRandomizeBiasedCoinMinimization(trial, subject)
    }else{
      doRandomizeNaiveMinimization(trial, subject)
    }
  }


  private def doRandomizeNaiveMinimization(trial: Trial, subject: TrialSubject): TreatmentArm = {
    //calculate the p-values for this allocation
    val arms = trial.treatmentArms
    val a = new ListBuffer[Double]()

    for (arm <- trial.treatmentArms) {
      val plannedSubjects = if (withRandomizedSubjects) (arm.plannedSize - arm.subjects.size) else arm.plannedSize
      var sum = 0.0
      var totalPlannedSubjects = 0.0
      for (arm1 <- trial.treatmentArms) {
        if (!arm.equals(arm1)) {
          sum += (if (withRandomizedSubjects) (arm1.plannedSize - arm1.subjects.size) else arm1.plannedSize)
        }
        totalPlannedSubjects += arm1.plannedSize
      }
      //Formula from: "Randomization by minimization for unbalanced treatment allocation" Baoguang Han, et al.
      var value = plannedSubjects * p + (1.0 - p) / (trial.treatmentArms.size - 1.0) * sum
      value = value / (if (withRandomizedSubjects) totalPlannedSubjects - trial.plannedSubjects else totalPlannedSubjects)
      a.append(value)

    }
    //get Treatment arm with calculated p-values
    val randomNumber = random.nextDouble()
    var sum: Double = 0.0
    var i: Int = 0
    var arm: TreatmentArm = null

    while (i < a.size && arm == null) {
      sum += a(i)
      if (randomNumber < sum) {
        arm = arms(i)
      }
      i += 1
    }
    return arm
  }

  /**
   * Calculate the probabilities per preferred treatment arm (Biased Coin Minimization)
   */
  private def initProbabilitiesPerPreferredTreatment(trial: Trial) {

    probabilitiesPerPreferredTreatment = new mutable.HashMap()


    var minArm = trial.treatmentArms.head

    for (arm <- trial.treatmentArms) {
      if (arm.plannedSize < minArm.plannedSize) {
        minArm = arm
      }
    }

    for (prefArm <- trial.treatmentArms) {
      val probabilities: mutable.Map[TreatmentArm, Double] = new mutable.HashMap()

      var pH_pref = 0.0
      var denuminator = 0.0
      var numinator = 0.0

      for (arm <- trial.treatmentArms) {
        if (!arm.equals(prefArm)) {
          denuminator += arm.plannedSize
        }
        if (!arm.equals(trial.treatmentArms.head)) {
          numinator += arm.plannedSize
        }
      }
      if (prefArm.equals(minArm)) {
        pH_pref = p
      } else {
        pH_pref = 1.0 - (denuminator / numinator) * (1 - p)
      }
      numinator = 0.0
      for (arm <- trial.treatmentArms) {
        if (!arm.equals(prefArm)) {
          numinator += arm.plannedSize
        }
      }
      val pL_without_ri = (1 - pH_pref) / numinator

      for (arm <- trial.treatmentArms) {
        if (arm.equals(prefArm)) {
          probabilities.put(arm, pH_pref)
        } else {
          probabilities.put(arm, (pL_without_ri * arm.plannedSize))
        }
      }
      probabilitiesPerPreferredTreatment.put(prefArm, probabilities)
    }
  }


 private def doRandomizeBiasedCoinMinimization(trial: Trial, subject: TrialSubject): TreatmentArm = {


   val relevantConstraints: mutable.Map[Constraint[Any], mutable.Map[TreatmentArm, Double]] = new mutable.HashMap()

   var relevantTrialSite: mutable.Map[TreatmentArm, Double] = null

   if (probabilitiesPerPreferredTreatment == null) initProbabilitiesPerPreferredTreatment(trial)
   //Counter for trial sites
   if (trial.isStratifiedByTrialSite) {
     if (countTrialSites == null) countTrialSites = new mutable.HashMap()

     val actMap = countTrialSites.get(subject.trialSite).getOrElse({
       val tmpMap: mutable.Map[TreatmentArm, Double] = new mutable.HashMap()

       for (arm <- trial.treatmentArms) {
         tmpMap.put(arm, 0.0);
       }
       countTrialSites.put(subject.trialSite, tmpMap)
       tmpMap
     })
     relevantTrialSite = actMap
   }

   if (countConstraints == null) countConstraints = new mutable.HashMap()


   //Get relevant constraints and if necessary create a new counter
   for (prop <- subject.properties) {

     val actMap = countConstraints.get((prop.criterion.asInstanceOf[Criterion[Any, Constraint[Any]]]).stratify(prop.value).get).getOrElse({

       val tmpMap: mutable.HashMap[TreatmentArm, Double] = new mutable.HashMap()

       for (arm <- trial.treatmentArms) {
         tmpMap.put(arm, 0.0);
       }
       countConstraints.put((prop.criterion.asInstanceOf[Criterion[Any, Constraint[Any]]]).stratify(prop.value).get, tmpMap)
       tmpMap
     })

     relevantConstraints.put((prop.criterion.asInstanceOf[Criterion[Any, Constraint[Any]]]).stratify(prop.value).get, actMap);
   }

    //calculate imbalance scores
    val imbalacedScores: mutable.HashMap[TreatmentArm, Double] = new mutable.HashMap()

    for(arm  <- trial.treatmentArms){
      var imbalacedScore = 0.0

     val listAllRelevantValues: ListBuffer[mutable.Map[TreatmentArm, Double]] = new ListBuffer()

      if(trial.isStratifiedByTrialSite){
        listAllRelevantValues.append(relevantTrialSite)
      }
      listAllRelevantValues.appendAll(relevantConstraints.values)

      for( mapW <- listAllRelevantValues){
        val adjustetCountsPerArm = new Array[Double] (trial.treatmentArms.size)

        var  i = 0

        for(actArm <- mapW.keySet){

          var adjustetCount = {

          if(actArm.id == arm.id){
             mapW.get(actArm).get + 1.0
          }else{
             mapW.get(actArm).get
          }
          }

          //calculate adjusted counts
          adjustetCount = adjustetCount / actArm.plannedSize
          adjustetCountsPerArm(i) = adjustetCount
          i += 1
        }
        //calculate marginal balance
        var marginalBalance = 0.0
        var numerator = 0.0

        for(x <- 0 until adjustetCountsPerArm.size){

          for(j <- x+1 until adjustetCountsPerArm.size){
            marginalBalance += scala.math.abs(adjustetCountsPerArm(x)-adjustetCountsPerArm(j))
          }
          numerator += adjustetCountsPerArm(x)
        }

        numerator+=adjustetCountsPerArm.last
        numerator =(adjustetCountsPerArm.length-1.0) * numerator
        marginalBalance = marginalBalance/numerator

        imbalacedScore+= marginalBalance
      }
      imbalacedScores.put(arm, imbalacedScore)
    }

    //find preferred treatment
    var tmpMinValue = Double.MaxValue

   val armsWithSameScore: ListBuffer[TreatmentArm] = new ListBuffer()

    for(arm <- imbalacedScores.keySet){
      if(imbalacedScores.get(arm).get < tmpMinValue){
        armsWithSameScore.clear
        tmpMinValue = imbalacedScores.get(arm).get
        armsWithSameScore.append(arm)
      }else if (imbalacedScores.get(arm).get == tmpMinValue){
        armsWithSameScore.append(arm)
      }
    }
    //get all with min value
   val a: ListBuffer[Double] = new ListBuffer()

    //==1 Default case one treatment arm with smallest imbalance score
    //all treatment with same score, calculate probability with ratio
    //other cases take randomly one treatment
    if(armsWithSameScore.size==1){
      for( arm <-  trial.treatmentArms){
        a.append(probabilitiesPerPreferredTreatment.get(armsWithSameScore(0)).get.get(arm).get)
      }
    }else if(armsWithSameScore.size == trial.treatmentArms.size){
      for(arm <- trial.treatmentArms){
        a.append(((arm.plannedSize*1.0)/(trial.plannedSubjects*1.0)))
      }
    }else{
      val preferredArm = trial.treatmentArms(random.nextInt(trial.treatmentArms.size))
      for(arm <- trial.treatmentArms){
        a.append(probabilitiesPerPreferredTreatment.get(preferredArm).get.get(arm).get)
      }
    }

    //get Treatment arm with calculated p-values
    val randomNumber = random.nextDouble()
    var sum: Double = 0.0
    var i: Int = 0
    var arm: TreatmentArm = null

    while( i < a.size && arm == null){
      sum+=a(i)
      if(randomNumber < sum){
        arm = trial.treatmentArms(i)
        }
        i += 1
    }

        //increase the count for the relevant constraints
        for(constraint <- relevantConstraints.keySet){
          countConstraints.get(constraint).get.put(arm, (countConstraints.get(constraint).get.get(arm).get +1.0))
        }
        if(trial.isStratifiedByTrialSite){
        countTrialSites.get(subject.trialSite).get.put(arm, countTrialSites.get(subject.trialSite).get.get(arm).get +1.0)
        }
        return arm;
   }


//
//
//        /**
//        * Necessary to test the algorithm.
//        * @return the probabilities per preferred treatment arm
//        */
//        public Map<TreatmentArm, MinimizationMapElementWrapper> getProbabilitiesPerPreferredTreatment() {
//          if(((MinimizationTempData) configuration.getTempData()).getProbabilitiesPerPreferredTreatment()==null) initProbabilitiesPerPreferredTreatment();
//        return ((MinimizationTempData) configuration.getTempData()).getProbabilitiesPerPreferredTreatment();
//        }
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
