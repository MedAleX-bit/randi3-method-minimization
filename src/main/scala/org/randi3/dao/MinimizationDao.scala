package org.randi3.dao


import scala.slick.session.Database.threadLocalSession

import scala.slick.driver.ExtendedProfile

import scala.collection.mutable.ListBuffer
import scalaz._
import org.randi3.schema.{DatabaseSchema, MinimizationSchema}
import org.randi3.model.criterion.constraint._
import scala.collection.mutable
import org.randi3.model.{Trial, TrialSite}
import org.joda.time.LocalDate

import scala.Left
import scalaz.Failure
import scala.Some
import scalaz.Success
import org.randi3.model.criterion.constraint.Constraint
import org.randi3.randomization.Minimization
import scala.Right
import scala.slick.session.Database
import scala.slick.lifted.{Query, Parameters}

class MinimizationDao(database: Database, driver: ExtendedProfile) extends AbstractRandomizationMethodDao(database, driver) {

  import driver.Implicit._

  val schemaCore = new DatabaseSchema(driver)

  import schemaCore._

  val schemaMinimization = new MinimizationSchema(driver)

  import schemaMinimization._

  val queryMinimizationFromId = for {
    id <- Parameters[Int]
    minimization <- Minimizations if minimization.randomizationMethodId is id
  } yield minimization.*

  val queryOrdinalConstraintValuesFromConstraintId = for {
    id <- Parameters[Int]
    ordinalConstraintValues <- OrdinalConstraintValues if ordinalConstraintValues.constraintId === id
  } yield ordinalConstraintValues.value


  private def queryCountConstraintFromConstraintId(constraintId: Int) = Query(MinimizationConstraints).filter(minConstraint => minConstraint.constraintId is constraintId)

  private def queryMinimizationConstraintCounts(constraintId: Int) = {
    Query(MinimizationConstraintTreatments).filter(count => count.minimizationConstraintId in
      queryCountConstraintFromConstraintId(constraintId).map(_.id))
  }

private def queryCountTrialSiteFromSiteId(siteId: Int) = Query(MinimizationTrialSites).filter(minTrialSite => minTrialSite.trialSiteId is siteId)

  private def queryMinimizationTrialSitesCounts(siteId: Int) = {
    Query(MinimizationSiteTreatments).filter(count => count.minimizationTrialSiteId in
      queryCountTrialSiteFromSiteId(siteId).map(_.id))
  }

  def create(randomizationMethod: Minimization, trialId: Int): Validation[String, Int] = {
    database withSession {
      val identifier =
        threadLocalSession withTransaction {
          val seed = randomizationMethod.random.nextLong()
          randomizationMethod.random.setSeed(seed)
          RandomizationMethods.noId insert(trialId, generateBlob(randomizationMethod.random).get, randomizationMethod.getClass().getName(), seed)
          val id = getId(trialId).toEither match {
            case Left(x) => return Failure(x)
            case Right(id1) => id1
          }
          Minimizations.noId insert(0, Some(id), randomizationMethod.p, randomizationMethod.seedRandomEqualScore, generateBlob(randomizationMethod.randomEqualScore).get)

          id
        }
      Success(identifier)
    }
  }

  def get(id: Int): Validation[String, Option[Minimization]] = {
    database withSession {
      val resultList = queryRandomizationMethodFromId(id).list
      if (resultList.isEmpty) Success(None)
      else if (resultList.size == 1) {
        val rm = resultList(0)
        if (rm._4 == classOf[Minimization].getName) {
          val minimizationEntry = queryMinimizationFromId(id).list
          if (minimizationEntry.isEmpty) Failure("Minimization not found")
          else if (minimizationEntry.size == 1) {
            val minimization = new Minimization(id = minimizationEntry(0)._1,
              version = minimizationEntry(0)._2,
              p = minimizationEntry(0)._4,
              seedRandomEqualScore = minimizationEntry(0)._5)(
              random = deserializeRandomGenerator(rm._3),
              randomEqualScore = deserializeRandomGenerator(minimizationEntry(0)._6))


            minimization.countConstraints = getCountConstraints(rm._2, minimizationEntry(0)._1)
            minimization.countTrialSites = getCountTrialSites(rm._2, minimizationEntry(0)._1)

            Success(Some(minimization))
          } else Failure("Duplicated database entry: minimization")
        } else {
          Failure("Wrong plugin")
        }
      } else Failure("Duplicated database entry")
    }
  }

  def getFromTrialId(trialId: Int): Validation[String, Option[Minimization]] = {
    database withSession {
      val resultList = queryRandomizationMethodFromTrialId(trialId).list
      if (resultList.isEmpty) Success(None)
      else if (resultList.size == 1) {
        val rm = resultList(0)
        if (rm._4 == classOf[Minimization].getName) {
          val minimizationEntry = queryMinimizationFromId(rm._1.get).list
          if (minimizationEntry.isEmpty) Failure("Minimization not found")
          else if (minimizationEntry.size == 1) {

            val minimization = new Minimization(id = minimizationEntry(0)._1,
              version = minimizationEntry(0)._2,
              p = minimizationEntry(0)._4,
              seedRandomEqualScore = minimizationEntry(0)._5)(
              random = deserializeRandomGenerator(rm._3),
              randomEqualScore = deserializeRandomGenerator(minimizationEntry(0)._6))



            minimization.countConstraints = getCountConstraints(rm._2, minimizationEntry(0)._1)
            minimization.countTrialSites = getCountTrialSites(rm._2, minimizationEntry(0)._1)

            Success(Some(minimization))
          }
          else Failure("More than one block size found")
        } else {
          Failure("Wrong plugin")
        }
      } else Failure("Duplicated database entry")
    }
  }


  private def getConstraints(trialId: Int, minimizationId: Int): List[Constraint[Any]] = {
    val results = new ListBuffer[Constraint[Any]]
    val constraints = Query(Constraints).filter(const => {
      const.id in Query(MinimizationConstraints).filter(minConst => {
        minConst.minimizationId == minimizationId
      }).map(minConst => minConst.constraintId)
    }).list

    constraints.foreach {
      constraint =>
        results.append((
          if (constraint._3 == classOf[FreeTextConstraintExact].getName) {
            FreeTextConstraintExact(id = constraint._1, version = constraint._2, configurations = List(constraint._4))
          } else if (constraint._3 == classOf[FreeTextConstraintNotEmpty].getName) {
            FreeTextConstraintNotEmpty(id = constraint._1, version = constraint._2)
          } else if (constraint._3 == classOf[DateConstraint].getName) {
            val firstDate = if (constraint._5.isDefined) Some(new LocalDate(constraint._5.get.getTime)) else None
            val secondDate = if (constraint._6.isDefined) Some(new LocalDate(constraint._6.get.getTime)) else None
            DateConstraint(id = constraint._1, version = constraint._2, configurations = List(firstDate, secondDate))
          } else if (constraint._3 == classOf[DoubleConstraint].getName) {
            DoubleConstraint(id = constraint._1, version = constraint._2, configurations = List(constraint._7, constraint._8))
          } else if (constraint._3 == classOf[IntegerConstraint].getName) {
            IntegerConstraint(id = constraint._1, version = constraint._2, configurations = List(constraint._9, constraint._10))
          } else if (constraint._3 == classOf[OrdinalConstraint].getName) {
            val ordinalConstraintValues = queryOrdinalConstraintValuesFromConstraintId(constraint._1).list.map(entry => Some(entry))
            OrdinalConstraint(id = constraint._1, version = constraint._2, configurations = ordinalConstraintValues)
          } else return List()).asInstanceOf[Constraint[Any]]
        )
    }

    results.toList
  }

  private def getCountConstraints(trialId: Int, minimizationId: Int): Map[Constraint[Any], mutable.Map[Int, Double]] = {
    val result: mutable.Map[Constraint[Any], mutable.Map[Int, Double]] = new mutable.HashMap()

    getConstraints(trialId, minimizationId).foreach(constraint => {

      val treatmentMap: mutable.Map[Int, Double] = new mutable.HashMap()

      val treatmentCount = Query(MinimizationConstraintTreatments).filter(entry => {
        entry.minimizationConstraintId in
          Query(MinimizationConstraints)
            .filter(minConst => minConst.constraintId == constraint.id
            && minConst.minimizationId == minimizationId)
            .map(minTrial => minTrial.id)
      }).list()

      treatmentCount.foreach(treatmentCount => {
        treatmentMap.put(treatmentCount._3, treatmentCount._4)
      })

      result.put(constraint, treatmentMap)
    })

    result.toMap
  }

  private def getCountTrialSites(trialId: Int, minimizationId: Int): Map[TrialSite, mutable.Map[Int, Double]] = {
    val result: mutable.Map[TrialSite, mutable.Map[Int, Double]] = new mutable.HashMap()

    val trialSites = Query(TrialSites).filter(entry => entry.id in
      Query(MinimizationTrialSites).filter(minTrial => minTrial.minimizationId == minimizationId)
        .map(minTrial => minTrial.trialSiteId)).list()

    trialSites.foreach(ts => {
      val treatmentMap: mutable.Map[Int, Double] = new mutable.HashMap()

      val treatmentCount = Query(MinimizationSiteTreatments).filter(entry => {
        entry.minimizationTrialSiteId in
          Query(MinimizationTrialSites)
            .filter(minTrial => minTrial.trialSiteId == ts._1
            && minTrial.minimizationId == minimizationId)
            .map(minTrial => minTrial.id)
      }).list()

      treatmentCount.foreach(treatmentCount => {
        treatmentMap.put(treatmentCount._3, treatmentCount._4)
      })

      val trialSite = TrialSite(id = ts._1, version = ts._2, name = ts._3, country = ts._4, street = ts._7, postCode = ts._5, city = ts._6, password = ts._8, isActive = ts._9).toOption.get
      result.put(trialSite, treatmentMap)
    })
    result.toMap
  }

  def update(randomizationMethod: Minimization): Validation[String, Minimization] = {
    database withSession {
      threadLocalSession withTransaction {
        queryRandomizationMethodFromId(randomizationMethod.id).mutate {
          r =>
            r.row = r.row.copy(_3 = generateBlob(randomizationMethod.random).get, _4 = randomizationMethod.getClass().getName())
        }
        queryMinimizationFromId(randomizationMethod.id).mutate {
          r =>
            r.row = r.row.copy(_4 = randomizationMethod.p,
              _5 = randomizationMethod.seedRandomEqualScore,
              _6 = generateBlob(randomizationMethod.randomEqualScore).get
            )
        }

      }

      randomizationMethod.countConstraints.foreach(countConstraint => {
        val dbEntry = queryCountConstraintFromConstraintId(countConstraint._1.id).list
        //add new count entry
        if (dbEntry.isEmpty) {
          threadLocalSession withTransaction {
            MinimizationConstraints.noId insert(randomizationMethod.id, countConstraint._1.id)
          }

          val newEntryId = queryCountConstraintFromConstraintId(countConstraint._1.id).list.head._1
          threadLocalSession withTransaction {
            MinimizationConstraintTreatments.noId insertAll (countConstraint._2.toSeq.map(entry => (newEntryId, entry._1, entry._2)).toSeq: _*)
          }
        } else {
          threadLocalSession withTransaction {
            queryMinimizationConstraintCounts(countConstraint._1.id).mutate {
              r => r.row = r.row.copy(_4 = countConstraint._2.get(r.row._3).get)
            }
          }
        }
      })

      randomizationMethod.countTrialSites.foreach(countSite => {
        val dbEntry = queryCountTrialSiteFromSiteId(countSite._1.id).list
        //add new count entry
        if (dbEntry.isEmpty) {
          threadLocalSession withTransaction {
            MinimizationTrialSites.noId insert(randomizationMethod.id, countSite._1.id)
          }

          val newEntryId = queryCountConstraintFromConstraintId(countSite._1.id).list.head._1
          threadLocalSession withTransaction {
            MinimizationSiteTreatments.noId insertAll (countSite._2.toSeq.map(entry => (newEntryId, entry._1, entry._2)).toSeq: _*)
          }
        } else {
          threadLocalSession withTransaction {
            queryMinimizationTrialSitesCounts(countSite._1.id).mutate {
              r => r.row = r.row.copy(_4 = countSite._2.get(r.row._3).get)
            }
          }
        }
      })


    }
    get(randomizationMethod.id).toEither match {
      case Left(x) => Failure(x)
      case Right(None) => Failure("Method not found")
      case Right(Some(minimizationMethod)) => Success(minimizationMethod)
    }
  }

  def delete(randomizationMethod: Minimization) {
    database withSession {
      threadLocalSession withTransaction {
        queryMinimizationFromId(randomizationMethod.id).mutate {
          r =>
            r.delete()
        }

        queryRandomizationMethodFromId(randomizationMethod.id).mutate {
          r =>
            r.delete()
        }
      }
    }

  }

}
