package org.randi3.dao

import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.ql._
import org.scalaquery.ql.TypeMapper._
import org.scalaquery.ql.extended.ExtendedProfile

import org.randi3.randomization.Minimization
import scala.collection.mutable.ListBuffer
import scalaz._
import org.randi3.schema.{DatabaseSchema, MinimizationSchema}

class MinimizationDao(database: Database, driver: ExtendedProfile) extends AbstractRandomizationMethodDao(database, driver) {

  import driver.Implicit._

  val schemaCore = new DatabaseSchema(driver)
  import schemaCore._
  val schemaMinimization = new MinimizationSchema(driver)
  import schemaMinimization._


  def create(randomizationMethod: Minimization, trialId: Int): Validation[String, Int] = {
    database withSession {
      Failure("")
    }
  }

  def get(id: Int): Validation[String, Option[Minimization]] = {
    database withSession {
      Failure("")
    }
  }

  def getFromTrialId(trialId: Int): Validation[String, Option[Minimization]] = {
    database withSession {
      Failure("")
    }
  }


  def update(randomizationMethod: Minimization): Validation[String, Minimization] = {
    database withSession {
      Failure("")
    }
    get(randomizationMethod.id).either match {
      case Left(x) => Failure(x)
      case Right(None) => Failure("Method not found")
      case Right(Some(minimizationMethod)) => Success(minimizationMethod)
    }
  }

  def delete(randomizationMethod: Minimization) {
    database withSession {
      Failure("")
    }

  }

}
