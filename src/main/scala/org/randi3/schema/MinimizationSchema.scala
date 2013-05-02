package org.randi3.schema

import org.scalaquery.ql._
import org.scalaquery.ql.TypeMapper._
import org.scalaquery.ql.extended.{ExtendedTable => Table}
import org.scalaquery.ql.extended._
import org.scalaquery.session._
import org.scalaquery.session.Database.threadLocalSession


/**
 * A simple example that uses statically typed queries against an in-memory
 * H2 database. The example data comes from Oracle's JDBC tutorial at
 * http://download.oracle.com/javase/tutorial/jdbc/basics/tables.html.
 */
class MinimizationSchema(driver: ExtendedProfile) {
  import driver.Implicit._

  val schema = new DatabaseSchema(driver)

  object Minimizations extends Table[(Int, Int, Option[Int], Double, Long, Array[Byte])]("Minimization") {
    def id = column[Int]("id", O PrimaryKey, O AutoInc)

    def version = column[Int]("Version", O NotNull)

    def randomizationMethodId = column[Option[Int]]("RandomizationMethodId")

    def p = column[Double]("ProbPreferredTreatment", O Nullable)

    def seedRandomEqualScore = column[Long]("SeedRandomEqualScore", O NotNull)

    def randomEqualScoreGenerator = column[Array[Byte]]("RandomGenerator", O NotNull)(PostgresByteArrayTypeMapper)

      def * = id ~ version ~ randomizationMethodId ~ p ~ seedRandomEqualScore ~ randomEqualScoreGenerator

    def noId = version ~ randomizationMethodId ~ p ~ seedRandomEqualScore ~ randomEqualScoreGenerator

    def randomizationMethod = foreignKey("MinimizationFK_RandomizationMethod", randomizationMethodId, schema.RandomizationMethods)(_.id)
  }


  object MinimizationConstraints extends Table[(Int, Int, Int)]("MinimizationConstraint") {
    def id = column[Int]("id", O PrimaryKey, O AutoInc)

    def minimizationId = column[Int]("MinimizationId")

    def constraintId = column[Int]("ConstraintId")

    def * = id ~ minimizationId ~ constraintId

    def noId = minimizationId ~ constraintId

    def randomizationMethod = foreignKey("MinimizationConstraintFK_Minimization", minimizationId, Minimizations)(_.id)

    def constraint = foreignKey("MinimizationFK_Constraint", constraintId, schema.Constraints)(_.id)
  }

  object MinimizationConstraintTreatments extends Table[(Int, Int, Int, Double)]("MinimizationConstraintTreatmentCounts") {
    def id = column[Int]("id", O PrimaryKey, O AutoInc)

    def minimizationConstraintId = column[Int]("MinimizationConstraintId")

    def treatmentId = column[Int]("TreatmentId")

    def count = column[Double]("Count")

    def * = id ~ minimizationConstraintId ~ treatmentId ~ count

    def noId = minimizationConstraintId ~ treatmentId ~ count

    def minimizationConstraint = foreignKey("MinimizationConstraintTreatmentFK_Constraint", minimizationConstraintId, MinimizationConstraints)(_.id)

    def treatment = foreignKey("MinimizationConstraintTreatmentFK_Treatment", treatmentId, schema.TreatmentArms)(_.id)
  }


  object MinimizationTrialSites extends Table[(Int, Int, Int)]("MinimizationTrialSites") {
    def id = column[Int]("id", O PrimaryKey, O AutoInc)

    def minimizationId = column[Int]("MinimizationId")

    def trialSiteId = column[Int]("TrialSiteId")

    def * = id ~ minimizationId ~ trialSiteId

    def noId = minimizationId ~ trialSiteId

    def randomizationMethod = foreignKey("MinimizationTrialSitesFK_Minimization", minimizationId, Minimizations)(_.id)

    def trialSite = foreignKey("MinimizationFK_TrialSite", trialSiteId, schema.TrialSites)(_.id)
  }


  object MinimizationSiteTreatments extends Table[(Int, Int, Int, Double)]("MinimizationSiteTreatmentCounts") {
    def id = column[Int]("id", O PrimaryKey, O AutoInc)

    def minimizationTrialSiteId = column[Int]("MinimizationTrialSiteId")

    def treatmentId = column[Int]("TreatmentId")

    def count = column[Double]("Count")

    def * = id ~ minimizationTrialSiteId ~ treatmentId ~ count

    def noId = minimizationTrialSiteId ~ treatmentId ~ count

    def minimizationConstraint = foreignKey("MinimizationSiteTreatmentFK_Constraint", minimizationTrialSiteId, MinimizationTrialSites)(_.id)

    def treatment = foreignKey("MinimizationSiteTreatmentFK_Treatment", treatmentId, schema.TrialSites)(_.id)
  }


  def getDatabaseTables: DDL = {
    null
  }
}
