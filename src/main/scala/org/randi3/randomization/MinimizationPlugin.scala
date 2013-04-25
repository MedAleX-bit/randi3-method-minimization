package org.randi3.randomization

import org.randi3.randomization.configuration._
import org.randi3.dao.MinimizationDao
import org.randi3.model._
import org.randi3.model.criterion.Criterion
import org.randi3.model.criterion.constraint.Constraint
import org.scalaquery.ql._
import org.scalaquery.ql.extended.ExtendedProfile
import org.scalaquery.session.Database
import scalaz._

import org.apache.commons.math3.random._

import org.randi3.schema.{LiquibaseUtil, MinimizationSchema}
import org.randi3.utility.{I18NHelper, I18NRandomization, AbstractSecurityUtil}

class MinimizationPlugin(database: Database, driver: ExtendedProfile, securityUtil: AbstractSecurityUtil) extends RandomizationMethodPlugin(database, driver, securityUtil) {

  private val i18n = new I18NRandomization(I18NHelper.getLocalizationMap("minimizationM", getClass.getClassLoader), securityUtil)

  val schema = new MinimizationSchema(driver)
  import schema._


  val name = classOf[Minimization].getName

  def i18nName = i18n.text("name")

  def description = i18n.text("description")

  val canBeUsedWithStratification = true

  private val minimizationDao = new MinimizationDao(database, driver)

  private def pConfigurationType = new DoubleConfigurationType(name = i18n.text("p"), description =  i18n.text("pDesc"))


  def randomizationConfigurationOptions(): (List[ConfigurationType[Any]], Map[String,List[Criterion[_ <: Any, Constraint[_ <: Any]]]]) = {
    (List(pConfigurationType), Map())
  }

  def getRandomizationConfigurations(id: Int): List[ConfigurationProperty[Any]] = {
    val method = minimizationDao.get(id).toOption.getOrElse(return Nil).getOrElse(return Nil)
    List(new ConfigurationProperty[Any](pConfigurationType, method.p)
    )
  }

  def randomizationMethod(random: RandomGenerator, trial: Trial, configuration: List[ConfigurationProperty[Any]]): Validation[String, RandomizationMethod] = {
    if (configuration.isEmpty) Failure(i18n.text("noConfAvailable"))
    else {
      val seedEqualScore = (new MersenneTwister()).nextLong()

      Success(new Minimization(p = configuration(0).value.asInstanceOf[Double],
      seedRandomEqualScore =  seedEqualScore)
      (random = random,
       randomEqualScore = new MersenneTwister(seedEqualScore)))
    }
  }

  def databaseTables(): Option[DDL] = {
    Some(getDatabaseTables)
  }

  def updateDatabase() {
    LiquibaseUtil.updateDatabase(database, "db/db.changelog-master-minimization.xml", this.getClass.getClassLoader)
  }

  def create(randomizationMethod: RandomizationMethod, trialId: Int): Validation[String, Int] = {
    minimizationDao.create(randomizationMethod.asInstanceOf[Minimization], trialId)
  }

  def get(id: Int): Validation[String, Option[RandomizationMethod]] = {
    minimizationDao.get(id)
  }

  def getFromTrialId(trialId: Int): Validation[String, Option[RandomizationMethod]] = {
    minimizationDao.getFromTrialId(trialId)
  }

  def update(randomizationMethod: RandomizationMethod): Validation[String, RandomizationMethod] = {
    minimizationDao.update(randomizationMethod.asInstanceOf[Minimization])
  }

  def delete(randomizationMethod: RandomizationMethod) {
    minimizationDao.delete(randomizationMethod.asInstanceOf[Minimization])
  }

}
