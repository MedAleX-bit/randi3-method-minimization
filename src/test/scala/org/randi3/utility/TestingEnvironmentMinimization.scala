package org.randi3.utility


import org.randi3.dao._
import org.randi3.schema.{MinimizationSchema, LiquibaseUtil}



object TestingEnvironmentMinimization extends TestingEnvironment {

  val schemaUrn = new MinimizationSchema(driver)

  LiquibaseUtil.updateDatabase(database, "db/db.changelog-master-minimization.xml", this.getClass.getClassLoader)

  lazy val minimizationDao = new MinimizationDao(database, driver)
}