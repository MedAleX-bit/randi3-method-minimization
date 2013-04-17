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


  def getDatabaseTables: DDL = {
    null
  }
}
