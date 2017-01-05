/*
* Copyright 2007-2010 WorldWide Conferencing, LLC
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing,
* software distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions
* and limitations under the License.
*/

package de.codecarving
package fhsldap
package model

import javax.naming._
import javax.naming.directory._
import java.util._
import scala.collection.JavaConversions._
import scala.Option._
import net.liftweb.common.{Empty, Box, Loggable, Full}
import net.liftweb.util.Props
import net.liftweb.http.{S, SessionVar, RequestVar}

trait LDAPUser extends Loggable {

  private val STUDENT = 1002
  private val EMPLOYEE = 1001

  /**
   * ldapAttributes holds specific LDAP Attributes found on zefi and ldap1
   */
  object ldapAttributes {
    object gidNumber extends SessionVar[Box[Int]](Empty)
    object email extends SessionVar[Box[String]](Empty)
    object displayName extends SessionVar[Box[String]](Empty)
    object cn extends SessionVar[Box[String]](Empty)
    object stg extends SessionVar[Box[String]](Empty)
  }

  import ldapAttributes._

  /**
   * @return gidNumber.
   * */
  def getRole: Box[Int] = {
    gidNumber.or(Box(0))
  }

  /**
   * Wether User is a student or not.
   */
  def isStudent: Boolean = {
    gidNumber.openOr(0) == STUDENT
  }

  /**
   * Wether User is an employee or not.
   */
  def isEmployee: Boolean = {
    gidNumber.openOr(0) == EMPLOYEE
  }

  /**
   * Wether User is an Admin or not.
   * This is defined in the Prop file in resources/
   */
  def isAdmin: Boolean = {
    Props.get("spirit.server.admins", "")
         .split(";")
         .contains(User.currentUserId.openOr(""))
  }

  /**
   * Trying to Authenticate against the LDAP Server!
   */
  def login2ldap(fhsid: String, password: String): Boolean = {
    if(Props.get("ldap.server.auth.use", "false") == "true") {

      val authEnv = new Hashtable[String,String]
      val ldapURL = Props.get("ldap.server.zefi", "")

      System.setProperty("javax.net.ssl.trustStore", Props.get("ldap.server.truststore", "sslstore"))

      authEnv.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory")
      authEnv.put(Context.PROVIDER_URL, ldapURL)

      val dn = "uid=" + fhsid + "," + "ou=people,ou=in,dc=fh-schmalkalden,dc=de"

      try {
        authEnv.put(Context.SECURITY_AUTHENTICATION, "simple")
    	authEnv.put(Context.SECURITY_PRINCIPAL, dn)
        authEnv.put(Context.SECURITY_CREDENTIALS, password)
        authEnv.put(Context.SECURITY_PROTOCOL, "SSL")

        val ctx2: DirContext = new InitialDirContext(authEnv)
        true
      } catch {
        case a: AuthenticationException =>
          logger error a.printStackTrace.toString
          S error "Error: Bitte richtige FHS-ID und Passwort angeben"
          S redirectTo "/user_mgt/login"
          false
        case b: NamingException =>
          logger error b.printStackTrace.toString
          S error "Error: Der LDAP Server ist derzeit nicht erreichbar!"
          S redirectTo "/user_mgt/login"
          false
        case c: TimeLimitExceededException =>
          logger error c.printStackTrace.toString
          S error "Error: Der LDAP Server ist derzeit nicht erreichbar!"
          S redirectTo "/user_mgt/login"
          false
        case d =>
          logger error d.printStackTrace.toString
          S error "Error: Eine Störung liegt vor!"
          S redirectTo "/user_mgt/login"
          false
        case _ =>
          false
      }
    } else {
        true
    }
  }

  import LDAPUtils._

  /**
   * Filling up the LDAP Attributes.
   */
  def fillAttributes {
    gidNumber(Full(getAttribute("gidNumber", User.currentUserId.open_!).openOr("0").toInt))
    email(Full(getEmailfromLDAP1(User.currentUserId.open_!,gidNumber.open_!).openOr("")))
    displayName(Full(getAttribute("displayName", User.currentUserId.open_!).openOr("")))
    cn(Full(getAttribute("cn", User.currentUserId.open_!).openOr("")))
    stg(Full(getAttribute("stg", User.currentUserId.open_!).openOr("")))
  }
}

object LDAPUtils extends Loggable {

  System.setProperty("javax.net.ssl.trustStore", Props.get("ldap.server.truststore", "sslstore"))

  /**
   * @param fhsid: Student or Employee FHS-ID.
   * @param gidNumber: 1001 for employees or 1002 for students.
   * @return Box[String]: Hopefully an email from an LDAP Server, but if not a default is defined in the Prop file.
   * Using Zefi for Auth and LDAP1 to get our "pretty" email adresses.
   */
  def getEmailfromLDAP1(fhsid: String, gidNumber: Int): Box[String] = {
    try {
      if(Props.get("ldap.server.auth.use", "false") == "true") {

        val base = gidNumber match {
          case 1002 => "ou=students,dc=fh-sm,dc=de"
          case 1001 => "ou=people,dc=fh-sm,dc=de"
        }

        val dn = "uid=" + fhsid + "," + base
        val ldapURL = Props.get("ldap.server.ldap1", "")

        val emailEnv = new Hashtable[String,String]
          emailEnv.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory")
          emailEnv.put(Context.PROVIDER_URL, ldapURL)
        val ctx: DirContext = new InitialDirContext(emailEnv)
        val attrs: Attributes = ctx.getAttributes(dn)
        val ids = attrs.getIDs.toList

        def getAttrValList(id: String): List[String] =
          if (ids contains id)
            for (i <- 0 to attrs.get(id).size - 1)
              yield attrs.get(id).get(i).toString
          else
            Nil: List[String]

        val emails = getAttrValList("mail") map { _.toString } filter { email =>
          email.matches("[a-zA-Z][.].\\w.*@fh-sm.de") ||
          email.matches("[a-zA-Z][.].\\w.*@stud.fh-sm.de")
        }
        if (emails isEmpty) Empty else Full(emails.head)
      } else {
        Full(Props.get("spirit.default.email", ""))
      }
    } catch {
       case any =>
          logger error any.printStackTrace.toString
          Full("")
    }
  }

  /**
   * Getting any known attribute from Zefi.
   * @param key The Key for the Attribute to get.
   * @param fhsid From what User the Attribute is to get.
   * @return Box[String] Either the Attribute is found, an empty String or a default value from the Props file.
   */
  def getAttribute(key: String, fhsid: String): Box[String] = {

    try {
      if(Props.get("ldap.server.auth.use", "false") == "true") {
        val env = new Hashtable[String,String]
        val ldapURL = Props.get("ldap.server.zefi", "")

        env.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory")
        env.put(Context.PROVIDER_URL, ldapURL)

        val ctx: DirContext = new InitialDirContext(env)
        val dn = "uid=" + fhsid + "," + "ou=people,ou=in,dc=fh-schmalkalden,dc=de"
        val attrs: Attributes = ctx.getAttributes(dn)

        val ids = attrs.getIDs.toList
        def getAttrVal(id: String) =
          if (ids contains id) attrs.get(id).get(0).toString else ""
        Full(getAttrVal(key))
      } else {
        Full(Props.get("default." + key, ""))
      }
    } catch {
        case any =>
          logger error any.printStackTrace.toString
          Full("")
      }
  }
}
