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

import net.liftweb.mapper._
import net.liftweb.sitemap._

import net.liftweb.http.S
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.sitemap.Loc.Hidden

class User extends MegaProtoUser[User] {
  def getSingleton = User
}

object User extends User with MetaMegaProtoUser[User] with LDAPUser {

  import SiteMap._

  /**
  * Setting enforceUniqueLinks to false will let use set multiple links.
  */
  enforceUniqueLinks = false

  override def menus: List[Menu] = sitemap

  /**
   * Setting the User Menu that only Login/Logout is viewed.
   */
  override lazy val sitemap: List[Menu] = List(loginMenuLoc, logoutMenuLoc).flatten(a => a)

  override def loginXhtml = {
    (<lift:surround with="default" at ="content">
      <h3>{"Login nur mit gültiger FHS-ID möglich!"}</h3>
      <form method="post" action={S.uri} name="login">
      <table>
        <tr><td colspan="2">{S.??("log.in")}</td></tr>
        <tr><td>{S.??("FHS-ID")}</td><td><user:user /></td></tr>
        <tr><td>{S.??("password")}</td><td><user:password /></td></tr>
        <tr><td><user:submit /></td></tr>
      </table>
      </form>
      <script type="text/javascript" language="JavaScript">
        document.forms['login'].elements['username'].focus();
      </script>
     </lift:surround>)
  }

  /**
   * Overriding login here is necessary because we need to Auth against fHS LDAP.
   * @todo Is this a proper solution?
   */
  override def login = {
    if (S.post_?) {
      if (S.param("username").open_!.equals("") || S.param("password").open_!.equals("")) {
        S error "Error: Bitte User und Pass angeben"
        S redirectTo "/user_mgt/login"
      }
      if (login2ldap(S.param("username").open_!,S.param("password").open_!)) {
        User.logUserIdIn(S.param("username").open_!)
        User.fillAttributes
        S notice "Login Successful as " + User.currentUserId.open_!
        S redirectTo "/index"
      } else { }
    }

    bind("user", loginXhtml,
      "user" -> ((<input type="text" name="username"/>)),
      "password" -> (<input type="password" name="password"/>),
      "submit" -> (<input type="submit" value={S.??("log.in")}/>))
  }
}
