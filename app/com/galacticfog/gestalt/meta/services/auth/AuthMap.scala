package com.galacticfog.gestalt.meta.services.auth

  class AuthMap[V](private val authHstore: Map[String, V])(f: => V => List[V]) {

    val auth = authHstore collect { case (k, v) => (k, f(v)) }
    val identities = auth.keys map { striptype(_) }
    val (groups, users) = auth.partition( _._1.startsWith("group:") )
    
    
    def getPermissions(identity: String): List[V] = {
      if (groups.contains(gid(identity))) groups(gid(identity))
      else if (users.contains(uid(identity))) users(uid(identity))
      else List()
    }
    
    def getPermissions(identities: Set[String]): Set[V] = for {
      id <- identities
      ps <- getPermissions( id )
    } yield ps
    
    def hasPermission(identities: Set[String], permission: V): Boolean = {
      getPermissions( identities ).contains(permission)
    }
    
    def hasPermission(identity: String, permission: V) = {
      getPermissions(identity).contains(permission)
    }
    
    private def striptype(k: String)  = k.drop(k.indexOf(":") + 1)
    private def stripident(k: String) = k.takeRight(k.length - k.indexOf(":") - 1)
    private def gid(id: String) = "group:" + id
    private def uid(id: String) = "user:" + id
  }