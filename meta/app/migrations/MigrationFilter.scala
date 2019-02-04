package migrations

import com.galacticfog.gestalt.meta.api.errors._
import scala.util.{Failure,Success,Try}
import controllers.util.QueryString

class MigrationFilter(vr: VersionReader[String,String]) {
  
  def filterEffectiveMigrations(qs: Map[String,Seq[String]], migrations: Seq[String]) = {

    if (qs.isEmpty) 
      Success(migrations)
    else {
      val skiplist = QueryString.list(qs, "skip")
      val versions = QueryString.list(qs, "version")
      val from = QueryString.single(qs, "from")
      val to = QueryString.single(qs, "to")
  
      // Confine to range if params given
      val baseMigrations: Seq[String] = {
        if (from.nonEmpty || to.nonEmpty) 
          range(from, to, migrations).get
        else if (versions.nonEmpty)
          Seq.empty
        else migrations
      }
      
      // Add any explicit versions requested
      val additions = migrations.filter(versions.contains(_))

      // Skip any versions requested
      val pruned = (baseMigrations ++ additions).toSet.filter(!skiplist.contains(_)).toList
      
      sortMigrations(pruned)
    }
  }
  
  def sortMigrations(migrations: Seq[String]): Try[Seq[String]] = Try {
    migrations.sortBy { n => 
      vr.versionNumber(n).fold {
        throw new BadRequestException(s"Bad migration name. found: '${n}', expected: 'V[0-9]+'")
      }{ v => v.toInt }
    }
  }
  
  def maxVersion(migrations: Seq[String]): Try[String] = {
    sortMigrations(migrations).map { m =>
      m.lastOption.getOrElse {
        throw new RuntimeException(s"The list of migrations is empty. No MAX.")
      }
    }
  }
  
  def minVersion(migrations: Seq[String]): Try[String] = {
    sortMigrations(migrations).map { m =>
      m.headOption.getOrElse {
        throw new RuntimeException(s"The list of migrations is empty. No MIN.")
      }
    }
  }    
  
  def range(from: Option[String], to: Option[String], migrations: Seq[String]): Try[Seq[String]] = {
    def minMax(v: Option[String], migrations: Seq[String], f: Seq[String] => Try[String]) = {
      v.fold {
        vr.versionNumber(f(migrations).get).get.toInt
      }{ n =>
        vr.versionNumber(n).getOrElse {
          throw new BadRequestException(s"Bad range value. found: '${n}', expected: 'V[0-9]+'")
        }.toInt    
      }
    }
    val nf = minMax(from, migrations, minVersion)
    val nt = minMax(to, migrations, maxVersion)
    
    range(nf, nt, migrations)
  }
  
  def range(from: Int, to: Int, migrations: Seq[String]): Try[Seq[String]] = {
    if (to < from) 
      Failure(new BadRequestException(s"Bad range values. 'to' must be >= 'from'."))
    else {
      sortMigrations(migrations).map { m =>
        m.filter { n =>
          vr.versionNumber(n) match {
            case None => false
            case Some(i) => ((i.toInt >= from) && (i.toInt <= to))
          }
        }
      }
    }
  }
  
}
