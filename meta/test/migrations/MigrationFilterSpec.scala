package migrations

//import com.galacticfog.gestalt.meta.api.errors.ConflictException
//import com.galacticfog.gestalt.meta.test.MetaRepositoryOps
import com.galacticfog.gestalt.meta.api.errors._
import play.api.test.PlaySpecification

class MigrationFilterSpec extends PlaySpecification /* with MetaRepositoryOps */{
  
  //def sortMigrations(migrations: Seq[String]): Try[Seq[String]]
  val filter = new MigrationFilter(DefaultVersionReader)
  
  "sortMigrations" should {
    
    "sort an unsorted Seq of migration versions" >> {
      val unsorted = Seq("V10", "V1", "V25", "V3")
      
      val sorted = filter.sortMigrations(unsorted)
      
      sorted must beSuccessfulTry
      sorted.get.size === 4
      sorted.get(0) === "V1" 
      sorted.get(1) === "V3"
      sorted.get(2) === "V10"
      sorted.get(3) === "V25"
    }
    
    "leave a sorted Seq of versions unchaged" >> {
      val alreadySorted = Seq("V3", "V6", "V9")
      alreadySorted(0) === "V3"
      alreadySorted(1) === "V6"
      alreadySorted(2) === "V9"
      
      val stillSorted = filter.sortMigrations(alreadySorted)
      stillSorted must beSuccessfulTry
      
      stillSorted.get(0) === "V3"
      stillSorted.get(1) === "V6"
      stillSorted.get(2) === "V9"      
    }
  }
  
  "maxVersion" should {
    "return the highest migration version in a given Seq" >> {
      val migrations = Seq("V4", "V2", "V3", "V5", "V1")
      val max = filter.maxVersion(migrations)
      max must beSuccessfulTry.withValue("V5")
    }
  }
  
  "minVersion" should {
    "return the lowest migration version in a given Seq" >> {
      val migrations = Seq("V4", "V2", "V3", "V5", "V1")
      val min = filter.minVersion(migrations)
      min must beSuccessfulTry.withValue("V1")
    }
  }
  
  "range(Int, Int, _)" should {
    
    val set1 = Seq("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10")
    
    "return a range of migration versions between 'to' and 'from' from a Seq" >> {
      val slice = filter.range(3, 5, set1)
      slice must beSuccessfulTry
      
      slice.get.size === 3
      slice.get(0) === "V3"
      slice.get(1) === "V4"
      slice.get(2) === "V5"
    }
    
    "throw an Exception if 'to' and 'from' overlap" >> {
      filter.range(3, 1, set1) must beFailedTry.withThrowable[BadRequestException]
    }
  }
  "range(Option[String], Option[String], _)" should {
    val set1 = Seq("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10")
    
    "return a range of migration versions between 'to' and 'from' from a Seq" >> {
      val slice = filter.range(Some("V3"), Some("V5"), set1)
      slice must beSuccessfulTry
      
      slice.get.size === 3
      slice.get(0) === "V3"
      slice.get(1) === "V4"
      slice.get(2) === "V5"
    }
    
    "begin at lowest available migration when 'from' is None" >> {
      val slice = filter.range(None, Some("V4"), set1)
      slice must beSuccessfulTry
      
      slice.get.size === 4
      slice.get(0) === "V1"
      slice.get(1) === "V2"
      slice.get(2) === "V3"
      slice.get(3) === "V4"
    }
    
    "end at the highest available migration when 'to' is None" >> {
      val slice = filter.range(Some("V7"), None, set1)
      slice must beSuccessfulTry
      
      slice.get.size === 4
      slice.get(0) === "V7"
      slice.get(1) === "V8"
      slice.get(2) === "V9"
      slice.get(3) === "V10"
    }
    
    "end at the highest available migration when 'to' is out of range" >> {
      val slice = filter.range(Some("V1"), Some("V25"), set1)
      slice must beSuccessfulTry
      slice.get.size === 10
      slice.get.last === "V10"
    }
    
    "begin at lowest available migration when 'from' is out of range" >> {
      val slice = filter.range(Some("V0"), Some("V5"), set1)
      slice must beSuccessfulTry
      slice.get.size === 5
      slice.get.head === "V1"
    }    
  }
  
/*
  "filterEffectiveMigrations" should {
    failure
  }
*/
}