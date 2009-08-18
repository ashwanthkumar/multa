// Multa template engine

package multa

import scala.io.Source
import collection.mutable.HashMap
import collection.mutable.ListBuffer
import util.matching.Regex
import util.matching.Regex.Match


// Global object cache
object H {
  val mold: HashMap[String, Mold] = new HashMap()
}

// Template builder
class Mold( file: String ){
  val f: String = Source.fromFile( file ).mkString
  val ms: CountedIterator[Match] = 
    """([^\}]*)(\#\{.*?\})([^\#]*)""".r.findAllIn( f ).matchData.counted
  val all: ListBuffer[String] = new ListBuffer()
  val spots: HashMap[String, List[Int]] = new HashMap()
  def render( dat: HashMap[String, String] ): String = {
    dat map( d => spots("#{" + d._1 + "}") map( pos => all.update( pos, d._2 ) ) )
    return all.mkString
  }
  ms foreach( m => { 
    val tag = m.group( 2 )
    if ( spots.contains( tag ) ) spots(tag) ::= 3 * ms.count + 1
    else spots += tag -> List( 3 * ms.count + 1 ) 
    all.appendAll( m.subgroups ) 
  } )
}
