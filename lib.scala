import scala.collection.JavaConverters._
import java.io.{File => JFile, PrintStream}
import scala.io.Source
// import org.fusesource.scalate._

import scala.language.reflectiveCalls



abstract class ContentFile
case class MDFile( n : Integer, name : String, slug : String, contents : String  ) extends ContentFile
case class AssetFile( original : JFile, name : String ) extends  ContentFile


trait DSLSupport {


  abstract class PageGenerator[T <: ContentFile]{
    def pages : List[T]

    def write(x : T, route : String) : Unit
  }


  trait templatableGenerator {
    var template : Option[String]= None
    def templateWith( t : String ) = {
      template =  Some(t)
      this
    }

  }


  val posts = new PageGenerator[MDFile] {
    def pages ()  = List()

    def write(x : MDFile, route : String ) = ()

    def from ( s : String )  =  {
        val files : List[java.io.File] = (new java.io.File(s)).listFiles.toList
        val asMDFile : List[MDFile] = files.flatMap ( Lib.fileToMDFile )
        val sortedMDFiles = asMDFile.sortBy( _.n ).reverse

        new PageGenerator[MDFile] {
            def pages = sortedMDFiles
            def write (x : MDFile, route : String ) = {
              Lib.writeMDFile( pages, x, route ) // TODO FIXIT
            }
        }
    }
  }


  val assets = new Object {
    def from( path : String ) = {
      val staticPages  : List[AssetFile] = (new JFile(path)).listFiles.toList
        .map( Lib.fileToAssetFile)


      new PageGenerator[AssetFile] {
        def pages = staticPages
        def write (  file : AssetFile, route: String ) : Unit = {
            Lib.writeAssetFile( file, route )
        }
      }

    }
  }

  implicit class RouteResolver[T <: ContentFile]( path_tpl : T => String) {
    def <<= ( gen : PageGenerator[T] ) = {
      val directory = gen.pages
      for( p <- gen.pages ){
        gen.write( p,  path_tpl( p ))
      }
    }
  }


}

object Lib{

  // set up a template
  val loader = new com.github.jknack.handlebars.io.FileTemplateLoader( new java.io.File("."), ".hbs" );
  val handlebars = new com.github.jknack.handlebars.Handlebars( loader )
  val templ = handlebars.compile( "site" )  // site.handlebars

  // def main (args : Array[String] ) = {
  //   val files : List[java.io.File] = (new java.io.File("posts")).listFiles.toList

  //   val asMDFile : List[MDFile] = files.flatMap ( fileToMDFile )


  //   val sortedMDFiles = asMDFile.sortBy( _.n ).reverse

  //   println( sortedMDFiles )

  //   asMDFile foreach (writeMDFile (sortedMDFiles , _))
  // }


  def readFile (f : java.io.File) : String = {
    try{
      ( Source.fromFile (f) ).mkString
    }catch{
      case (e : Exception) => println( "could not read" ); ""
    }
  }


  def writeMDFile (listing : List[MDFile],  file : MDFile, dir : String ) = {
    val MDFile(_, title, slug, html ) = file
    val toc = listing.map( {x => Map("post" -> x.name, "slug" -> x.slug).asJava } ).asJava

    val tplData = Map( "title" -> title, "body" -> html, "toc" -> toc )

    val fullHTML = templ( tplData.asJava )

    val _ = (new JFile( "output/" + dir  )).mkdirs()
    val ps = new PrintStream( new JFile( "output/" + dir + "/index.html" ) )
    ps.print( fullHTML )
  }



  def writeAssetFile(file : AssetFile, path : String ) = {

      // make sure the prefix exists
      val opath = "output/" + path
      val rxp = """(.*)/[^\/]+""".r
      opath match {
        case rxp( prefix ) => new JFile( prefix ).mkdirs()
        case _ => ()
      }

      // create an output stream for the destination
      val ostream = new PrintStream( new JFile( opath ) )

      // get the path to the original
      val spath = java.nio.file.FileSystems.getDefault().getPath(".", file.original.getPath)


      java.nio.file.Files.copy( spath, ostream )

  }


  def fileToMDFile ( file : JFile ) = {
      val md = new org.markdown4j.Markdown4jProcessor

      val matcher = """(\d+)\.(.*)\.md""".r
      file.getName match {
        case matcher( n, name ) =>
          val postnum = Integer.parseInt( n )
          val html = md.process ( readFile ( file ) )
          val slug = name.toLowerCase.replace( " ", "-" )
          Some( MDFile(postnum, name, slug, html  ) )
        case _ =>
          None
      }
  }


  def fileToAssetFile( f : JFile ) = {
    val name = f.getName
    AssetFile( f, name ) 
  }
}
