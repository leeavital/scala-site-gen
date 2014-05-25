import scala.language.reflectiveCalls

object Main extends DSLSupport{

  def main (args : Array[String] ) = {

    {e : MDFile => "post/" + e.slug} <<=
      posts from "posts"
//      templateWith "site.hbs"


    {a : AssetFile => "static/" + a.name} <<=
      assets from "static"



  }


}
