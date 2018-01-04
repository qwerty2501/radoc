package net.qwerty2501.radoc
import java.io._

import scala.xml._
import scala.io.Source
object APIDocumentRenderer {

  def renderTo(rootAPIDocument: RootAPIDocument, outputPath: String): Unit =
    renderTo(rootAPIDocument, outputPath, APIDocumentRendererContext())
  def renderTo(rootAPIDocument: RootAPIDocument,
               outputPath: String,
               context: APIDocumentRendererContext): Unit = {
    APIDocumentRendererInternal.outputDocument(
      APIDocumentRendererInternal.render(rootAPIDocument, context),
      outputPath)
  }

}

private object APIDocumentRendererInternal {
  def outputDocument(document: String, outputPath: String): Unit = {
    val printWriter = new PrintWriter(new File(outputPath))
    printWriter.write(document)
    printWriter.close()

  }

  def render(rootAPIDocument: RootAPIDocument,
             context: APIDocumentRendererContext): String = {
    "<!DOCTYPE html>\n" +
      new PrettyPrinter(80, 2)
        .format(renderRootAPIDocument(rootAPIDocument, context))

  }

  def getResourceText(path: String): String = {
    Source.fromResource(path, this.getClass.getClassLoader).mkString
  }

  def renderRootAPIDocument(rootAPIDocument: RootAPIDocument,
                            context: APIDocumentRendererContext): Elem = {
    <html>
        <head>
          <meta charset="UTF-8"/>
          <title>{rootAPIDocument.title}</title>
          <link href="https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.3/css/bootstrap.min.css" rel="stylesheet" integrity="sha384-Zug+QiDoJOrZ5t4lssLdxGhVrurbmBWopoEl+M6BdEfwnCJZtKxi1KgxUyJq13dy" crossorigin="anonymous"/>
        </head>
        <body>
          <nav class="navbar navbar-light bg-primary">
            <a class="navbar-brand" href="#">{rootAPIDocument.title}</a>
            {
              if (rootAPIDocument.documents.size > 1){
                <button class="navbar-toggler navbar-toggler-right" type="button" data-toggle="collapse" data-target="#navbarNavDropdown" aria-controls="navbarNavDropdown" aria-expanded="false" aria-label="Toggle navigation">
                  <span class="navbar-toggler-icon"></span>
                </button>

                  <div class="collapse navbar-collapse" id="navbarNavDropdown">

                    <ul class="navbar-nav">
                      {
                        rootAPIDocument.documents.map{doc=>
                          <li class="nav-item">
                            <a class="nav-link" href="#">{doc._1.toString}</a>
                          </li>
                        }
                      }
                    </ul>
                  </div>
              }
            }
          </nav>
          <!--
          {
            if (rootAPIDocument.documents.size == 1) {
              renderRootAPIDocumentWithVersion(rootAPIDocument.documents.head._2,context)
            } else if (rootAPIDocument.documents.size > 1) {

            }
          }
          -->

          <script src="https://code.jquery.com/jquery-3.1.1.slim.min.js" integrity="sha384-A7FZj7v+d/sdmMqp/nOQwliLvUsJfDHW+k9Omg/a/EheAdgtzNs3hpfag6Ed950n" crossorigin="anonymous"></script>
          <script src="https://cdnjs.cloudflare.com/ajax/libs/tether/1.4.0/js/tether.min.js" integrity="sha384-DztdAPBWPRXSA/3eYEEUWrWCy7G5KFbe8fFjk5JAIxUYHKkDx6Qin1DkWx51bBrb" crossorigin="anonymous"></script>
          <script src="https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.3/js/bootstrap.min.js" integrity="sha384-a5N7Y/aK3qNeh15eJKGWxsqtnX/wWdSZSKp+81YjTmS15nvnvxKHuzaWwXHDli+4" crossorigin="anonymous"></script>
          <script src="https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.3/js/bootstrap.bundle.min.js" integrity="sha384-VspmFJ2uqRrKr3en+IG0cIq1Cl/v/PHneDw6SQZYgrcr8ZZmZoQ3zhuGfMnSR/F2" crossorigin="anonymous"></script>
        </body>
    </html>
  }

  def renderRootAPIDocumentWithVersion(
      rootAPIDocumentWithVersion: RootAPIDocumentWithVersion,
      context: APIDocumentRendererContext): Elem = {

    val apiCategories = rootAPIDocumentWithVersion.apiCategories
    val categories = apiCategories.keys

    def renderGroupHeaders(groups: Seq[String]): Seq[Elem] = {
      groups.map { group =>
        <li class="nav-item"><a href="" class="nav-link" ><span >{group}</span></a></li>

      }
    }

    <div class="container-fluid">
      <div class="row">
        <nav class="col-sm-3 col-md-2 hidden-xs-down bg-secondary sidebar">
          <ul class="nav nav-pills flex-column">

            {if (apiCategories.exists(_._1 == "")) {
            renderGroupHeaders(apiCategories.head._2.apiDocumentGroups.keys.toSeq)
          }}
          </ul>
            {
            apiCategories.filter(_._1 != "").map{tAPICategory=>
              <p>{tAPICategory._1}</p>
                <ul class="nav nav-pills flex-column">
              renderGroupHeaders(tAPICategory._2.apiDocumentGroups.keys.toSeq)
                  </ul>
            }
            }


        </nav>
        <main class="col-sm-9 offset-sm-3 col-md-10 offset-md-2 pt-3">
        </main>
      </div>

    </div>

  }
}
