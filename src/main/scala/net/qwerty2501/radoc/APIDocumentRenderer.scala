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
          <style>
            {
              """
                |/*!
                | * Start Bootstrap - Simple Sidebar (https://startbootstrap.com/template-overviews/simple-sidebar)
                | * Copyright 2013-2017 Start Bootstrap
                | * Licensed under MIT (https://github.com/BlackrockDigital/startbootstrap-simple-sidebar/blob/master/LICENSE)
                | */
                |
                |body {
                |  overflow-x: hidden;
                |}
                |
                |#wrapper {
                |  padding-left: 0;
                |  -webkit-transition: all 0.5s ease;
                |  -moz-transition: all 0.5s ease;
                |  -o-transition: all 0.5s ease;
                |  transition: all 0.5s ease;
                |}
                |
                |#wrapper.toggled {
                |  padding-left: 250px;
                |}
                |
                |#sidebar-wrapper {
                |  z-index: 1000;
                |  position: fixed;
                |  left: 250px;
                |  width: 0;
                |  height: 100%;
                |  margin-left: -250px;
                |  overflow-y: auto;
                |  background: #000;
                |  -webkit-transition: all 0.5s ease;
                |  -moz-transition: all 0.5s ease;
                |  -o-transition: all 0.5s ease;
                |  transition: all 0.5s ease;
                |}
                |
                |#wrapper.toggled #sidebar-wrapper {
                |  width: 250px;
                |}
                |
                |#page-content-wrapper {
                |  width: 100%;
                |  position: absolute;
                |  padding: 15px;
                |}
                |
                |#wrapper.toggled #page-content-wrapper {
                |  position: absolute;
                |  margin-right: -250px;
                |}
                |
                |
                |/* Sidebar Styles */
                |
                |.sidebar-nav {
                |  position: absolute;
                |  top: 0;
                |  width: 250px;
                |  margin: 0;
                |  padding: 0;
                |  list-style: none;
                |}
                |
                |.sidebar-nav li {
                |  text-indent: 20px;
                |  line-height: 40px;
                |}
                |
                |.sidebar-nav li a {
                |  display: block;
                |}
                |
                |.sidebar-nav li a:hover {
                |  text-decoration: none;
                |  color: #fff;
                |  background: rgba(255, 255, 255, 0.2);
                |}
                |
                |.sidebar-nav li a:active, .sidebar-nav li a:focus {
                |  text-decoration: none;
                |}
                |
                |.sidebar-nav>.sidebar-brand {
                |  height: 65px;
                |  font-size: 18px;
                |  line-height: 60px;
                |}
                |
                |.sidebar-nav>.sidebar-brand a {
                |  color: #999999;
                |}
                |
                |.sidebar-nav>.sidebar-brand a:hover {
                |  color: #fff;
                |  background: none;
                |}
                |
                |@media(min-width:768px) {
                |  #wrapper {
                |    padding-left: 0;
                |  }
                |  #wrapper.toggled {
                |    padding-left: 250px;
                |  }
                |  #sidebar-wrapper {
                |    width: 0;
                |  }
                |  #wrapper.toggled #sidebar-wrapper {
                |    width: 250px;
                |  }
                |  #page-content-wrapper {
                |    padding: 20px;
                |    position: relative;
                |  }
                |  #wrapper.toggled #page-content-wrapper {
                |    position: relative;
                |    margin-right: 0;
                |  }
                |}
                |
                |
              """.stripMargin
            }
          </style>
        </head>

        <body>
          <nav class="navbar navbar-dark bg-dark">


          <button class="navbar-toggler float-sm-left " id="menu-toggle">
            <span class="navbar-toggler-icon "></span>
          </button>

          <a class="navbar-brand mx-auto" href="#">
            {rootAPIDocument.title}
          </a>{if (rootAPIDocument.documents.size > 1) {
            <select class="span2 navbar-btn float-sm-right">
              {rootAPIDocument.documents.map { doc =>
              <option value={doc._1.toString}>
                {doc._1.toString}
              </option>
            }}

            </select>

          }}

        </nav>

        {
          if (rootAPIDocument.documents.size == 1) {
            renderRootAPIDocumentWithVersion(rootAPIDocument.documents.head._2, context)
          } else if (rootAPIDocument.documents.size > 1) {

          }
        }





          <script src="https://code.jquery.com/jquery-3.1.1.slim.min.js" integrity="sha384-A7FZj7v+d/sdmMqp/nOQwliLvUsJfDHW+k9Omg/a/EheAdgtzNs3hpfag6Ed950n" crossorigin="anonymous"></script>
          <script src="https://cdnjs.cloudflare.com/ajax/libs/tether/1.4.0/js/tether.min.js" integrity="sha384-DztdAPBWPRXSA/3eYEEUWrWCy7G5KFbe8fFjk5JAIxUYHKkDx6Qin1DkWx51bBrb" crossorigin="anonymous"></script>
          <script src="https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.3/js/bootstrap.min.js" integrity="sha384-a5N7Y/aK3qNeh15eJKGWxsqtnX/wWdSZSKp+81YjTmS15nvnvxKHuzaWwXHDli+4" crossorigin="anonymous"></script>
          <script src="https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.3/js/bootstrap.bundle.min.js" integrity="sha384-VspmFJ2uqRrKr3en+IG0cIq1Cl/v/PHneDw6SQZYgrcr8ZZmZoQ3zhuGfMnSR/F2" crossorigin="anonymous"></script>
          <script type="text/javascript">
            {
              """
                |function renderContent(targetId,contentId){
                | var target = document.getElementById(targetId);
                | var content = document.importNode(document.getElementById(contentId).content,true);
                | target.textContent = null;
                | target.appendChild(content);
                |}
              """.stripMargin
            }
          </script>
          <script type="text/javascript">
            {
              """
                |
                |$('#menu-toggle').click(function(e) {
                |  e.preventDefault();
                | $('#wrapper').toggleClass('toggled');
                |});
                |
              """.stripMargin
            }
          </script>
        </body>
    </html>
  }

  def renderRootAPIDocumentWithVersion(
      rootAPIDocumentWithVersion: RootAPIDocumentWithVersion,
      context: APIDocumentRendererContext): Elem = {

    val apiCategories = rootAPIDocumentWithVersion.apiCategories
    val mainContentId = "main-content-" + rootAPIDocumentWithVersion.version.toString.hashCode

    def generateTemplateId(categoryId: String, groupId: String) =
      (categoryId + groupId).hashCode.toString
    def renderGroupHeaders(groups: Seq[String],
                           categoryId: String,
                           targetId: String): Seq[Elem] = {
      groups.map { group =>
        <li class="nav-item" onclick={"renderContent(\"" + targetId + "\",\"" + generateTemplateId(categoryId,group) +  "\")"} ><a href="javascript:void(0)" class="nav-link" ><span >{group}</span></a></li>
      }
    }

    {
      <div id="wrapper" class="toggled" >

        <!-- Sidebar -->
        <div id="sidebar-wrapper">
          <ul class="sidebar-nav">

            {if (apiCategories.exists(_._1 == "")) {
            renderGroupHeaders(apiCategories.head._2.apiDocumentGroups.keys.toSeq,apiCategories.head._1, mainContentId)
          }}
          </ul>
          {
          apiCategories.filter(_._1 != "").map{tAPICategory=>
            <p>{tAPICategory._1}</p>
              <ul class="sidebar-nav">
                renderGroupHeaders(tAPICategory._2.apiDocumentGroups.keys.toSeq,tAPICategory._1,mainContentId)
              </ul>
          }
          }


        </div>

        <div id="page-content-wrapper">
          <div class="container-fluid">
            <main >
              <div id={mainContentId} />
            </main>

            {
            apiCategories.map{apiCategory=>
              apiCategory._2.apiDocumentGroups.map{apiDocumentGroup=>
                <template id={generateTemplateId(apiCategory._2.category,apiDocumentGroup._2.group)} >
                      {renderAPIGroupDocument(apiDocumentGroup._2)}
                </template>
              }
            }
            }

          </div>
        </div>

      </div>

    }

  }

  def renderAPIGroupDocument(apiDocumentGroup: APIDocumentGroup): Elem = {
    <div>
      <h1>{apiDocumentGroup.group}</h1>
      {apiDocumentGroup.apiDocuments.map { apiDocument =>
        renderAPIDocument(apiDocument._2)
      }
    }
    </div>
  }

  def renderAPIDocument(apiDocument: APIDocument): Elem = {
    <div>
      <h2><p>{apiDocument.method}</p><p>{apiDocument.path.displayPath}</p></h2>
      <div>
        {apiDocument.description}
      </div>
      {apiDocument.messageDocuments.map(renderMessageDocument)}
    </div>
  }

  def renderMessageDocument(messageDocument: MessageDocument): Elem = {

    <div>
      <div>{messageDocument.messageName}</div>

      <div>{renderMessage(messageDocument.request)}</div>

      <div>{renderMessage(messageDocument.response)}</div>
    </div>

  }

  def renderMessage(message: Message): Elem = {
    <div>{renderContent(message.content)}</div>
  }

  def renderContent(content: Content): Elem = {
    <div>{content.toString}</div>
  }
}
