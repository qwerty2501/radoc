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
    val doc = renderRootAPIDocument(rootAPIDocument, context)
    dtd
      .DocType("html", dtd.SystemID("about:legacy-compat"), Nil)
      .toString() + "\n" + Xhtml.toXhtml(doc)
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
          <link href="https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css" rel="stylesheet" integrity="sha384-Gn5384xqQ1aoWXA+058RXPxPg6fy4IWvTNh0E263XmFcJlSAwiGgFAW/dAiS6JXm" crossorigin="anonymous"/>
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
            renderRootAPIDocumentWithVersion(rootAPIDocument.documents.head._2,rootAPIDocument, context)
          } else if (rootAPIDocument.documents.size > 1) {

          }
        }





          <script src="https://code.jquery.com/jquery-3.1.1.slim.min.js" integrity="sha384-A7FZj7v+d/sdmMqp/nOQwliLvUsJfDHW+k9Omg/a/EheAdgtzNs3hpfag6Ed950n" crossorigin="anonymous"></script>
          <script src="https://cdnjs.cloudflare.com/ajax/libs/tether/1.4.0/js/tether.min.js" integrity="sha384-DztdAPBWPRXSA/3eYEEUWrWCy7G5KFbe8fFjk5JAIxUYHKkDx6Qin1DkWx51bBrb" crossorigin="anonymous"></script>
          <script src="https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/js/bootstrap.bundle.min.js" integrity="sha384-feJI7QwhOS+hwpX2zkaeJQjeiwlhOP+SdQDqhgvvo1DsjtiSQByFdThsxO669S2D" crossorigin="anonymous"></script>
          <script type="text/javascript">
            {
              """
                |function renderContent(mainContentId,contentId){
                | var target = document.getElementById(mainContentId);
                | var content = document.importNode(document.getElementById(contentId).content,true);
                | target.textContent = null;
                | target.appendChild(content);
                |}
              """.stripMargin
            }
            {Unparsed(
            """
              |var getUrlParameter = function getUrlParameter(sParam) {
              |    var sPageURL = decodeURIComponent(window.location.search.substring(1)),
              |        sURLVariables = sPageURL.split('&'),
              |        sParameterName,
              |        i;
              |
              |    for (i = 0; i < sURLVariables.length; i++) {
              |        sParameterName = sURLVariables[i].split('=');
              |
              |        if (sParameterName[0] === sParam) {
              |            return sParameterName[1] === undefined ? true : sParameterName[1];
              |        }
              |    }
              |};
            """.stripMargin)

            }
            {
              Unparsed(
              """
                |
                |$( document ).ready(function(){
                | var mainContentId=getUrlParameter('mainContentId');
                | var contentId=getUrlParameter('contentId');
                | if (mainContentId != null){
                |   if (contentId != null){
                |     renderContent(mainContentId,contentId);
                |   }
                | }
                |});
                |
              """.stripMargin)


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
      rootAPIDocument: RootAPIDocument,
      context: APIDocumentRendererContext): Elem = {

    val apiCategories = rootAPIDocumentWithVersion.apiCategories
    val mainContentId =
      InternalLink.mainContentId(rootAPIDocumentWithVersion.version)

    def generateTemplateId(categoryId: String, groupId: String) =
      (categoryId + groupId).hashCode.toString
    def renderGroupHeaders(groups: Seq[APIDocumentGroup],
                           categoryId: String,
                           mainContentId: String,
                           version: Version): Seq[Elem] = {
      groups.map { group =>
        <li class="nav-item"  ><a href={Link.href(group) } class="nav-link" ><span >{group.group}</span></a></li>

      }
    }

    {
      <div id="wrapper" class="toggled" >

        <!-- Sidebar -->
        <div id="sidebar-wrapper">
          <ul class="sidebar-nav">

            {if (apiCategories.exists(_._1 == "")) {
            renderGroupHeaders(apiCategories.head._2.apiDocumentGroups.values.toSeq,apiCategories.head._1, mainContentId,rootAPIDocumentWithVersion.version)
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
            <main class="bd-content container-fluid" id={mainContentId}  role="main"/>

            {
            apiCategories.map{apiCategory=>
              apiCategory._2.apiDocumentGroups.map{apiDocumentGroup=>
                <template id={ InternalLink.templateId(apiCategory._2.category,apiDocumentGroup._2.group) } >
                      {renderAPIGroupDocument(apiDocumentGroup._2,apiCategory._2,rootAPIDocumentWithVersion,rootAPIDocument,context)}
                </template>
              }
            }
            }

          </div>
        </div>

      </div>

    }

  }

  def renderAPIGroupDocument(
      apiDocumentGroup: APIDocumentGroup,
      currentCategory: APIDocumentCategory,
      currentAPIDocumentWithVersion: RootAPIDocumentWithVersion,
      rootAPIDocument: RootAPIDocument,
      context: APIDocumentRendererContext): Elem = {
    <div>
      <div>
        <h1 class="bd-title">{apiDocumentGroup.group}</h1>
      </div>
      <p>
        {
          apiDocumentGroup.apiDocuments.map { apiDocument =>
            renderAPIDocument(apiDocument._2,apiDocumentGroup,currentCategory,currentAPIDocumentWithVersion,rootAPIDocument,context)
          }
        }
      </p>

    </div>
  }

  def renderAPIDocument(
      apiDocument: APIDocument,
      currentGroup: APIDocumentGroup,
      currentCategory: APIDocumentCategory,
      currentAPIDocumentWithVersion: RootAPIDocumentWithVersion,
      rootAPIDocument: RootAPIDocument,
      context: APIDocumentRendererContext): Elem = {
    <p>
    <div >
      <h3><p>{apiDocument.method} {apiDocument.path.displayPath}</p></h3>
      <div>
        <p>
          {apiDocument.description.render(TextRenderingArguments(
          rootAPIDocument,
          currentAPIDocumentWithVersion,
          currentCategory,
          currentGroup,
          apiDocument,
          apiDocument.messageDocumentMap.head._2,
          context))}
        </p>
      </div>
      {apiDocument.messageDocumentMap.map(messageDocumentT =>
        renderMessageDocument(messageDocumentT._2,apiDocument,currentGroup,currentCategory,currentAPIDocumentWithVersion,rootAPIDocument,context))
      }
    </div>
    </p>
  }

  def renderMessageDocument(
      messageDocument: MessageDocument,
      currentAPIDocument: APIDocument,
      currentGroup: APIDocumentGroup,
      currentCategory: APIDocumentCategory,
      currentAPIDocumentWithVersion: RootAPIDocumentWithVersion,
      rootAPIDocument: RootAPIDocument,
      context: APIDocumentRendererContext): Elem = {

    def renderMessage(message: Message): Elem = {
      <div>
        <div>
          {
            renderParameters("Headers", message.headers.map{arg=>
            val name = arg._1
            val param = arg._2
            Parameter(name,param.value,param.typeName,param.description)}.toSeq)
          }
        </div>
        <div>{renderContent(message.content)}</div>
      </div>

    }

    def renderContent(content: Content): Elem = {
      <div>{content.toString}</div>
    }

    def renderParameters(title: String, parameters: Seq[Parameter]): Node = {
      if (parameters.nonEmpty) {
        <div>
          <div ><h5>{title}</h5></div>
          <table class="table table-sm table-bordered">
            <thead class="thead-inverse">
              <tr>
                <th scope="col" >name</th>
                <th scope="col" >value</th>
                <th scope="col" >type</th>
                <th scope="col" >description</th>
              </tr>
            </thead>
            {
            parameters.map { parameter =>
              <tr >
                <td scope="row">{parameter.name}</td>
                <td  >{parameter.value.toString}</td>
                <td >{parameter.typeName}</td>
                <td >{parameter.description.render(
                  TextRenderingArguments(
                    rootAPIDocument,currentAPIDocumentWithVersion,currentCategory,
                    currentGroup,currentAPIDocument,messageDocument,context
                  ))}
                </td>
              </tr>
            }
            }
          </table>

        </div>

      } else xml.Text("")

    }

    <div>
      <div><h2>{messageDocument.messageName}</h2></div>

      <p>
        <p><h3>Request</h3></p>
        <p>{messageDocument.request.path.actualPath}</p>
        <p>{renderParameters("Path parameters",messageDocument.request.path.pathParameters)}</p>
        <p>{renderParameters("Queries",messageDocument.request.path.queries)}</p>
        <div>{renderMessage(messageDocument.request)}</div>
      </p>

      <p>
        <h3>Response</h3>
        <div>{renderMessage(messageDocument.response)}</div>
      </p>

    </div>

  }

}
