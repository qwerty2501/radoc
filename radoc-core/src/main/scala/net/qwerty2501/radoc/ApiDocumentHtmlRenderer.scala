package net.qwerty2501.radoc

import java.io.{File, PrintWriter}

import io.circe.parser.parse

import scala.io.Source
import scala.xml._

private object ApiDocumentHtmlRenderer {

  private[radoc] def render(rootApiDocument: RootApiDocument,
                            context: ApiDocumentHtmlRendererContext): String = {
    val doc = renderRootApiDocument(rootApiDocument, context)
    dtd
      .DocType("html", dtd.SystemID("about:legacy-compat"), Nil)
      .toString() + "\n" + Xhtml.toXhtml(doc)
  }

  private[radoc] def getResourceText(path: String): String = {
    Source.fromResource(path, this.getClass.getClassLoader).mkString
  }

  private[radoc] def renderRootApiDocument(
      rootApiDocument: RootApiDocument,
      context: ApiDocumentHtmlRendererContext): Elem = {

    <html>
      <head>
        <meta charset="UTF-8"/>
        <title>{rootApiDocument.title}</title>
        {renderCss("bootstrap.min.css")}
        {renderCss("simple-sidebar.css")}

      </head>

      <body>

        <nav class="navbar navbar-dark bg-dark sticky-top"  >

          <button class="navbar-toggler float-sm-left " id="menu-toggle">
            <span class="navbar-toggler-icon "></span>
          </button>

          <a class="navbar-brand mx-auto" href="#">
            {rootApiDocument.title}
          </a>{if (rootApiDocument.documents.size > 1) {
          <select class="span2 navbar-btn float-sm-right">
            {rootApiDocument.documents.map { doc =>
            <option value={doc._1.toString}>
              {doc._1.toString}
            </option>
          }}

          </select>

        }}
        </nav>



        <div class="container-fluid" >
          {
          if (rootApiDocument.documents.size == 1) {
            renderRootApiDocumentWithVersion(rootApiDocument.documents.head._2,rootApiDocument, context)
          } else if (rootApiDocument.documents.size > 1) {

          }
          }
        </div>
        {renderJavaScript("jquery-3.3.1.slim.min.js")}
        {renderJavaScript("bootstrap.bundle.min.js")}

        {renderJavaScript("ui.js")}
      </body>
    </html>
  }

  private[radoc] def renderRootApiDocumentWithVersion(
      rootApiDocumentWithVersion: RootApiDocumentWithVersion,
      rootApiDocument: RootApiDocument,
      context: ApiDocumentHtmlRendererContext): Elem = {

    val apiCategories = rootApiDocumentWithVersion.apiCategories
    val mainContentId =
      Link.mainContentId(rootApiDocumentWithVersion.version)

    def generateTemplateId(categoryId: String, groupId: String) =
      (categoryId + groupId).hashCode.toString
    def renderGroupHeaders(groups: Seq[ApiDocumentGroup],
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
            renderGroupHeaders(apiCategories.head._2.apiDocumentGroups.values.toSeq,apiCategories.head._1, mainContentId,rootApiDocumentWithVersion.version)
          }}
          </ul>
          {
          apiCategories.filter(_._1 != "").map{tApiCategory=>
            <p>{tApiCategory._1}</p>
              <ul class="sidebar-nav">
                renderGroupHeaders(tApiCategory._2.apiDocumentGroups.keys.toSeq,tApiCategory._1,mainContentId)
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
                <template id={ Link.templateId(apiCategory._2.category,apiDocumentGroup._2.group) } >
                  {renderApiGroupDocument(apiDocumentGroup._2,apiCategory._2,rootApiDocumentWithVersion,rootApiDocument,context)}
                </template>
              }
            }
            }

          </div>
        </div>

      </div>

    }

  }

  private[radoc] def renderApiGroupDocument(
      apiDocumentGroup: ApiDocumentGroup,
      currentCategory: ApiDocumentCategory,
      currentApiDocumentWithVersion: RootApiDocumentWithVersion,
      rootApiDocument: RootApiDocument,
      context: ApiDocumentHtmlRendererContext): Elem = {
    <div>
      <div>
        <h1 class="bd-title">{apiDocumentGroup.group}</h1>
      </div>
      <p>
        {
        apiDocumentGroup.apiDocuments.toSeq.sortBy(t=>Method.priority(t._1._1)).map { apiDocument =>
          renderApiDocument(apiDocument._2,apiDocumentGroup,currentCategory,currentApiDocumentWithVersion,rootApiDocument,context)
        }
        }
      </p>

    </div>
  }

  private[radoc] def renderApiDocument(
      apiDocument: ApiDocument,
      currentGroup: ApiDocumentGroup,
      currentCategory: ApiDocumentCategory,
      currentApiDocumentWithVersion: RootApiDocumentWithVersion,
      rootApiDocument: RootApiDocument,
      context: ApiDocumentHtmlRendererContext): Elem = {

    <p>
      <div >

        <p>
          <div class={"badge p-2  " + (apiDocument.method match{
            case Method.Get=>"badge-success"
            case Method.Post => "badge-primary"
            case Method.Put => "badge-warning"
            case Method.Delete => "badge-danger"
            case _ =>"badge-default"
          } ) } ><span class="font-weight-bold text-white"  style="font-size:16px;" >{apiDocument.method}</span></div>
        </p>
        <div>
          <p>
            {apiDocument.description.renderHtml(HtmlRenderArguments(
            rootApiDocument,
            currentApiDocumentWithVersion,
            currentCategory,
            currentGroup,
            apiDocument,
            apiDocument.messageDocumentMap.head._2,
            context))}
          </p>
        </div>


        <ul class="nav nav-pills">
          {apiDocument.messageDocumentMap.values.map(messageDocument =>
          <li class="nav-item "  ><a class={"nav-link" + (if(messageDocument == apiDocument.messageDocumentMap.values.head)" active" else "")} data-toggle="tab" href={"#" + tabId(apiDocument,messageDocument) }>{messageDocument.messageName}</a></li>)
          }
        </ul>
        <div class="tab-content">
          {apiDocument.messageDocumentMap.values.map(messageDocument =>
          renderMessageDocument(HtmlRenderArguments(rootApiDocument,currentApiDocumentWithVersion,currentCategory,currentGroup,apiDocument,messageDocument,context)))
          }
        </div>

      </div>
      <br/>
      <br/>
    </p>

  }

  def renderMessage(message: Message,
                    renderMessageHeadHandler: => Node,
                    contentId: String,
                    htmlRenderArguments: HtmlRenderArguments): Elem = {
    <div>
      <div>
        {renderParameters("Headers", message.headers.getHeaders,htmlRenderArguments)}
        {
        if (message.bodyHint.typeParameterMap.nonEmpty){
          <span class="font-weight-bold" style="font-size:20px;">Body Content Fields</span>
            <div class="card card-body bg-light">
              {renderBodyHint(message.bodyHint,htmlRenderArguments)}
            </div>
        }
        }

      </div>
      {renderExample(message,renderMessageHeadHandler,contentId,htmlRenderArguments)}
    </div>

  }

  def renderParameters(title: String,
                       parameters: Seq[Parameter],
                       htmlRenderArguments: HtmlRenderArguments): Node = {
    if (parameters.nonEmpty) {
      <div>
        <div ><span class="font-weight-bold" style="font-size:20px;">{title}</span></div>
        <table class="table table-sm table-striped table-bordered">
          <thead class="thead-inverse">
            <tr>
              <th scope="col" >Field</th>
              <th scope="col" >Type</th>
              <th scope="col" >Description</th>
            </tr>
          </thead>
          {
          parameters.map { parameter =>
            <tr >
              <td scope="row" style="width:15%;">{renderParameter(parameter,parameter.field)}</td>
              <td style="width:15%;" >{parameter.typeName}</td>
              <td style="width:70%;" >{parameter.description.renderHtml(htmlRenderArguments)}
              </td>
            </tr>
          }
          }
        </table>

      </div>

    } else xml.Text("")

  }

  def renderExample(message: Message,
                    renderMessageHeadHandler: => Node,
                    contentId: String,
                    htmlRenderArguments: HtmlRenderArguments): Node = {

    <details>
      <summary>expand example</summary>
      <div style="background:black;color:white;" >
        {renderMessageHeadHandler}
        {renderContent(message,contentId,htmlRenderArguments)}
      </div>
    </details>

  }

  def renderContent(message: Message,
                    contentId: String,
                    htmlRenderArguments: HtmlRenderArguments): Node = {
    val context = htmlRenderArguments.context
    if (context.contentHtmlRenderer != null) {
      context.contentHtmlRenderer.render(message, htmlRenderArguments)
    } else {
      ContentHtmlRenderer.default.render(message, htmlRenderArguments)
    }

  }

  def renderBodyHint(bodyHint: BodyHint,
                     htmlRenderArguments: HtmlRenderArguments): Node = {
    <div>
      {
      bodyHint.typeParameterMap.map {
        case (key, value) => renderParameters(key, value,htmlRenderArguments)
      }
      }
    </div>
  }

  private[radoc] def renderMessageDocument(
      htmlRenderArguments: HtmlRenderArguments): Elem = {
    val currentApiDocument = htmlRenderArguments.currentAPIDocument
    val currentMessageDocument = htmlRenderArguments.currentMessageDocument
    val ti = tabId(currentApiDocument, currentMessageDocument)
    <div id={ti} class={"tab-pane" + (if (currentMessageDocument == currentApiDocument.messageDocumentMap.values.head)" active" else "") } >
      <p>
        {renderUrlPathOuter(renderDisplayUrlPath(currentMessageDocument.request.path))}
        <h3>Request</h3>
        {renderUrlPathOuter(renderActualURLPath(currentMessageDocument.request.path))}
        {renderParameters("Path parameters",currentMessageDocument.request.path.pathParameters,htmlRenderArguments)}
        {renderParameters("Queries",currentMessageDocument.request.path.queries,htmlRenderArguments)}

        <div>{renderMessage(currentMessageDocument.request,xml.Text(""),"request-content-" + ti,htmlRenderArguments)}</div>
      </p>

      <p>
        <h3>Response</h3>
        <div>{renderMessage(currentMessageDocument.response,xml.Text(""),"response-content-"+ ti,htmlRenderArguments)}</div>
      </p>

    </div>

  }

  private def renderCss(fileName: String): Node = {
    val path = "assets/css/" + fileName
    <style>{Unparsed(ResourceLoader.loadCss(fileName))}</style>
  }

  private def renderJavaScript(fileName: String): Node = {
    val path = "assets/js/" + fileName
    <script type="text/javascript">{Unparsed(ResourceLoader.loadJavaScript(fileName).replace("</script>","\\u003c\\u002f\\u0073\\u0063\\u0072\\u0069\\u0070\\u0074\\u003e"))}</script>
  }

  private def renderUrlPathOuter(node: Seq[Node]): Node = {
    <div class="badge p-2" style="background:black;" ><span class="font-weight-bold text-white" style="font-size:20px;">{node}</span></div>
  }

  private def renderDisplayUrlPath(urlPath: UrlPath): Seq[Node] = {
    urlPath.parts.collect {

      case plain: PlainPath => xml.Text(plain.path)
      case pathParameter: PathParameter =>
        renderParameter(pathParameter.parameter, pathParameter.displayField)
      case separator: Separator => xml.Text(separator.toString)
    }
  }

  private def renderActualURLPath(urlPath: UrlPath): Seq[Node] = {
    urlPath.parts.collect {
      case plain: PlainPath => xml.Text(plain.path)
      case pathParameter: PathParameter =>
        renderParameter(pathParameter.parameter,
                        pathParameter.parameter.value.getValueString)
      case queryParameter: QueryParameter =>
        <span>{renderParameter(queryParameter.parameter, queryParameter.parameter.field)}{"="}{renderParameter(queryParameter.parameter, queryParameter.parameter.value.getValueString)}</span>
      case p => xml.Text(p.toString)
    }
  }

  private def renderParameter(parameter: Parameter, text: String): Node = {
    <span style={"color:" + parameter.color.toString} >{text}</span>
  }

  private[radoc] def tabId(apiDocument: ApiDocument,
                           messageDocument: MessageDocument): String = {
    Link.fragmentId(apiDocument) + messageDocument.messageName.hashCode
  }
}
