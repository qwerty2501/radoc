package net.qwerty2501.radoc

import java.io.{File, PrintWriter}

import scala.io.Source
import scala.xml._

private object APIDocumentHtmlRenderer {

  private[radoc] def render(rootAPIDocument: RootAPIDocument,
                            context: APIDocumentRendererContext): String = {
    val doc = renderRootAPIDocument(rootAPIDocument, context)
    dtd
      .DocType("html", dtd.SystemID("about:legacy-compat"), Nil)
      .toString() + "\n" + Xhtml.toXhtml(doc)
  }

  private[radoc] def getResourceText(path: String): String = {
    Source.fromResource(path, this.getClass.getClassLoader).mkString
  }

  private[radoc] def renderRootAPIDocument(
      rootAPIDocument: RootAPIDocument,
      context: APIDocumentRendererContext): Elem = {

    <html>
      <head>
        <meta charset="UTF-8"/>
        <title>{rootAPIDocument.title}</title>
        {renderCss("bootstrap.min.css")}
        {renderCss("simple-sidebar.css")}
        { renderCss("highlight.default.min.css")}
        {renderJavaScript("highlight.pack.js")}

      </head>

      <body>

        <nav class="navbar navbar-dark bg-dark sticky-top"  >

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



        <div class="container-fluid" >
          {
          if (rootAPIDocument.documents.size == 1) {
            renderRootAPIDocumentWithVersion(rootAPIDocument.documents.head._2,rootAPIDocument, context)
          } else if (rootAPIDocument.documents.size > 1) {

          }
          }
        </div>
        {renderJavaScript("jquery-3.3.1.slim.min.js")}
        {renderJavaScript("bootstrap.bundle.min.js")}

        {renderJavaScript("ui.js")}
      </body>
    </html>
  }

  private[radoc] def renderRootAPIDocumentWithVersion(
      rootAPIDocumentWithVersion: RootAPIDocumentWithVersion,
      rootAPIDocument: RootAPIDocument,
      context: APIDocumentRendererContext): Elem = {

    val apiCategories = rootAPIDocumentWithVersion.apiCategories
    val mainContentId =
      Link.mainContentId(rootAPIDocumentWithVersion.version)

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
                <template id={ Link.templateId(apiCategory._2.category,apiDocumentGroup._2.group) } >
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

  private[radoc] def renderAPIGroupDocument(
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
        apiDocumentGroup.apiDocuments.toSeq.sortBy(t=>Method.priority(t._1._1)).map { apiDocument =>
          renderAPIDocument(apiDocument._2,apiDocumentGroup,currentCategory,currentAPIDocumentWithVersion,rootAPIDocument,context)
        }
        }
      </p>

    </div>
  }

  private[radoc] def renderAPIDocument(
      apiDocument: APIDocument,
      currentGroup: APIDocumentGroup,
      currentCategory: APIDocumentCategory,
      currentAPIDocumentWithVersion: RootAPIDocumentWithVersion,
      rootAPIDocument: RootAPIDocument,
      context: APIDocumentRendererContext): Elem = {

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
        <p>
          <div class="bg-dark badge p-2 "><span class="font-weight-bold text-white" style="font-size:20px;"    >{apiDocument.path.displayPath}</span></div>
        </p>
        <div>
          <p>
            {apiDocument.description.renderHtml(TextRenderingArguments(
            rootAPIDocument,
            currentAPIDocumentWithVersion,
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
          renderMessageDocument(messageDocument,apiDocument,currentGroup,currentCategory,currentAPIDocumentWithVersion,rootAPIDocument,context))
          }
        </div>

      </div>
      <br/>
      <br/>
    </p>

  }

  private[radoc] def renderMessageDocument(
      messageDocument: MessageDocument,
      currentAPIDocument: APIDocument,
      currentGroup: APIDocumentGroup,
      currentCategory: APIDocumentCategory,
      currentAPIDocumentWithVersion: RootAPIDocumentWithVersion,
      rootAPIDocument: RootAPIDocument,
      context: APIDocumentRendererContext): Elem = {

    def renderMessage(message: Message, contentId: String): Elem = {
      <div>
        <div>
          {
          renderParameters("Headers", message.headerMap.map{ arg=>
            val name = arg._1
            val param = arg._2
            Parameter(name,param.value,param.typeName,param.description)}.toSeq)
          }
        </div>

        {renderContent(message,contentId)}

      </div>

    }

    def renderContent(message: Message, contentId: String): Node = {
      if (message.content != Content()) {
        <div>

          <button type="button" class="btn btn-info" data-toggle="collapse" data-target={"#"+contentId}>expand example content</button>
          <div id={contentId} class="collapse">
            <pre><br/><code class="json">{message.content.renderHtml(ContentType(message.headerMap))}</code><br/></pre>
          </div>
        </div>

      } else {
        xml.Text("")
      }

    }

    def renderParameters(title: String, parameters: Seq[Parameter]): Node = {
      if (parameters.nonEmpty) {
        <div>
          <div ><h5>{title}</h5></div>
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
                <td scope="row" style="width:15%;">{parameter.name}</td>
                <td style="width:15%;" >{parameter.typeName}</td>
                <td style="width:70%;" >{parameter.description.renderHtml(
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
    val ti = tabId(currentAPIDocument, messageDocument)
    <div id={ti} class={"tab-pane" + (if (messageDocument == currentAPIDocument.messageDocumentMap.values.head)" active" else "") } >
      <p>
        <h3>Request</h3>
        {messageDocument.request.path.actualPath}
        {renderParameters("Path parameters",messageDocument.request.path.pathParameters)}
        {renderParameters("Queries",messageDocument.request.path.queries)}
        <div>{renderMessage(messageDocument.request,"request-content-" + ti)}</div>
      </p>

      <p>
        <h3>Response</h3>
        <div>{renderMessage(messageDocument.response,"response-content-"+ ti)}</div>
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

  private[radoc] def tabId(apiDocument: APIDocument,
                           messageDocument: MessageDocument): String = {
    Link.fragmentId(apiDocument) + messageDocument.messageName.hashCode
  }
}
