package net.qwerty2501.radoc

import scala.collection.mutable

class APIDocumentBuilder(private val apiClient: APIClient) {
  private var rootAPIDocument = RootAPIDocument("", Map())

  def setRootDocumentTitle(title: String): Unit =
    rootAPIDocument = RootAPIDocument(title, rootAPIDocument.documents)
  def getRootAPIDocument: RootAPIDocument = rootAPIDocument
  def request(req: Request, documentArgs: DocumentArgs): Response = {
    val res = apiClient.request(req)
    append(req, res, documentArgs)
    res
  }

  def request(req: Request, category: String, description: Text): Response =
    request(req, category, description, Version())
  def request(req: Request,
              category: String,
              description: Text,
              version: Version): Response =
    request(req, DocumentArgs(category, description, version))

  def request(req: Request, description: Text): Response =
    request(req, "", description)

  def request(req: Request): Response = request(req, Text())

  def append(req: Request, res: Response): Unit =
    append(req, res, Text())
  def append(req: Request, res: Response, description: Text): Unit =
    append(req, res, "", description)

  def append(req: Request,
             res: Response,
             category: String,
             description: Text): Unit =
    append(req, res, category, description, Version())
  def append(req: Request,
             res: Response,
             category: String,
             description: Text,
             version: Version): Unit =
    append(req, res, DocumentArgs(category, description, version))

  def append(req: Request, res: Response, documentArgs: DocumentArgs): Unit = {
    val apiGroup =
      if (documentArgs.group == "") req.path.displayPath else documentArgs.group

    val rootAPIDocumentWithVersion = rootAPIDocument.documents
      .getOrElse(documentArgs.version,
                 RootAPIDocumentWithVersion(documentArgs.version, Map()))

    val apiDocumentCategory =
      rootAPIDocumentWithVersion.apiCategories
        .getOrElse(documentArgs.category,
                   APIDocumentCategory(documentArgs.category, Map()))
    val apiDocumentGroup = apiDocumentCategory.apiDocumentGroups
      .getOrElse(apiGroup,
                 APIDocumentGroup(apiGroup,
                                  Map(),
                                  apiDocumentCategory.category,
                                  rootAPIDocumentWithVersion.version))

    val groupKey = (req.method, req.path.displayPath)
    val apiDocument = apiDocumentGroup.apiDocuments
      .getOrElse(groupKey,
                 APIDocument(req.method,
                             req.path,
                             Map(),
                             Text(),
                             apiDocumentGroup.group,
                             apiDocumentCategory.category,
                             rootAPIDocumentWithVersion.version))

    if (apiDocument.description != Text() && documentArgs.description != Text()) {
      throw new IllegalStateException("description is already set.")
    }

    val messageName = generateMessageName(
      apiDocument.messageDocumentMap,
      "[%d %s]%s"
        .format(res.status.code, res.status.toString, documentArgs.messageName))

    val newAPIDocument = APIDocument(
      apiDocument.method,
      apiDocument.path,
      apiDocument.messageDocumentMap + (messageName ->
        MessageDocument(messageName, req, res)),
      if (documentArgs.description != Text()) documentArgs.description
      else apiDocument.description,
      apiDocumentGroup.group,
      apiDocumentCategory.category,
      rootAPIDocumentWithVersion.version
    )

    val apiDocuments = mutable.Map(apiDocumentGroup.apiDocuments.toSeq: _*)
    apiDocuments.put(groupKey, newAPIDocument)
    val newAPIDocumentGroup =
      APIDocumentGroup(apiDocumentGroup.group,
                       apiDocuments.toMap,
                       apiDocumentCategory.category,
                       rootAPIDocumentWithVersion.version)

    val apiDocumentGroups =
      mutable.Map(apiDocumentCategory.apiDocumentGroups.toSeq: _*)
    apiDocumentGroups.put(apiGroup, newAPIDocumentGroup)

    val newAPIDocumentCategory =
      APIDocumentCategory(documentArgs.category, apiDocumentGroups.toMap)

    val apiCategories =
      mutable.Map(rootAPIDocumentWithVersion.apiCategories.toSeq: _*)

    apiCategories.put(documentArgs.category, newAPIDocumentCategory)

    val newRooAPIDocumentWithVersion =
      RootAPIDocumentWithVersion(documentArgs.version, apiCategories.toMap)

    val newRootAPIDocumentWithVersions =
      mutable.Map(rootAPIDocument.documents.toSeq: _*)
    newRootAPIDocumentWithVersions.put(documentArgs.version,
                                       newRooAPIDocumentWithVersion)

    rootAPIDocument = RootAPIDocument(rootAPIDocument.title,
                                      newRootAPIDocumentWithVersions.toMap)
  }

  private def generateMessageName(
      messageDocumentMap: Map[String, MessageDocument],
      messageName: String): String =
    if (messageDocumentMap.keys.exists(_ == messageName))
      generateNewMessageName(messageDocumentMap, messageName, 2)
    else messageName

  private def generateNewMessageName(
      messageDocumentMap: Map[String, MessageDocument],
      messageName: String,
      number: Int): String =
    if (messageDocumentMap.keys.exists(_ == messageName + "-" + number))
      generateNewMessageName(messageDocumentMap, messageName, number + 1)
    else messageName + "-" + number
}
