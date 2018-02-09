package net.qwerty2501.radoc

import scala.collection.mutable

class ApiDocumentBuilder(private val apiClient: ApiClient,
                         private val defaultVersion: Version) {
  def this(apiClient: ApiClient) = this(apiClient, Version.firstVersion)
  private var rootAPIDocument = RootApiDocument("", Map())

  def setRootDocumentTitle(title: String): Unit =
    rootAPIDocument = RootApiDocument(title, rootAPIDocument.documents)
  def buildRootAPIDocument: RootApiDocument = rootAPIDocument

  def requestAndRequest(req: Request): Response =
    requestAndRequest(req, DocumentArgs())
  def requestAndRequest(req: Request, documentArgs: DocumentArgs): Response = {
    val res = apiClient.request(req)
    append(req, res, documentArgs)
    res
  }

  def append(req: Request, res: Response): Unit =
    append(req, res, DocumentArgs())
  def append(req: Request, res: Response, documentArgs: DocumentArgs): Unit = {
    val apiGroup =
      if (documentArgs.group == "") req.path.displayPath else documentArgs.group

    val targetVersion =
      if (documentArgs.version == Version.empty) defaultVersion
      else documentArgs.version

    rootAPIDocument.synchronized {

      val rootAPIDocumentWithVersion = rootAPIDocument.documents
        .getOrElse(targetVersion,
                   RootApiDocumentWithVersion(targetVersion, Map()))

      val apiDocumentCategory =
        rootAPIDocumentWithVersion.apiCategories
          .getOrElse(documentArgs.category,
                     ApiDocumentCategory(documentArgs.category, Map()))
      val apiDocumentGroup = apiDocumentCategory.apiDocumentGroups
        .getOrElse(apiGroup,
                   ApiDocumentGroup(apiGroup,
                                    Map(),
                                    apiDocumentCategory.category,
                                    rootAPIDocumentWithVersion.version))

      val groupKey = (req.method, req.path.displayPath)
      val apiDocument = apiDocumentGroup.apiDocuments
        .getOrElse(groupKey,
                   ApiDocument(req.method,
                               req.path,
                               Map(),
                               Text(),
                               apiDocumentGroup.group,
                               apiDocumentCategory.category,
                               rootAPIDocumentWithVersion.version))

      if (apiDocument.description != Text() && documentArgs.description != Text()) {
        throw new IllegalStateException("description is already set.")
      }

      val messageName = generateMessageName(apiDocument.messageDocumentMap,
                                            "[%d %s]%s"
                                              .format(res.status.code,
                                                      res.status.toString,
                                                      documentArgs.messageName))

      val newAPIDocument = ApiDocument(
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
        ApiDocumentGroup(apiDocumentGroup.group,
                         apiDocuments.toMap,
                         apiDocumentCategory.category,
                         rootAPIDocumentWithVersion.version)

      val apiDocumentGroups =
        mutable.Map(apiDocumentCategory.apiDocumentGroups.toSeq: _*)
      apiDocumentGroups.put(apiGroup, newAPIDocumentGroup)

      val newAPIDocumentCategory =
        ApiDocumentCategory(documentArgs.category, apiDocumentGroups.toMap)

      val apiCategories =
        mutable.Map(rootAPIDocumentWithVersion.apiCategories.toSeq: _*)

      apiCategories.put(documentArgs.category, newAPIDocumentCategory)

      val newRooAPIDocumentWithVersion =
        RootApiDocumentWithVersion(targetVersion, apiCategories.toMap)

      val newRootAPIDocumentWithVersions =
        mutable.Map(rootAPIDocument.documents.toSeq: _*)
      newRootAPIDocumentWithVersions.put(targetVersion,
                                         newRooAPIDocumentWithVersion)

      rootAPIDocument = RootApiDocument(rootAPIDocument.title,
                                        newRootAPIDocumentWithVersions.toMap)
    }
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
