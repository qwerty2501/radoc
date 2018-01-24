package net.qwerty2501.radoc

import scala.collection.mutable

abstract class APIDocumentBuilderBase(private val apiClient: APIClient) {
  private var rootAPIDocument = RootAPIDocument(Map())

  def getRootAPIDocument: RootAPIDocument = rootAPIDocument
  def request(req: Request, documentArgs: DocumentArgs): Response = {
    val res = apiClient.request(req)
    append(req, res, documentArgs)
    res
  }

  def request(req: Request, category: String, description: String): Response =
    request(req, DocumentArgs(category, description, Version(), Map()))

  def request(req: Request, description: String): Response =
    request(req, "", description)

  def request(req: Request): Response = request(req, "")

  private def append(req: Request,
                     res: Response,
                     documentArgs: DocumentArgs): Unit = {
    val apiGroup =
      if (documentArgs.group == "") req.path.displayPath else documentArgs.group
    val messageName =documentArgs.messageName
    val rootAPIDocumentWithVersion = rootAPIDocument.documents
      .getOrElse(documentArgs.version,
                 RootAPIDocumentWithVersion(documentArgs.version, Map()))

    val apiDocumentCategory =
      rootAPIDocumentWithVersion.apiCategories
        .getOrElse(documentArgs.category,
                   APIDocumentCategory(documentArgs.category, Map()))
    val apiDocumentGroup = apiDocumentCategory.apiDocumentGroups
      .getOrElse(apiGroup, APIDocumentGroup(apiGroup, Map()))

    val groupKey = (req.method, req.path.displayPath)
    val apiDocument = apiDocumentGroup.apiDocuments
      .getOrElse(groupKey, APIDocument(Seq(), "", Map()))

    if (apiDocument.description != "" && documentArgs.description != "") {
      throw new IllegalStateException("description is already set.")
    }

    if (apiDocument.extendArguments.nonEmpty && documentArgs.extendArgs.nonEmpty) {
      throw new IllegalStateException("extendArguments is already set.")
    }

    val newAPIDocument = APIDocument(
      apiDocument.messageDocuments :+
        MessageDocument(documentArgs.messageName, req, res),
      if (documentArgs.description != "") documentArgs.description
      else apiDocument.description,
      documentArgs.extendArgs
    )

    val apiDocuments = mutable.Map(apiDocumentGroup.apiDocuments.toSeq: _*)
    apiDocuments.put(groupKey, newAPIDocument)
    val newAPIDocumentGroup =
      APIDocumentGroup(apiDocumentGroup.group, apiDocuments.toMap)

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

    rootAPIDocument = RootAPIDocument(newRootAPIDocumentWithVersions.toMap)
  }
}
