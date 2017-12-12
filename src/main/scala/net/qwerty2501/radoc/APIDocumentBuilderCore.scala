package net.qwerty2501.radoc

import scala.collection.mutable

class APIDocumentBuilderCore(private val apiClient: APIClient) {
  private var rootAPIDocument = RootAPIDocument(Map())

  def request(req: Request,
              category: String,
              group: String,
              description: String,
              apiName: String,
              version: Version,
              extendArgs: Map[String, String]): Response = {
    val res = apiClient.request(req)
    append(req, res, category, group, description, apiName, version, extendArgs)
    res
  }

  def request(req: Request,
              category: String,
              description: String,
              apiName: String,
              version: Version,
              extendArgs: Map[String, String]): Response = {
    request(req,
            category,
            req.path.displayPath,
            description,
            apiName,
            version,
            extendArgs)
  }

  def request(req: Request,
              category: String,
              description: String,
              version: Version,
              extendArgs: Map[String, String]): Response = {
    val res = apiClient.request(req)
    append(req,
           res,
           category,
           req.path.displayPath,
           description,
           res.status.toString,
           version,
           extendArgs)
    res
  }

  def request(req: Request, description: String): Response =
    request(req, "", description, Version(), Map())

  def request(req: Request): Response = request(req, "")

  private def append(req: Request,
                     res: Response,
                     category: String,
                     group: String,
                     description: String,
                     apiName: String,
                     version: Version,
                     extendArgs: Map[String, String]): Unit = {
    val apiGroup = if (group == "") req.path.displayPath else group
    val messageName = if (apiName == "") res.status.toString else ""
    val rootAPIDocumentWithVersion = rootAPIDocument.documents
      .getOrElse(version, RootAPIDocumentWithVersion(version, Map()))

    val apiDocumentCategory =
      rootAPIDocumentWithVersion.apiCategories
        .getOrElse(category, APIDocumentCategory(category, Map()))
    val apiDocumentGroup = apiDocumentCategory.apiDocumentGroups
      .getOrElse(apiGroup, APIDocumentGroup(apiGroup, Map()))

    val groupKey = (req.method, req.path.displayPath)
    val apiDocument = apiDocumentGroup.apiDocuments
      .getOrElse(groupKey, APIDocument(Seq(), "", Map()))

    if (apiDocument.description != "" && description != "") {
      throw new IllegalStateException("description is already set.")
    }

    if (apiDocument.extendArguments.nonEmpty && extendArgs.nonEmpty) {
      throw new IllegalStateException("extendArguments is already set.")
    }

    val newAPIDocument = APIDocument(
      apiDocument.messageDocuments :+
        MessageDocument(apiName, req, res),
      if (description != "") description else apiDocument.description,
      extendArgs)

    val apiDocuments = mutable.Map(apiDocumentGroup.apiDocuments.toSeq: _*)
    apiDocuments.put(groupKey, newAPIDocument)
    val newAPIDocumentGroup =
      APIDocumentGroup(apiDocumentGroup.group, apiDocuments.toMap)

    val apiDocumentGroups =
      mutable.Map(apiDocumentCategory.apiDocumentGroups.toSeq: _*)
    apiDocumentGroups.put(apiGroup, newAPIDocumentGroup)

    val newAPIDocumentCategory =
      APIDocumentCategory(category, apiDocumentGroups.toMap)

    val apiCategories =
      mutable.Map(rootAPIDocumentWithVersion.apiCategories.toSeq: _*)

    apiCategories.put(category, newAPIDocumentCategory)

    val newRooAPIDocumentWithVersion =
      RootAPIDocumentWithVersion(version, apiCategories.toMap)

    val newRootAPIDocumentWithVersions =
      mutable.Map(rootAPIDocument.documents.toSeq: _*)
    newRootAPIDocumentWithVersions.put(version, newRooAPIDocumentWithVersion)

    rootAPIDocument = RootAPIDocument(newRootAPIDocumentWithVersions.toMap)
  }
}

object APIDocumentBuilderCore {
  def apply(apiClient: APIClient) = new APIDocumentBuilderCore(apiClient)
}
