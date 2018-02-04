package net.qwerty2501.radoc

case class HtmlRenderArguments(
                                rootAPIDocument: RootApiDocument,
                                currentRootAPIDocumentWithVersion: RootApiDocumentWithVersion,
                                currentCategory: ApiDocumentCategory,
                                currentGroup: ApiDocumentGroup,
                                currentAPIDocument: ApiDocument,
                                currentMessageDocument: MessageDocument,
                                context: ApiDocumentHtmlRendererContext)
