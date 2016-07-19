#' Solicita autorizacao para o download do cadastro dos fundos registrados na CVM.
#'
#' @param loginInfo data.frame retornado pela funcao cvmLogin, contendo informacoes de login.
#' @param refDate string contendo data de referencia no formato aaaa-mm-dd.
#' @return um objeto do tipo data.frame contendo a url para efetuar o download solicitado.
#' @author Bruno M. S. S. Melo <ctbrunomelo@gmail.com>
#' @details
#' Esta funcao solicita autorizacao pala efetuar o download do cadastro dos fundos registrados na CVM.
#' @examples
#' \dontrun{
#' dfAutorizacao <- cvmSolicAutorizDownloadCadastro(loginInfo = dfLogin)
#' }
#' @seealso \url{http://sistemas.cvm.gov.br}
#' @export
cvmSolicAutorizDownloadCadastro <- function(loginInfo, refDate){

  dfLoginInfo <- loginInfo

  headerfields = c(
    Accept = "text/xml",
    Accept = "multipart/*",
    'Content-Type' = "text/xml; charset=utf-8",
    SOAPAction = "http://www.cvm.gov.br/webservices/solicAutorizDownloadCadastro"
  )

  body <- cvmRetornaCorpoSoap("solicAutorizDownloadCadastro", "1.1")
  body <- gsub(x = body,
               pattern = "(<Guid>) string ",
               replacement= paste0("\\1", dfLoginInfo$Guid))
  body <- gsub(x = body,
               pattern = "(<IdSessao>) int ",
               replacement= paste0("\\1", dfLoginInfo$IdSessao))
  body <- gsub(x = body,
               pattern = "(<strDtRefer>) int ",
               replacement= paste0("\\1", refDate))
  body <- gsub(x = body,
               pattern = "(<strMotivoAutorizDownload>) string ",
               replacement= paste0("\\1", "Pedido de autorizacao"))


  reader = RCurl::basicTextGatherer()

  RCurl::curlPerform(
    url = "http://sistemas.cvm.gov.br/webservices/Sistemas/SCW/CDocs/WsDownloadInfs.asmx",
    httpheader = headerfields,
    postfields = body,
    writefunction = reader$update
  )

  responseString <- reader$value()

  tagFaultPos <- stringr::str_locate_all(
    responseString, stringr::fixed(c("<faultstring>","</faultstring>")))

  if (length(tagFaultPos[[1]])>0) {
    faultString <- stringr::str_sub(responseString,
                                    tagFaultPos[[1]][1,1]+nchar("<faultstring>"),
                                    tagFaultPos[[2]][1,2]-nchar("</faultstring>"))

    stop(faultString)
  }

  # Use the xmlRoot-function to access the top node
  xml <- XML::xmlParse(responseString)
  xmltop <- XML::xmlRoot(xml)

  # To extract the XML-values from the document, use xmlSApply:
  response <- XML::xmlSApply(
    XML::xmlChildren(xmltop)$Body,
    function(x) XML::xmlSApply(x, XML::xmlValue))


  # Finally, get the data in a data-frame and have a look at the first rows and columns
  dfResponse <- data.frame(t(response),row.names=NULL,stringsAsFactors = F)
}
