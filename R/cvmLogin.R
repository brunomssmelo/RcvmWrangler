#' Efetua o Login no webservice da CVM
#'
#' @param identificador do sistema junto a CVM
#' @param ver versao SOAP ("1.1" ou "1.2")
#' @return um objeto do tipo data.frame contendo o GUID e idSessao.
#' @author Bruno M. S. S. Melo
#' @details
#' Esta funcao efetua o Login no webservice retornando o GUID e idSessao a serem utilizados
#' em outros métodos disponibilizados pela CVM.
#' Obs.: Para informacoes relativas ao cadastro, consulte o site da CVM.
#' @examples
#' \dontrun{
#' dfSession <- cvmLogin(iNrSist, strSenha)
#' }
#' @seealso \url{http://sistemas.cvm.gov.br}
#' @export
cvmLogin <- function(iNrSist, strSenha, verSoap = "1.1"){

  headerfields = c(
    Accept = "text/xml",
    Accept = "multipart/*",
    'Content-Type' = "text/xml; charset=utf-8",
    SOAPAction = "http://www.cvm.gov.br/webservices/Login"
  )

  body <- cvmRetornaCorpoSoap("Login", "1.1")
  body <- gsub(x = body,
               pattern = "(<Guid>) string ",
               replacement= paste0("\\1","ddbeda5b-e56a-4d3b-9996-2f0301250630"))
  body <- gsub(x = body,
               pattern = "(<IdSessao>) int ",
               replacement= paste0("\\1","686285394"))
  body <- gsub(x = body,
               pattern = "(<iNrSist>) int ",
               replacement= paste0("\\1",iNrSist))
  body <- gsub(x = body,
               pattern = "(<strSenha>) string ",
               replacement= paste0("\\1",strSenha))


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


  xml <- XML::xmlParse(responseString)

  # Use the xmlRoot-function to access the top node
  xmltop <- XML::xmlRoot(xml)

  # To extract the XML-values from the document, use xmlSApply:
  response <- XML::xmlSApply(
    XML::xmlChildren(xmltop)$Header,
    function(x) XML::xmlSApply(x, XML::xmlValue))


  # Finally, get the data in a data-frame and have a look at the first rows and columns
  dfResponse <- data.frame(t(response),row.names=NULL,stringsAsFactors = F)
}
