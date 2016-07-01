#' Retorna o formato da mensagem SOAP a ser enviado ao webservice da CVM
#'
#' @param op nome do metodo desejado
#' @param ver versao SOAP
#' @return um objeto do tipo data.frame contendo o resumo da politica de investimentos do RPPS.
#' @author Bruno M. S. S Melo
#' @details
#' Esta funcao extrai o corpo da mensagem SOAP a ser enviada ao webservice da CVM, de acordo
#' com o nome do método fornecido pelo parametro ("op") e versao ("ver") SOAP desejada.
#' @examples
#' \dontrun{
#' body <- cvmRetornaCorpoSoap("Login", "1.1")
#' }
#' @seealso \code{RCurl}, \code{XML} \code{stringr}
#' @export
cvmRetornaCorpoSoap <- function(op, ver) {

  # Operation soap definition url
  opUrl <- paste0("http://sistemas.cvm.gov.br/webservices/Sistemas/SCW/CDocs/WsDownloadInfs.asmx?op=",op)
  html.text <- RCurl::getURL(opUrl, followlocation = TRUE)

  # parse html
  doc = XML::htmlParse(html.text, asText=TRUE)
  plain.text <- XML::xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]",
                                 XML::xmlValue)
  plain.text <- paste(plain.text, collapse = " ")

  if (ver == "1.1") {
    envelopeTags <- c("<soap:Envelope", "</soap:Envelope>")

    tagPos <- stringr::str_locate_all(plain.text, stringr::fixed(envelopeTags))
  }

  body <- stringr::str_sub(plain.text, tagPos[[1]][1,1], tagPos[[2]][1,2])

}
