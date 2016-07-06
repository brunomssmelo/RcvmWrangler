#' Retorna o formato da mensagem SOAP a ser enviado ao webservice da CVM
#'
#' @param op nome do metodo desejado
#' @param ver versao SOAP ("1.1" ou "1.2")
#' @return um objeto do tipo data.frame contendo o resumo da politica de investimentos do RPPS.
#' @author Bruno M. S. S. Melo
#' @details
#' Esta funcao extrai o corpo da mensagem SOAP a ser enviada ao webservice da CVM, de acordo
#' com o nome do método fornecido pelo parametro ("op") e versao ("ver") SOAP desejada.
#' As opcoes disponiveis sao
#'\itemize{
#'\item \code{op = "Login"} Verifica credenciais e efetua o login no CVMWeb.
#'\item \code{op = "retornaDtLmtEntrDocsArqsDisp"} Retorna a data limite de entrega de documentos (pelos administradores) que foram considerados na geração dos arquivos compactados, atualmente disponíveis no servidor.
#'\item \code{op = "retornaListaComptcDocs"} Retorna um array de strings contendo as datas de competências de documentos entregues a partir de determinada data.
#'\item \code{op = "retornaListaComptcDocsAdm"} Retorna um array de strings contendo as datas de competências de documentos entregues a partir de determinada data e CNPJ de administrador
#'\item \code{op = "retornaListaComptcDocsPartic"} Retorna um array de strings contendo as datas de competências de documentos entregues a partir de determinada data e CNPJ de participante.
#'\item \code{op = "retornaListaDownloadDocs"} Retorna um Array de strings com as URLS para download de documentos recebidos pela CVM. Contempla Documentos Eventuais, Demonstrativo Trimestral e Demonstrações Financeiras Anuais. Disponível somente para fundos FIDC.
#'\item \code{op = "solicAutorizDownloadArqAnual"} Verifica autorização para download e retorna string contendo URL para download de arquivo (referência PeriodoAnual)
#'\item \code{op = "solicAutorizDownloadArqComptc"} Verifica autorização para download e retorna string contendo URL para download de arquivo (referência DtComptcDoc).
#'\item \code{op = "solicAutorizDownloadArqEntrega"} Verifica autorização para download e retorna string contendo URL para download de arquivo (referência DtEntregaDoc)
#'\item \code{op = "solicAutorizDownloadArqEntregaPorData"} Verifica autorização para download e retorna string contendo URL para download de arquivo (referência DtEntregaDoc)
#'\item \code{op = "solicAutorizDownloadCadastro"} Verifica autorização para download e retorna string contendo URL para download de arquivo (referência DtComptcDoc).
#' }
#' @examples
#' \dontrun{
#' body <- cvmRetornaCorpoSoap("Login", "1.1")
#' }
#' @seealso \code{RCurl}, \code{XML}, \url{http://sistemas.cvm.gov.br/webservices/Sistemas/SCW/CDocs/WsDownloadInfs.asmx}
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
    envelopeTags <- c('<?xml version="1.0" encoding="utf-8"?>', "</soap:Envelope>")

    tagPos <- stringr::str_locate_all(plain.text, stringr::fixed(envelopeTags))
  }

  body <- stringr::str_sub(plain.text, tagPos[[1]][1,1], tagPos[[2]][1,2])

}
