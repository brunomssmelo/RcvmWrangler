#' Funcao que realiza o download de algum arquivo que tenha sido previamente liberado pelo
#' webservice da CVM.
#'
#' @param autorizDownload data.frame retornado por alguma funcao do tipo cvmSolicAUtorizDownload*
#' @param nomeArq nome do arquivo de destino
#' @author Bruno M. S. S. Melo <ctbrunomelo@gmail.com>
#' @details
#' Esta funcao realiza download de algum arquivo que ja tenha sido liberado pela CVM por meio
#' de alguma funcao do tipo "cvmSolicAUtorizDownload*".
#' @examples
#' \dontrun{
#' cvmDownload(autorizDownload)
#' }
#' @seealso \url{http://sistemas.cvm.gov.br}
#' @export
cvmDownload <- function(autorizDownload, nomeArq = 'cvm.zip') {

  dfAutorizacaoDownload <- autorizDownload

  url <- dfAutorizacaoDownload[1,1]

  curl = RCurl::getCurlHandle()
  bfile = RCurl::getBinaryURL (
    url = url,
    curl= curl,
    progressfunction = function(down, up) {print(down)}, noprogress = FALSE
  )

  writeBin(bfile, nomeArq)
}

