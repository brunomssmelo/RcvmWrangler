#' Funcao que carrega para um data.frame os informes diarios de fundos a partir de arquivo
#' zipado contendo os xmls referentes aos mesmos.
#'
#' @param nomeArq nome do arquivo zipado que contem os xml relativos aos informes diarios.
#' @param listaFundos vetor de caracters contendo os CNPJ dos fundos dos quais deseja-se extrair as informacoes
#' @return objeto do tipo data.frame contendo os informes diarios dos fundos.
#' @author Bruno M. S. S. Melo <ctbrunomelo@gmail.com>
#' @details
#' Esta funcao carrega para um data.frame os informes diarios de fundos a partir de arquivo
#' zipado contendo os xmls referentes aos mesmos.
#' @examples
#' \dontrun{
#' dfInformes <- cvmCarregaArqAnualInformes(nomeArq, listaFundos)
#' }
#' @seealso \url{http://sistemas.cvm.gov.br}
#' @export
cvmCarregaArqAnualInformes <- function(nomeArq, listaFundos = NULL) {

  message(paste0(paste0("Abrindo arquivo \"", nomeArq), "\" ..."))
  dirTmp <- "./tmp_"
  unzip(zipfile = nomeArq, exdir = dirTmp)
  message("Concluido")
  message("Processando informes diarios ...")

  dirMain <- getwd()
  setwd(dirTmp)
  lsFiles <- list.files()

  # progress bar
  pb <- txtProgressBar(min = 1,
                       max = length(lsFiles),
                       initial = 1)

  dfInforme <- data.frame()
  for (i in 1:length(lsFiles)) {

    setTxtProgressBar(pb, i)

    try({

      xmlFile <- lsFiles[i]
      xmlParsed <- XML::xmlParse(xmlFile)

      if (is.null(listaFundos)) {
        xmlPath <- "/ROOT/INFORMES/INFORME_DIARIO"
      } else {
        xmlPath <- paste0(paste(rep("/ROOT/INFORMES/INFORME_DIARIO[CNPJ_FDO =", length(listaFundos)),
                                listaFundos, collapse = "] | "), "]")
      }

      xmlNodeSet <- XML::getNodeSet(xmlParsed, xmlPath)

      xmlInfo <- XML::xmlSApply(xmlNodeSet, function(x) XML::xmlSApply(x, XML::xmlValue))

      dfInformeCorrente <- data.frame(t(xmlInfo),
                                      row.names = F,
                                      stringsAsFactors = F)



      dfInforme <- rbind(
        dfInforme,
        dfInformeCorrente
      )
    }, silent = T)
  }

  close(pb)

  setwd(dirMain)
  
  dfInforme$VL_TOTAL <- as.numeric(sub(",", ".", dfInforme$VL_TOTAL))
  dfInforme$VL_QUOTA <- as.numeric(sub(",", ".", dfInforme$VL_QUOTA))
  dfInforme$PATRIM_LIQ <- as.numeric(sub(",", ".", dfInforme$PATRIM_LIQ))
  dfInforme$CAPTC_DIA <- as.numeric(sub(",", ".", dfInforme$CAPTC_DIA))
  dfInforme$RESG_DIA <- as.numeric(sub(",", ".", dfInforme$RESG_DIA))
  dfInforme$DT_COMPTC <- as.Date(dfInforme$DT_COMPTC)

  unlink(dirTmp, recursive=TRUE)

  message("Concluido")

  dfInforme
}
