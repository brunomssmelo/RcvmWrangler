% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cvmSolicAutorizDownloadArqAnualInformes.R
\name{cvmSolicAutorizDownloadArqAnualInformes}
\alias{cvmSolicAutorizDownloadArqAnualInformes}
\title{Solicita autorizacao para o download dos informes diarios dos ultimos 12 meses.}
\usage{
cvmSolicAutorizDownloadArqAnualInformes(loginInfo)
}
\arguments{
\item{loginInfo}{data.frame retornado pela funcao cvmLogin, contendo informacoes de login.}
}
\value{
um objeto do tipo data.frame contendo a url para efetuar o download solicitado.
}
\description{
Solicita autorizacao para o download dos informes diarios dos ultimos 12 meses.
}
\details{
Esta funcao solicita autorizacao pala efetuar o download dos informes diarios dos ultimos
doze meses.
}
\examples{
\dontrun{
dfAutorizacao <- cvmSolicAutorizDownloadArqAnualInformes(loginInfo = dfLogin)
}
}
\author{
Bruno M. S. S. Melo <ctbrunomelo@gmail.com>
}
\seealso{
\url{http://sistemas.cvm.gov.br}
}

