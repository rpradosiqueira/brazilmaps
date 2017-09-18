#' Population estimates (2017), city level
#'
#' @description IBGE's population estimates by municipality for 2017. \cr\cr
#'   The data is formatted for easy merging with output from \code{\link[brazilmaps]{get_brmap}}.
#'
#' @usage data(pop2017)
#'
#' @details
#' \itemize{
#'   \item \code{mun} The 7-digit code corresponding to the city.
#'   \item \code{nome_mun} The city name.
#'   \item \code{pop2017} The 2017 population estimate (in number of people)
#'     for the corresponding city
#' }
#'
#' @name pop2017
#' @format A data frame with 5570 rows and 3 variables.
#' @docType data
#' @references
#'   \itemize{
#'     \item \url{http://www.ibge.gov.br/home/estatistica/populacao/estimativa2017/default.shtm}
#'     \item \url{http://www.ibge.gov.br/home/estatistica/populacao/estimativa2017/estimativa_dou.shtm}
#'   }
#' @keywords data
"pop2017"


#' Gini index (2015), state level
#'
#' @description IBGE's Gini index of the monthly income distribution of persons 15 years of age
#'   or over, with income for 2015. \cr\cr
#'   The data is formatted for easy merging with output from \code{\link[brazilmaps]{get_brmap}}.
#'
#' @usage data(gini2015)
#'
#' @details
#' \itemize{
#'   \item \code{cod} The 2-digit code corresponding to the state.
#'   \item \code{uf} The state name.
#'   \item \code{gini} The 2015 Gini Index
#' }
#'
#' @name gini2015
#' @format A data frame with 27 rows and 3 variables.
#' @docType data
#' @references
#'   \itemize{
#'     \item \url{http://www.ibge.gov.br/home/estatistica/pesquisas/pesquisa_resultados.php?id_pesquisa=40}
#'   }
#' @keywords data
"gini2015"


#' Number of deaths (2015), microregion level
#'
#' @description DATASUS' registry of deaths from IBGE's brazilian microregions for 2015. \cr\cr
#'   The data is formatted for easy merging with output from \code{\link[brazilmaps]{get_brmap}}.
#'
#' @usage data(deaths)
#'
#' @details
#' \itemize{
#'   \item \code{cod} The 5-digit code corresponding to the microregion.
#'   \item \code{micro} The microregion name.
#'   \item \code{ndeaths} The 2015 absolut number of deaths
#' }
#'
#' @name deaths
#' @format A data frame with 580 rows and 3 variables.
#' @docType data
#' @references
#'   \itemize{
#'     \item \url{http://datasus.saude.gov.br/informacoes-de-saude/tabnet}
#'   }
#' @keywords data
"deaths"
