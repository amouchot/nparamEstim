#' Title
#'Fonction faisant la moyenne d'un vecteur
#' @param x vecteur numérique
#'
#' @return moyenne du vecteur
#' @import FactoMineR
#' @export
#'
#' @examples
#'moyenne(c(1, 2, 3, 4))
moyenne = function(x){
  sum(x)/length(x)
}

