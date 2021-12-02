
#' Unbalanced treatment and time sample list
#'
#' A sample list with 4 columns SampleName, Well, Time and Treatment
#' Not all treatments are avaliable at all time points.
#' All samples are placed on the same plate.
#'
#' @docType data
#' @keywords datasets
#' @name multi_trt_day_samples
#' @usage data(multi_trt_day_samples)
#' @format An object of class \code{"tibble"}
#'
#' @author siebourj
"multi_trt_day_samples"


#' An artificial invivo sample list
#'
#' An animal sample list with 7 columns.
#'
#' @docType data
#' @keywords datasets
#' @name invivo_samples
#' @usage data(invivo_samples)
#' @author Guido Steiner
#'
#' @format An object of class \code{"tibble"}
#' \describe{
#' \item{Study}{Study ID}
#' \item{AnimalID}{The animal IDs}
#' \item{Sex}{Female (F) or Male (M)}
#' \item{Genotype}{Genotype (CAR - mutant, WT - wild type)}
#' \item{Bodyeight}{Weight of the anima (g)}
#' \item{Litter}{The litter IDs}
#' \item{Earmark}{Side (_L_ left, _R_ right, or _B_ both),
#'   and quantity (1-3, unspecified if only 1) of earmarks}
#' \item{Litter_alloc}{Litter with which the animal can be allocated}
#' }
"invivo_samples"
