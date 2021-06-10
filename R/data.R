#' Treated mice sorted into cages
#'
#' This dataset contains the original arrival data of 40 mice (variables
#' `Box`--`ArrivalWeight`).
#' In addition, it contains optimal treatment and cage assignments generated
#' by Guido. The `Group` variable is the assigned treatment group, and 
#' `CageNr`, `CageCol`, and `CageRow` variables contain the assigned cage
#' number and its position for each animal.
#' Variables marked with an asterisk (*) are important for the
#' cage layout generation.
#'
#' @name cages
#' @author Guido Steiner
#' @docType data
#' @keywords data
#' @usage data(cages)
#'
#' @format A tibble with 40 rows and 15 variables:
#' \describe{
#' \item{Box}{Litter box}
#' \item{BoxSection}{Section of litter box}
#' \item{*Litter}{`Box` & `BoxSection`}
#' \item{*Sex}{Female (F) or Male (M)}
#' \item{Earmark}{Side (_L_ left, _R_ right, or _B_ both),
#'   and quantity (1-3, unspecified if only 1) of earmarks}
#' \item{*AnimalID}{`Sex` - `Litter` - `Earmark`}
#' \item{*BirthDate}{Date of birth (date)}
#' \item{*Genotype}{Genotype (MUT - mutant, WT - wild type)}
#' \item{Comment}{Comments about the animal (two of them are deformed)}
#' \item{*ArrivalWeight}{Weight of the animal at arrival (g)}
#' \item{*Group}{_(assigned)_ Blinded treatment group (1 or 2)}
#' \item{CageNr}{_(generated)_ Cage number}
#' \item{CageCol}{_(generated)_ Column of cage rack}
#' \item{CageRow}{_(generated)_ Row of cage rack}
#' }
"cages"

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
