
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


#' Subject sample list with group and time plus controls
#'
#' A sample list with 9 columns as described below.
#' There are 3 types of records (rows) indicated by the `SampleType` variable.
#' Patient samples, controls and spike-in standards.
#' Patient samples were collected over up to 7 time points.
#' Controls and SpikeIns are QC samples for distribution of the samples on
#' 96 well plates.
#'
#' @docType data
#' @keywords datasets
#' @name longitudinal_subject_samples
#' @usage data(longitudinal_subject_samples)
#' @author Juliane Siebourg
#' @format An object of class \code{"tibble"}
#' \describe{
#' \item{SampleID}{A unique sample identifier.}
#' \item{SampleType}{Indicates whether the sample is a patient sample, control oder spike-in.}
#' \item{SubjectID}{The subject identifier.}
#' \item{Group}{Indicates the treatment group of a subject.}
#' \item{Week}{Sampling time points in weeks of study.}
#' \item{Sex}{Subject Sex, Female (F) or Male (M).}
#' \item{Age}{Subject age.}
#' \item{BMI}{Subject Body Mass Index.}
#' \item{SamplesPerSubject}{Look up variable for the number of samples per subject.
#' This varies as not subject have samples from all weeks.}
#' }
"longitudinal_subject_samples"


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
