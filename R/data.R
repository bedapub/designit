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


#' A sample list from an in vivo experiment with multiple treatments and 2 strains
#'
#' This sample list is intended to be used in connection with the \code{"invivo_study_treatments"} data object
#'
#' @docType data
#' @keywords datasets
#' @name invivo_study_samples
#' @usage data(invivo_study_samples)
#' @author Guido Steiner
#'
#' @format An object of class \code{"tibble"}
#' \describe{
#' \item{AnimalID}{The animal IDs, i.e. unique identifiers for each animal}
#' \item{Strain}{Strain (A or B)}
#' \item{Sex}{Female (F) or Male (M)}
#' \item{BirthDate}{Date of birth, not available for all the animals}
#' \item{Earmark}{Markings to distinguish individual animals, applied on the left (L), right (R) or both(B) ears}
#' \item{ArrivalWeight}{Initial body weight of the animal}
#' \item{Arrival weight Unit}{Unit of the body weight, here: grams}
#' \item{Litter}{The litter IDs, grouping offspring from one set of parents}
#' }
"invivo_study_samples"


#' A treatment list together with additional constraints on the strain and sex of animals
#'
#' This treatment list is intended to be used in connection with the \code{"invivo_study_samples"} data object
#'
#' @docType data
#' @keywords datasets
#' @name invivo_study_treatments
#' @usage data(invivo_study_treatments)
#' @author Guido Steiner
#'
#' @format An object of class \code{"tibble"}
#' \describe{
#' \item{Treatment}{The treatment to be given to an individual animal (1-3, plus a few untreated cases)}
#' \item{Strain}{Strain (A or B) - a constraint which kind of animal may receive the respective treatment}
#' \item{Sex}{Female (F) or Male (M) - a constraint which kind of animal may receive the respective treatment}
#' }
"invivo_study_treatments"

#' Example dataset with a plate effect
#'
#' Here top and bottom row were both used as controls (in dilutions). The top
#' row however was affected differently than the bottom one. This makes
#' normalization virtually impossible.
#'
#' @docType data
#' @keywords datasets
#' @name plate_effect_example
#' @usage data(plate_effect_example)
#' @author Balazs Banfai
#'
#' @format An object of class \code{"tibble"}
#' \describe{
#' \item{row}{Plate row}
#' \item{column}{Plate column}
#' \item{conc}{Sample concentration}
#' \item{log_conc}{Logarithm of sample concentration}
#' \item{treatment}{Sample treatment}
#' \item{readout}{Readout from experiment}
#' }
"plate_effect_example"
