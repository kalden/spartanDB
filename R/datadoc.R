#' Latin-hypercube parameter value set to demonstrate storage of pre-generated sample file
#'
#' This is a dataset containing values for the six parameters that control the
#' simulation detailed in the case study section of the spartan vignette. These
#' parameters are defined the accompanying publications referred to in
#' the vignette
#'
#' \itemize{
#'   \item stableBindProbability. Parameter values between 0 and 100
#'   \item chemokineExpressionThreshold. Parameter values between 0 and 1
#'   \item initialChemokineExpressionValue. Parameter values between 0.1 and 0.5
#'   \item maxChemokineExpressionValue. Parameter values between 0.015 and 0.08
#'   \item maxProbabilityOfAdhesion. Parameter values between 0 and 1
#'   \item adhesionFactorExpressionSlope. Parameter values between 0.25 and 5
#' }
#'
#' @docType data
#' @keywords datasets
#' @name pregenerated_lhc
#' @usage data(pregenerated_lhc)
#' @format A list with 500 rows (one per parameter set) and six columns
NULL

#' Latin-hypercube example summary file showing parameters against simulation results
#'
#' This is a dataset containing values for the six parameters that control the
#' simulation detailed in the case study section of the spartan vignette, with their
#' responses from simulations under those conditions. These parameters are defined
#' in the accompanying publications referred to in the vignette
#'
#' \itemize{
#'   \item stableBindProbability. Parameter values between 0 and 100
#'   \item chemokineExpressionThreshold. Parameter values between 0 and 1
#'   \item initialChemokineExpressionValue. Parameter values between 0.1 and 0.5
#'   \item maxChemokineExpressionValue. Parameter values between 0.015 and 0.08
#'   \item maxProbabilityOfAdhesion. Parameter values between 0 and 1
#'   \item adhesionFactorExpressionSlope. Parameter values between 0.25 and 5
#'   \item Velocity. Simulation output for cell velocity
#'   \item Displacment. Simulation output for cell displacement
#' }
#'
#' @docType data
#' @keywords datasets
#' @name ppsim_lhc_results
#' @usage data(ppsim_lhc_results)
#' @format A list with 500 rows (one per parameter set) and eight columns
NULL

#' Robustness example parameter sample file
#'
#' This is a dataset containing values for the six parameters that control the
#' simulation detailed in the case study section of the spartan vignette, with
#' sampling performed for a robustness analysis. These parameters are defined
#' in the accompanying publications referred to in the vignette
#'
#' \itemize{
#'   \item stableBindProbability. Parameter values between 0 and 100
#'   \item chemokineExpressionThreshold. Parameter values between 0 and 1
#'   \item initialChemokineExpressionValue. Parameter values between 0.1 and 0.5
#'   \item maxChemokineExpressionValue. Parameter values between 0.015 and 0.08
#'   \item maxProbabilityOfAdhesion. Parameter values between 0 and 1
#'   \item adhesionFactorExpressionSlope. Parameter values between 0.25 and 5
#'   \item paramOfInterest. Which parameter is being considered in this robustness analysis
#' }
#'
#' @docType data
#' @keywords datasets
#' @name ppsim_robustness_set
#' @usage data(ppsim_robustness_set)
#' @format A list with 80 rows (one per parameter set) and seven columns
NULL


#' Robustness example parameter sample file, with simulation responses
#'
#' This is a dataset containing values for the six parameters that control the
#' simulation detailed in the case study section of the spartan vignette, with their
#' responses from simulations under those conditions. These parameters are defined
#' in the accompanying publications referred to in the vignette
#'
#' \itemize{
#'   \item stableBindProbability. Parameter values between 0 and 100
#'   \item chemokineExpressionThreshold. Parameter values between 0 and 1
#'   \item initialChemokineExpressionValue. Parameter values between 0.1 and 0.5
#'   \item maxChemokineExpressionValue. Parameter values between 0.015 and 0.08
#'   \item maxProbabilityOfAdhesion. Parameter values between 0 and 1
#'   \item adhesionFactorExpressionSlope. Parameter values between 0.25 and 5
#'   \item Velocity. Simulation output for cell velocity
#'   \item Displacment. Simulation output for cell displacement
#'   \item Velocity. Simulation output for cell velocity
#'   \item Displacment. Simulation output for cell displacement
#' }
#'
#' @docType data
#' @keywords datasets
#' @name ppsim_robustness_results
#' @usage data(ppsim_robustness_results)
#' @format A list with 24000 rows (one per parameter set) and eight columns
NULL

#' eFAST example parameter sample
#'
#' This is a dataset containing values for the six parameters that control the
#' simulation detailed in the case study section of the spartan vignette, with
#' sampling performed for an efast analysis. These parameters are defined
#' in the accompanying publications referred to in the vignette
#'
#' \itemize{
#'   \item stableBindProbability. Parameter values between 0 and 100
#'   \item chemokineExpressionThreshold. Parameter values between 0 and 1
#'   \item initialChemokineExpressionValue. Parameter values between 0.1 and 0.5
#'   \item maxChemokineExpressionValue. Parameter values between 0.015 and 0.08
#'   \item maxProbabilityOfAdhesion. Parameter values between 0 and 1
#'   \item adhesionFactorExpressionSlope. Parameter values between 0.25 and 5
#'   \item Velocity. Simulation output for cell velocity
#'   \item Displacment. Simulation output for cell displacement
#'   \item paramOfInterest. Which parameter is being considered in this robustness analysis
#' }
#'
#' @docType data
#' @keywords datasets
#' @name pregenerated_efast_set
#' @usage data(pregenerated_efast_set)
#' @format A 4D array with dims 65,7,7,3
NULL



