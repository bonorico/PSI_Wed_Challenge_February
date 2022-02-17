#' Loss function for the optimization problem
#' @param tot_dur - scalar. Total study duration (accrual time + follow-up
#'    time) in months.
#' @param accr_time - scalar. Accrual time in months.
#' @param h_c - scalar. Value of hazard rate in control arm.
#' @param HR - scalar. Hazard ratio.
#' @param given_N - integer. Fixed sample size.
#' @detail To avoid singular values a constraint "total time greater equal
#'     than accrual time" must be enforced.
#' @importFrom gsDesign nSurvival
#' @export
#' @returns numeric

# objective function
loss_function <- function(tot_dur, accr_time = 24, h_c = 0.05/12, HR = 0.775,
                        given_N = 10157)
{

  if (tot_dur <= accr_time)  # set a constraint
    out <- 1e+200
  else
  {
    res <- nSurvival(lambda1 =h_c, lambda2 =h_c*HR,
                     Ts = tot_dur, Tr = accr_time,
                     alpha = 0.05, sided = 2)

    out <- abs(round(res$n, 0) - given_N)  # absolute value --> minimum at 0

  }

  return(out)
}

# test
# to_minimize(24)
