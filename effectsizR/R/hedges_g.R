#' @title Calculation of the effect size Hedge's g for group comparisons
#'
#' @description <ES estimate for group designs with a total sample larger than 20, comparing
#' two independent or dependent groups: Standardised mean difference calculated by
#' using the difference between the post-test means in the numerator of the equation
#'  and the standard deviation units in the denominator (pooled SDs are used to correct for possible
#'   upwards bias as especially problematic in samples smaller than 50). Standardisation
#'    permits direct comparisons across studies using the same index of effect.>
#' @param data a list of values as returned by the significance test
#' @param N number of total participants
#' @param n_experimental number of participants in the experimental group
#' @param n_control number of participants in the control group
#' @param mean_experimental mean of the values of the experimental group
#' @param mean_control mean of the values of the control group
#' @param sd_experimental standard deviation of the values of the experimental group
#' @param sd_control standard deviation of the values of the control group
#' @return Hedge's g for group comparisons
#' @export

hedges_g <- function(data,
                     N,
                     n_experimental,
                     n_control,
                     mean_experimental,
                     mean_control,
                     sd_experimental,
                     sd_control
){
  sd_pooled <- sqrt((((sd_experimental_group)^2*(n_experimental_group-1))+
                     ((sd_control_group^2)*(n_control_group-1)))/
                     ((n_experimental_group+n_control_group)-2))
  g <- round((((mean_experimental_group-mean_control_group)/sd_pooled))*((N-3)/(N-2.25))*
   (sqrt((N-2)/N)),2)
  cat(data$data.name, "Effect Size, g = ", g)
}
