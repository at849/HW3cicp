
#' Title Confidence intervals width with estimates
#'
#' @param est: tibble which contains mCPR estimates. Columns: “Country or area”, iso, Year, Median, U95,L95
#' @param iso_code: country iso code
#' @param coverage: width of intervals to be calculated. Options can be: 80, 95, or NA
#'
#' @return: a tibble with Year and interval widths
#' @export
#'
#' @examples: function(est,iso_code , coverage = 95)
get_width_ci <- function(est,iso_code , coverage = 95) {
  data <- est%>%
    filter(iso == iso_code)%>%
    mutate(ci_width = if (coverage==95) U95-L95 else U80-L80)%>%
    select(Year, ci_width)
  print(data)
}
