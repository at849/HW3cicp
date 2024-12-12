
#' Title ggplot with data and estimates or error messages
#'
#' @param dat: tibble which contains mCPR observations. Columns: iso, year, cp
#' @param est: tibble which contains mCPR estimates. Columns: “Country or area”, iso, Year, Median, U95,L95
#' @param iso_code: country iso code
#' @param CI: confidence intervals to be plotted. Options can be: 80, 95, or NA (no CI plotted)
#'
#' @return: Either error messages or ggplot object with data and estimates
#' @export
#'
#' @examples: function(dat, est, iso_code, CI=95)
plot_cp <- function(dat, est, iso_code, CI=95) {

  if (!(iso_code %in% dat$iso)) {
    stop("Input data file dat and estimates file est must contain variable iso.")
  }
  if (!(iso_code %in% est$iso)) {
    stop("Input data file dat and estimates file est must contain variable iso.")
  }

  if (!"year" %in% names(dat)) {
    stop("Input data file must contain variable year and/or cp.")
  }

  if (!"cp" %in% names(dat)) {
    stop("Input data file must contain variable year and/or cp.")
  }

  if (!is.numeric(dat$cp)) {
    stop("Input cp in data file dat must be numeric.")
  }

  correct_CIs <- c(80, 95, NA)
  if (!(CI %in% correct_CIs)) {
    stop("CI must be 80, 95, or NA.")
  }

  data<-full_join(est, dat, by= "iso", relationship = "many-to-many")

  mydata <- data%>%
    filter(iso == iso_code)

  p<-mydata%>%
    ggplot(aes(x = Year, y=Median)) +
    geom_point(aes(x = year, y=cp), size=1)+
    geom_line(aes(x = Year, y=Median)) +
    labs(x = "Time", y = "Modern use (%)")

  for (i in seq_along(iso_code))
  {
    plot<-p+ labs(title = mydata$`Country or area`[i])
  }

  if (is.na(CI)) plot
  else if (CI==80)
    plot+geom_smooth(
      stat = "identity",
      aes(ymax = U80, ymin = L80))
  else
    plot+geom_smooth(
      stat = "identity",
      aes(ymax = U95, ymin = L95))
}
