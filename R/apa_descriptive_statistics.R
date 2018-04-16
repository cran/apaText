#' Creates Mean (M) and Standard Deviation (SD) output with confidence interval in APA style using Markdown.
#' @param dv Name of the dependent variable column
#' @param data Project data frame name
#' @param show.conf.interval Show confidence interval (default behavior is TRUE)
#' @param show.N Show confidence interval (default behavior is TRUE)
#' @param ... Tidyverse selection of rows
#' @return Descriptive statistics (M and SD) in APA style using Markdown
#' @examples
#' #2-way ANOVA Example
#' library(apaTables) #load apaTables to access goggles
#'
#' #Main Effect Means: Gender
#' apa.desc(goggles, gender=="Female", dv = attractiveness)
#' apa.desc(goggles, gender=="Male", dv = attractiveness)
#'
#' #Main Effect Means: Alcohol
#' apa.desc(goggles, alcohol=="None", dv = attractiveness)
#' apa.desc(goggles, alcohol=="2 Pints", dv = attractiveness)
#' apa.desc(goggles, alcohol=="4 Pints", dv = attractiveness)
#'
#' #Cell Mean: Female, 2 Pints
#' apa.desc(goggles, alcohol=="2 Pints", gender=="Female", dv = attractiveness)
#' @export
apa.desc <- function(data = NULL, ..., dv = NULL, show.conf.interval = NULL, show.N = NULL) {
  if (is.null(show.conf.interval)==TRUE) {
    show.conf.interval <- TRUE
  }

  if (is.null(show.N)==TRUE) {
    show.N <- TRUE
  }

  dv <- dplyr::enquo(dv)
  row_selection_criteria <- dplyr::quos(...)

  data_column_frame <- dplyr::filter(data, !!! row_selection_criteria)
  data_column_frame <- dplyr::select(data_column_frame, !!! dv)
  data_column_frame <- stats::na.omit(data_column_frame) #only one column

  group_data <- data_column_frame[,1]

  N <- length(group_data)
  group_m <- mean(group_data,na.rm = TRUE)
  group_sd <- stats::sd(group_data,na.rm = TRUE)
  group_se <- group_sd/sqrt(N)
  group_df <- N - 1
  ci_t <-stats::qt(.975, df=group_df) #95% CI
  LL <-group_m - ci_t *  group_se
  UL <-group_m + ci_t *  group_se

  #make markdown output
  out_str <- sprintf("*M* = %1.2f, *SD* = %1.2f",group_m,group_sd)

  #make markdown output
  if (show.conf.interval == TRUE) {
    out_str <- sprintf("%s, 95%% CI[%1.2f, %1.2f]", out_str, LL, UL)
  }

  if (show.N == TRUE) {
    out_str <- sprintf("%s, *N* = %g", out_str, N)
  }

  return(out_str)
}
