#' Scale a variable to the mean and standard deviation of another variable.
#'
#' This function scales a numeric vector (Variable1) so that its mean and
#' standard deviation match those of another numeric vector (Variable2).
#'
#' @param variable1 A numeric vector that you want to scale.
#' @param variable2 A numeric vector whose mean and standard deviation will be
#' used as the target scale for variable1.
#'
#' @return A numeric vector representing the scaled Variable1.
#' @export
#' @examples
#' # Sample data
#' set.seed(123) # for reproducibility
#' age <- rnorm(100, mean = 30, sd = 5)
#' income <- rnorm(100, mean = 75000, sd = 20000)
#'
#' # Scale age to match the scale of income
#' age_scaled <- scale_to_variable2(variable1 = age, variable2 = income)
#'
#' # Check the mean and standard deviation of the scaled variable
#' mean(age_scaled)
#' sd(age_scaled)
#'
#' # Compare with the mean and standard deviation of income
#' mean(income)
#' sd(income)

ch.scale_to_variable2 <- function(variable1, variable2) {
  # Calculate the mean and standard deviation of Variable 2
  mean_var2 <- mean(variable2, na.rm = TRUE) # Added na.rm to handle potential NAs
  sd_var2 <- sd(variable2, na.rm = TRUE)   # Added na.rm to handle potential NAs

  # Calculate the mean and standard deviation of Variable 1
  mean_var1 <- mean(variable1, na.rm = TRUE) # Added na.rm
  sd_var1 <- sd(variable1, na.rm = TRUE)   # Added na.rm

  # Apply the linear transformation to Variable 1
  scaled_variable1 <- mean_var2 + (variable1 - mean_var1) / sd_var1 * sd_var2

  return(scaled_variable1)
}