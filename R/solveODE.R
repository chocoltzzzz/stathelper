#' Solve Second-Order Ordinary Differential Equations
#'
#' This function solves second-order ordinary differential equations (ODEs) of the form:
#' \deqn{a y'' + b y' + c y = f(x)}
#' It computes both the complementary solution (\eqn{y_c}) and the particular solution (\eqn{y_p}) if a forcing function \eqn{f(x)} is provided.
#'
#' @param a Numeric. The coefficient of the second derivative term (\eqn{y''}).
#' @param b Numeric. The coefficient of the first derivative term (\eqn{y'}).
#' @param c Numeric. The coefficient of the function term (\eqn{y}).
#' @param f Character or numeric. The forcing function \eqn{f(x)}. Can be:
#'   - \code{NULL} for a homogeneous equation.
#'   - A constant numeric value.
#'   - A string representing \eqn{f(x)} in forms like "x", "exp(kx)", "kx", or "sin(kx)".
#'
#' @return A list containing:
#'   \describe{
#'     \item{\code{Complementary_Solution}}{Character. The complementary solution \eqn{y_c}.}
#'     \item{\code{Particular_Solution}}{Character. The particular solution \eqn{y_p}, or \code{"Unsupported forcing function"} if \eqn{f(x)} is invalid.}
#'   }
#'
#' @details
#' The function determines the complementary solution based on the discriminant of the characteristic equation:
#' \deqn{r^2 + \frac{b}{a}r + \frac{c}{a} = 0.}
#' - If the discriminant is positive, it provides two real roots (\eqn{r_1} and \eqn{r_2}).
#' - If zero, it provides one repeated root (\eqn{r}).
#' - If negative, it provides complex roots.
#'
#' For the particular solution, the function attempts to match the forcing function \eqn{f(x)} with common forms (constant, polynomial, exponential, sinusoidal) and calculates \eqn{y_p}.
#'
#' @examples
#' # Homogeneous equation
#' solveODE(1, 2, 1)
#'
#' # Constant forcing function
#' solveODE(1, -3, 2, f = 5)
#'
#' # Polynomial forcing function
#' solveODE(1, 3, 2, f = "3x")
#'
#' # Exponential forcing function
#' solveODE(1, -2, 2, f = "2exp(3x)")
#'
#' # Sinusoidal forcing function
#' solveODE(1, 0, 4, f = "sin(2x)")
#'
#' @export

solveODE <- function(a, b, c, f = NULL) {
  discriminant <- b^2 - 4 * a * c
  if (discriminant > 0) {
    r1 <- (-b + sqrt(discriminant)) / (2 * a)
    r2 <- (-b - sqrt(discriminant)) / (2 * a)
    yc <- paste0("C1 * exp(", round(r1, 2), " * x) + C2 * exp(", round(r2, 2), " * x)")
  } else if (discriminant == 0) {
    r <- -b / (2 * a)
    yc <- paste0("C1 * exp(", round(r, 2), " * x) + C2 * x * exp(", round(r, 2), " * x)")
  } else {

    real_part <- -b / (2 * a)
    imag_part <- sqrt(-discriminant) / (2 * a)
    yc <- paste0("exp(", round(real_part, 2), " * x) * (C1 * cos(",
                 round(imag_part, 2), " * x) + C2 * sin(", round(imag_part, 2), " * x))")
  }

  yp <- NULL
  if (!is.null(f)) {
    if (is.numeric(f)) {
      B <- f / c
      yp <- as.character(round(B, 2))
    } else if (grepl("x", f) && !grepl("exp", f)) {
      coeff <- as.numeric(sub("x", "", f))
      A <- coeff / c
      B <- -(b * A) / c
      yp <- paste0(round(A, 2), " * x + ", round(B, 2))
    } else if (grepl("exp", f)) {
      matches <- regmatches(f, regexec("([0-9.]+)exp\\^?\\(([0-9.-]+)x\\)", f))
      if (length(matches[[1]]) > 2) {
        k <- as.numeric(matches[[1]][3])
        const <- as.numeric(matches[[1]][2])
        denom <- a * k^2 + b * k + c
        A <- const / denom
        yp <- paste0(round(A, 2), " * exp(", k, " * x)")
      } else {
        yp <- "Invalid exponential forcing function format"
      }
    } else if (grepl("sin|cos", f)) {
      k <- as.numeric(sub(".*\\(", "", sub("\\).*", "", f)))
      denom <- a * k^2 + c
      A <- 1 / denom
      yp <- paste0(round(A, 2), " * sin(", k, " * x)")
    } else {
      yp <- "Unsupported forcing function"
    }
  } else {
    yp <- "0 (homogeneous equation)"
  }

  list(
    Complementary_Solution = yc,
    Particular_Solution = yp
  )
}
