#' @title Optimize Elo Ratings
#' @description This function performs grid optimization on a prespecified set
#' of parameters to find the optimal learning rates for calculating
#' the elo ratings
#' @usage optimize_elo_ratings(teams, outcomes, lambda_in, delta_in, k0_in, c_in, d_in)
#' @importFrom methods hasArg
#' @param teams an (n x 2) character matrix,
#' contains unique names for the respective home and away teams in n
#' subsequent matches
#' @param outcomes an (n x 2) numeric matrix,
#' contains the points that the respective home and away teams scored in n
#' subsequent matches
#' @param lambda_in a numerical vector, learning rate values to consider in
#' the grid optimization, default value: seq(0, 3, 0.25)
#' @param k0_in a constant, eefault value: seq(1, 10, 1)
#' @param b_in a constant, logarithmic base, default value: 10
#' @param d_in a constant, default value: 400
#' @return a dataframe with the results of the grid optimization,
#' the mean squared error for every combination of parameter sets
#' lambda and delta specified in the parameter vectors
#' @examples
#' # toy example
#' teams <- matrix(c("team A", "team B", "team B", "team A"), nrow = 2)
#' outcomes <- matrix(c(1, 3, 2, 1), nrow = 2)
#' optimize_elo_ratings(teams, outcomes, seq(1, 2, by = 0.25), seq(0.05, 0.55, by = 0.05))
#' @export


optimize_elo_ratings <- function(teams = NULL, outcomes = NULL, lambda_in = seq(0, 3, 0.25),
                                 k0_in = seq(1, 10, 1), c_in = 10, d_in = 400) {


  # ================================================================================
  # check requirements for calculation
  # ================================================================================


  # check requirements


  # requirements 'teams'
  if (!hasArg(teams)) {
    stop("'teams' is required to perform the optimization")
  } else if (!is.matrix(teams) | !is.character(teams)) {
    stop("'teams' is required to be a character matrix")
  } else if (dim(teams)[2] != 2) {
    stop("the dimensions of 'teams' are required to be n x 2")
  }

  # requirements 'outcomes'
  if (!hasArg(outcomes)) {
    stop("'outcomes' is required to perform the optimization")
  } else if (!is.matrix(outcomes) | !is.numeric(outcomes)) {
    stop("'outcomes' is required to be a numeric matrix")
  } else if (dim(outcomes)[2] != 2) {
    stop("the dimensions of 'outcomes' are required to be n x 2")
  }

  # combined requirements 'teams' and 'outcomes'
  if ((hasArg(teams) & hasArg(outcomes)) & is.matrix(teams) & is.matrix(outcomes)) {
    if (dim(teams)[1] != dim(outcomes)[1]) {
      stop("the dimensions of 'teams' and 'outcomes' need to be identical")
    }
  }

  # requirements 'lambda_in'
  if (hasArg(lambda_in) & (!is.numeric(lambda_in) |
                           (is.numeric(lambda_in) & is.matrix(lambda_in)))) {
    stop("'lambda_in' is required to be a vector or a real number")
  }

  # requirements 'k0'
  if (hasArg(k0) & (!is.numeric(k0) |
                           (is.numeric(k0) & is.matrix(k0)))) {
    stop("'k0' is required to be a vector or a real number")
  }

  # requirements 'c_in'
  if (hasArg(c_in) & (!is.numeric(c_in) |
                      (is.numeric(c_in) & length(c_in) != 1))) {
    stop("'c_in' is required to be a real number")
  }

  # requirements 'd_in'
  if (hasArg(d_in) & (!is.numeric(d_in) |
                      (is.numeric(d_in) & length(d_in) != 1))) {
    stop("'d_in' is required to be a real number")
  }


  # ================================================================================
  # create local variables and perform grid optimization
  # ================================================================================


  lambda_seq_l <- length(lambda_in)

  k0_seq_l <- length(k0_in)

  avg_sq_e <- matrix(0, nrow = lambda_seq_l, ncol = k0_seq_l)


  # perform grid optimization


  for (i1 in 1:lambda_seq_l) {
    for (i2 in 1:k0_seq_l) {
      current_lambda <- lambda_in[i1]
      current_k0 <- k0_in[i2]
      result <- calculate_elo_ratings(teams = teams,
                                     outcomes = outcomes,
                                     lambda = current_lambda,
                                     k0 = k0_in,
                                     c = c_in, d = d_in, return_e = TRUE)
      avg_sq_e[i1, i2] <- result
      message(paste0("calculated result for ",
                     "lambda = ", current_lambda,
                     ", ",
                     "k0 = ", current_k0))
    }
  }


  # prepare result
  result <- data.frame("mean squared error" = as.vector(avg_sq_e),
                       "lambda" = lambda_in[rep(1:lambda_seq_l, delta_seq_l)],
                       "k0" = k0_in[rep(1:k0_seq_l,
                                              lambda_seq_l)[order(rep(1:k0_seq_l,
                                                                      lambda_seq_l))]])

  return(result)
}
