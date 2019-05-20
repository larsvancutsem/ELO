#' @title Calculate ELO Ratings
#' @description This function calculates dynamic performance ratings
#' called "ELO ratings"
#' @usage calculate_elo_ratings(teams, outcomes, lambda, delta, k0, c, d, return_e)
#' @importFrom methods hasArg
#' @param teams an (n x 2) character matrix,
#' contains unique names for the respective home and away teams in n
#' subsequent matches
#' @param outcomes an (n x 2) numeric matrix,
#' contains the points that the respective home and away teams scored in n
#' subsequent matches
#' @param lambda a parameter tb optimized default = 1
#' @param delta a parameter tb optimized default = 0.05
#' @param k0 a constant for scaling default = 10
#' @param c a constant for scaling default = 10
#' @param d a constant for scaling default = 400
#' @param return_e a boolean variable, conditions the function
#' to return either the mean squared error when return_e = TRUE,
#' or the elo when return_e = FALSE, default value: FALSE
#' @return either an (n x 2) matrix containing the elo ratings for the teams in
#' the n input matches or the mean squared error for the specific parameter
#' setting, conditional on boolean parameter return_e being FALSE or TRUE
#' @examples
#' # toy example
#' teams <- matrix(c("team A", "team B", "team B", "team A"), nrow = 2)
#' outcomes <- matrix(c(1, 3, 2, 1), nrow = 2)
#' calculate_elo_ratings(teams, outcomes)
#' @export


calculate_elo_ratings <- function(teams = NULL, outcomes= NULL, lambda = 1, delta = 0.05, k0 = 1, c = 10, d = 400, return_e = FALSE) {


  # ================================================================================
  # check requirements for calculation
  # ================================================================================


  # check requirements


  # requirements 'teams'
  if (!hasArg(teams)) {
    stop("'teams' is required to perform the calculation")
  } else if (!is.matrix(teams) | !is.character(teams)) {
    stop("'teams' is required to be a character matrix")
  } else if (dim(teams)[2] != 2) {
    stop("the dimensions of 'teams' are required to be n x 2")
  }

  # requirements 'outcomes'
  if (!hasArg(outcomes)) {
    stop("'outcomes' is required to perform the calculation")
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

  # requirements 'lambda'
  if (hasArg(lambda) & (!is.numeric(lambda) | (is.numeric(lambda) & length(lambda) != 1))) {
    stop("'lambda' is required to be a real number")
  }

  # requirements 'delta'
  if (hasArg(detlta) & (!is.numeric(delta) | (is.numeric(delta) & length(delta) != 1))) {
    stop("'delta' is required to be a real number")
  }

  # requirements 'k0'
  if (hasArg(k0) & (!is.numeric(k0) | (is.numeric(k0) & length(k0) != 1))) {
    stop("'k0' is required to be a real number")
  }

  # requirements 'c'
  if (hasArg(c) & (!is.numeric(c) | (is.numeric(c) & length(c) != 1))) {
    stop("'c' is required to be a real number")
  }

  # requirements 'd'
  if (hasArg(d) & (!is.numeric(d) | (is.numeric(d) & length(d) != 1))) {
    stop("'d' is required to be a real number")
  }


  # ================================================================================
  # create local variables and calculate elo ratings
  # ================================================================================



  amount_matches <- length(outcomes[, 1])

  elo_ratings <- matrix(0, ncol = 2, nrow = amount_matches)

  team_list <- as.list(unique(c(teams[, 1], teams[, 2])))
  for (team in team_list) {
    team_list[[team]] <- list(0)
  }

  abs_error <- list()

  # calculate elo ratings


  for (i in 1:amount_matches) {

    # retrieve match data
    home_team <- teams[i, 1]
    away_team <- teams[i, 2]
    score_diff <- outcomes[i, 1] - outcomes[i, 2]

    # retrieve ratings of respective teams
    H_list <- team_list[[home_team]]
    A_list <- team_list[[away_team]]

    # retrieve most recent ratings of respective teams
    team_rate_H <- H_list[[length(H_list)]]
    team_rate_A <- A_list[[length(A_list)]]

    # input pi ratings into matrix
    elo_ratings[i, 1] <- team_rate_H
    elo_ratings[i, 2] <- team_rate_A

    # calculate expected score
    e_score_H <- 1 / (1 + c^((team_rate_A - team_rate_H)/d))
    e_score_A <- 1 - e_score_H

    # calculate actual score
    score_H <- ifelse(score_diff > 0, 1, ifelse(score_diff < 0, 0, 0.5))
    score_A <- 1 - score_H

    # store absolute error in list of absolute errors
    abs_error[[i]] <- abs(score_H - e_score_H) + abs(score_A - e_score_A)

    # update ratings
    team_list[[home_team]] <- append(H_list, team_rate_H + (k0 * (1 + delta)^lambda) * (score_H - e_score_H))
    team_list[[away_team]] <- append(A_list, team_rate_A + (k0 * (1 + delta)^lambda) * (score_A - e_score_A))

  }


  # return either matrix with pi ratings or
  if (!return_e) {
    return(elo_ratings)
  } else {
    mean_sq_e <- mean(unlist(abs_error)^2)
    return(mean_sq_e)
  }
}
