# ' Verify and validate structure of model created by user
# '
# ' @param model SFC model object
# '
# ' @return updated equation and external values

validate_model_input <- function(model) {

  # 1 Check if there are no variables in the model
  if (is.null(model$variables)) {
    stop("List model$variables is empty
Please complete exogenous variables and initial values")
  }

  # 2 Check if there are no equations in the model
  if (is.null(model$equations)) {
    stop("List model$equations is empty
Please complete equations")
  }

  # 3 Check if provided equations do not contains invalid characters
  equations <- model$equations$equation
  for (i in equations) {
    is_invalid_name <- stringr::str_detect(i, "[!?~:;#``@$<>{}|]") # \\'[!?><,~:;#^``@$%}{|]')
    if (is_invalid_name == TRUE) {
      stop("Invalid character(s) in equation
Please check ", i)
    }
  }

  # 4 Check if variable is declared more than once
  var <- model$variables$name
  duplication <- duplicated(var)
  for (i in c(1:length(var))) {
    if (duplication[i] == TRUE) {
      stop("Variable declared more than once
Please check ", var[i])
    }
  }

  # 5 check if all provided variables are used in created equations
  all_eqs <- model$equations %>%
    dplyr::filter(hidden == FALSE)
  all_eqs <- all_eqs$equation
  all_eqs <- paste(all_eqs, collapse = " ")
  eqs_clean <- stringr::str_replace_all(all_eqs, "[-+=/*()\\[\\]]", " ")
  eqs_clean <- stringr::str_squish(eqs_clean)
  eqs_clean <- stringr::str_split(eqs_clean, " ")
  eqs_clean <- eqs_clean[[1]]
  var <- model$variables
  var <- var$name
  for (i in var) {
    if (!(i %in% eqs_clean)) {
      warning("Not all provided variables are used in equations
Please check ", i)
    }
  }

  # divide equations into rhs lhs (endogenous)
  eqs <- model$equations %>%
    dplyr::filter(hidden == FALSE)
  eqs <- eqs$equation
  eqs_separated <- tibble::tibble(eqs) %>%
    tidyr::separate(.data$eqs, c("lhs", "rhs"), "=") %>%
    dplyr::mutate(
      lhs = stringr::str_squish(lhs),
      rhs = stringr::str_squish(rhs)
    )
  eqs_separated <- eqs_separated %>%
    dplyr::mutate(rhs = godley:::add_lag_info(.data$rhs))

  # extract exogenous
  external_values <- setdiff(model$variables$name, eqs_separated$lhs)
  external_values <- model$variables %>%
    dplyr::filter(name %in% external_values) %>%
    dplyr::select(c("name", "init")) %>%
    dplyr::rename(
      lhs = name,
      rhs = init
    )

  # 6 check if there is description for all endogenous variables
  egs_splitted <- tidyr::separate(model$equations, equation, c("lhs", "rhs"), "=") %>%
    dplyr::filter(hidden == FALSE) %>%
    dplyr::mutate(
      lhs = stringr::str_squish(lhs),
      rhs = stringr::str_squish(rhs)
    )

  egs_splitted <- dplyr::filter(egs_splitted, egs_splitted$lhs %in% model$variables$name)

  # 7 check if initial values for exogenous variables are provided. If not assign by default value 0.
  if (sum(external_values$rhs == 0)) {
    d <- external_values[which(external_values$rhs == 0), ]$lhs
    message(paste(c("Exogenous variable(s):", d, "take(s) default 0 value"), collapse = " "))
  }

  # 8 check if endogenous variables are explained by the model only once
  end_v <- tidyr::separate(model$equations, equation, c("lhs", "rhs"), "=") %>%
    dplyr::filter(hidden == FALSE) %>%
    dplyr::mutate(
      lhs = stringr::str_squish(lhs),
      rhs = stringr::str_squish(rhs)
    )

  end_v <- end_v$lhs
  len_endo <- length(end_v[duplicated(end_v)])

  if (len_endo > 0) {
    stop(paste(c("Endogenous variable(s):", end_v[duplicated(end_v)], "is/ are explained by the model more than once
Please remove one of the equations"), collapse = " "))
  }

  # 9 check if all variables from equations are defined
  all_eqs <- model$equations %>%
    dplyr::filter(hidden == FALSE)
  all_eqs <- all_eqs$equation
  all_eqs <- paste(all_eqs, collapse = " ")
  eqs_clean <- stringr::str_replace_all(all_eqs, "[-+=/*()\\[\\]]", " ")
  eqs_clean <- stringr::str_squish(eqs_clean)
  eqs_clean <- stringr::str_split(eqs_clean, " ")
  eqs_clean <- unique(eqs_clean[[1]])
  eqs_clean <- eqs_clean[!stringr::str_detect(eqs_clean, "^[:digit:]+$")]

  var <- model$variables$name

  for (i in eqs_clean) {
    if (!(i %in% var)) {
      warning("Not all variables used in equations are defined
Please check ", i)
    }
  }

  # 10 Check if provided equations do not contains a comma
  equations <- model$equations$equation
  for (i in equations) {
    is_invalid_name <- stringr::str_detect(i, "[,]")
    if (is_invalid_name == TRUE) {
      warning("Comma(s) found in equation
Please check ", i)
    }
  }

  return(list(eqs_separated, external_values))
}
