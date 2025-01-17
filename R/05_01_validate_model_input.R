#' Verify if parentheses in the equation are properly balanced
#'
#' This function checks if all parentheses in a given equation string are correctly
#' paired and balanced. If there are no parentheses, the equation is considered
#' trivially balanced. It returns `TRUE` if the parentheses are balanced and `FALSE` otherwise.
#'
#' @param eq A character string representing an equation.
#'
#' @return A logical value indicating whether the parentheses are balanced (`TRUE`) or not (`FALSE`).
#' @keywords internal
check_parentheses_balance <- function(eq) {
  # Split the equation into individual characters
  chars <- strsplit(eq, "")[[1]]

  # Filter out only parentheses characters
  parens <- chars[chars %in% c("(", ")")]

  # If there are no parentheses, the equation is trivially balanced
  if (length(parens) == 0) {
    return(TRUE)
  }

  # Map opening parentheses to +1 and closing parentheses to -1
  vals <- ifelse(parens == "(", 1, -1)

  # Compute the cumulative sum of the parentheses values
  cs <- cumsum(vals)

  # The equation is balanced if:
  # 1) The cumulative sum never becomes negative.
  # 2) The final cumulative sum returns to zero.
  all(cs >= 0) && tail(cs, 1) == 0
}


# ' Verify and validate structure of model created by user
# '
# ' @param model SFC model object
# '
# ' @return updated equation and external values

validate_model_input <- function(model, verbose) {
  # 1 Check if there are no equations in the model
  if (is.null(model$equations)) {
    stop("List model$equations is empty
Please complete equations
")
  }

  # 2 Check if there are no variables in the model
  if (is.null(model$variables)) {
    stop("List model$variables is empty
Please complete exogenous variables and initial values
")
  }

  # 3 Check if provided equations do not contain invalid characters
  all_equations <- model$equations$equation
  for (i in all_equations) {
    if (stringr::str_detect(i, "[\u00A7\u00A3@#\\${};:'\\\\~?]")) {
      stop("Invalid character(s) in equation. Please check: ", i, "")
    }
  }

  # 4. Verify that the equations are properly balanced in terms of parentheses
  for (eq in all_equations) {
    if (!check_parentheses_balance(eq)) {
      stop("\nParentheses not balanced in equation:\n", eq, "\n")
    }
  }

  # 5. Check if equation is declared more than once
  duplication <- duplicated(all_equations)
  for (i in c(1:length(all_equations))) {
    if (duplication[i] == TRUE) {
      stop("Equation declared more than once
Please check: ", all_equations[i], "
")
    }
  }

  # 6. Check if variable is declared more than once
  variables_user <- model$variables$name
  duplication <- duplicated(variables_user)
  for (i in c(1:length(variables_user))) {
    if (duplication[i] == TRUE) {
      stop("Variable declared more than once
Please check: ", variables_user[i], "
")
    }
  }

  ### define sets
  ## equations
  # all equations
  all_equations <- model$equations$equation
  # not-hidden equations
  equations <- model$equations %>%
    dplyr::filter(hidden == FALSE) %>%
    dplyr::pull(equation)
  # not-hidden equations in one string
  equations_glued <- paste(equations, collapse = " ")
  # functions
  functions <- stringr::str_replace_all(equations_glued, "[!%^&*)\\-+=\\[\\]|<,>/]", " ") %>%
    stringr::str_replace_all("[(]", "( ") %>%
    stringr::str_squish() %>%
    stringr::str_split(" ", simplify = T) %>%
    grep("\\(", ., value = T) %>%
    stringr::str_replace_all("[(]", "")
  # equations divided into lhs and rhs
  equations_sep <- tibble::tibble(equations) %>%
    tidyr::separate(.data$equations, c("lhs", "rhs"), "=") %>%
    dplyr::mutate(
      lhs = stringr::str_squish(lhs),
      rhs = stringr::str_squish(rhs)
    )
  # equations rhs
  equations_rhs <- equations_sep$rhs

  ## variables
  # user defined variables
  variables_user <- model$variables$name
  # variables from visible equations
  variables_eqs <- stringr::str_replace_all(equations_glued, "[!%^&*()\\-+=\\[\\]|<,>/]", " ") %>%
    stringr::str_squish() %>%
    stringr::str_split(" ", simplify = T) %>%
    vecsets::vsetdiff(functions) %>%
    unique()
  variables_eqs <- variables_eqs[suppressWarnings(is.na(as.numeric(variables_eqs)))]
  # variables not-defined by the user but present in equations
  variables_not_user <- vecsets::vsetdiff(variables_eqs, variables_user)
  # final variable list
  variables <- c(variables_user, variables_not_user)
  # endogenous variables
  variables_endo <- equations_sep$lhs
  # exogenous variables
  variables_exo <- setdiff(variables, variables_endo)
  variables_exo_tbl <- model$variables %>%
    dplyr::filter(name %in% variables_exo) %>%
    dplyr::select(c("name", "init")) %>%
    dplyr::rename(
      lhs = name,
      rhs = init
    )
  # variables only on rhs
  variables_rhs <- stringr::str_replace_all(equations_rhs, "[!%^&*()\\-+=\\[\\]|<,>/]", " ") %>%
    stringr::str_squish() %>%
    stringr::str_split(" ", simplify = T) %>%
    vecsets::vsetdiff(functions) %>%
    unique()
  variables_rhs <- variables_rhs[suppressWarnings(is.na(as.numeric(variables_rhs)))]

  # 7. check for ".i" in variable names
  for (v in variables) {
    if (stringr::str_detect(v, "\\.i")) {
      stop("Expression .i can not be used in variable names
Please check: ", v, "
")
    }
  }

  # 8. check if there are endogenous variables not defined by the user
  v <- vecsets::vsetdiff(variables_not_user, variables_exo)
  if (length(v) != 0) {
    stop(paste0("These endogenous variables are not defined
Please add them to the model: ", paste0(v, collapse = ", "), "
"))
  }

  # 9. check if endogenous variables are explained by the model only once
  if (length(variables_endo[duplicated(variables_endo)]) > 0) {
    stop(paste0("Endogenous variable(s): ", paste0(variables_endo[duplicated(variables_endo)], collapse = ", "), " is/ are explained by the model more than once
Please remove one of the equations
"))
  }

  # 10. check if there are exogenous variables not defined by the user
  v <- vecsets::vsetdiff(variables_not_user, variables_endo)
  if (length(v) != 0) {
    stop(paste0("These exogenous variables are not defined
Please add them to the model: ", paste0(v, collapse = ", "), "
"))
  }

  # 11. check if all exogenous variables have init values defined by the user
  v <- model$variables[(model$variables$name %in% variables_exo) & is.na(model$variables$init), ]$name
  if (length(v) != 0) {
    stop(paste0("These variables are exogenous and require an initial value: ", paste0(v, collapse = ", "), "
"))
  }

  # 12. check init values
  len <- c()
  for (i in model$variables$init) {
    len <- c(len, length(i))
  }
  len_i <- which(!(len == max(len) | len == 1))
  if (!(identical(len_i, integer(0)))) {
    v <- model$variables$name[c(len_i)]
    stop(paste0("Please provide either single values or same lenght vectors for init values. Problem with inits in variable(s): ", paste0(v, collapse = ", "), "
"))
  }

  # 13. check if all provided variables are used in created equations
  v <- vecsets::vsetdiff(variables_user, variables_eqs)
  if (length(v) != 0) {
    if (verbose) {
      message(paste0("These user defined variables are not used in equations: ", paste0(v, collapse = ", "), "
"))
    }
  }

  # verbose
  if (verbose) {
    v <- vecsets::vsetdiff(variables_endo, variables_rhs)
    message(paste0("Endogenous variables: ", paste0(variables_endo, collapse = ", "), "
"))
    message(paste0("Endogenous variables as explained only: ", paste0(v, collapse = ", "), "
"))
    message(paste0("Exogenous variables: ", paste0(variables_exo, collapse = ", "), "
"))
  }

  return(list(equations_sep, variables_exo_tbl, functions))
}
