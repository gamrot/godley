#' Create SFC model object
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom stats time
#' @importFrom utils tail
#'
#' @param name string name for created SFC model object
#' @param template string name of model template chosen from: 'SIM', 'PC', 'LP', 'REG', 'OPEN', 'BMW', 'BMWK', 'DIS', 'DISINF', 'SIMEX', 'PCEX'
#' or user created SFC model object to be used as a template
#'
#' @return SFC model object

create_model <- function(name = "SFC model",
                         template) {
  # argument check
  # type
  checkmate::assert_string(name)

  # copying variables and equations if the name of an existing model was given
  if (!missing(template)) {
    if (is.character(template)) {
      if (template %in% c("SIM", "PC", "PCEX", "LP", "REG", "OPEN", "BMW", "BMWK", "DIS", "DISINF", "SIMEX")) {
        # loading a model defined based on examples
        model <- suppressMessages(load_model_template(template))
        message("Model ", template, " loaded from template")
      } else {
        stop("There is no template named ", template, "
Please choose from: SIM, SIMEX, PC, PCEX, LP, REG, OPEN, BMW, BMWK, DIS, DISINF")
      }
    } else {
      checkmate::assert_class(template, "SFC")
      model <- template
      model <- model[names(model) %in% c("variables", "equations")]
      message("Model loaded from environment")
    }
  } else {
    model <- structure(list(), class = "SFC")
    message("Empty model created")
  }

  # providing a name for the model
  model <- structure(append(list("name" = name), model), class = "SFC")

  return(model)
}
