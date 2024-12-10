library(magrittr)

diagnostics <- function(models = c("SIM", "PC", "LP", "REG", "OPEN", "BMW", "BMWK", "DIS", "DISINF", "SIMEX", "PCEX"),
                        parameters,
                        methods = c("Newton", "Gauss"),
                        lower = 0,
                        upper = 1,
                        step = 0.1,
                        precision = 0) {
  message("
*** CALCULATING SCENARIOS ***
")
  master_list <- list()

  for (m in models) {
    # wyniki pakietu godley
    model_g <- create_model(name = "diag", template = m)

    # auto parameters
    eqs <- model_g$equations %>%
      dplyr::filter(hidden == FALSE)
    eqs <- eqs$equation
    equations_sep <- tibble::tibble(eqs) %>%
      tidyr::separate(.data$eqs, c("lhs", "rhs"), "=") %>%
      dplyr::mutate(
        lhs = stringr::str_squish(lhs),
        rhs = stringr::str_squish(rhs)
      )
    variables_exo <- setdiff(model_g$variables$name, equations_sep$lhs)
    variables_exo <- model_g$variables %>%
      dplyr::filter(name %in% variables_exo) %>%
      dplyr::select(c("name", "init")) %>%
      dplyr::rename(
        lhs = name,
        rhs = init
      )
    
    if (missing(parameters)) {
      parameters <- variables_exo$lhs
    }

    for (p in parameters) {
      for (met in methods) {

        # wyniki pakietu godley
        tryCatch(
          {
            model_g_sen <- create_sensitivity(
              model_pass = model_g,
              variable = p,
              lower = lower,
              upper = upper,
              step = step
            )
              
            model_g_sen <- simulate_scenario(
              model_g_sen,
              max_iter = 350,
              periods = 100,
              hidden_tol = 0.1,
              tol = 1e-05,
              method = met
            )
          },
          error = function(e) {}
        )

        message(paste0("Finished godley model: ", m, ", parameter: ", p, ", method: ", met))

        # equations
        equations_s <- list()
        for (e in dplyr::filter(model_g$equations, hidden == F)$equation) {
          equations_s <- append(equations_s, as.formula(stringr::str_replace(e, "=", "~")))
        }

        # parameters
        equations_s_param <- list()
        for (e in 1:nrow(variables_exo)) {
          equations_s_param <- append(
            equations_s_param,
            as.formula(
              paste(
                variables_exo[e, ]$lhs,
                " ~ ",
                variables_exo[e, ]$rhs
              )
            )
          )
        }

        # equations hidden
        eqs <- model_g$equations %>%
          dplyr::filter(hidden == TRUE)
        eqs <- eqs$equation
        equations_sep <- tibble::tibble(eqs) %>%
          tidyr::separate(.data$eqs, c("lhs", "rhs"), "=") %>%
          dplyr::mutate(
            lhs = stringr::str_squish(lhs),
            rhs = stringr::str_squish(rhs)
          )

        equations_s_hidden <- c()

        for (e in 1:nrow(equations_sep)) {
          hidden <- c(as.character(equations_sep[e, ]$rhs))
          names(hidden) <- as.character(equations_sep[e, ]$lhs)

          equations_s_hidden <- append(equations_s_hidden, hidden)
        }

        # wyniki pakietu sfcr
        eqs <- sfcr::sfcr_set(equations_s[[1]])
        for (e in 2:length(equations_s)) {
          eqs <- sfcr::sfcr_set(eqs, equations_s[[e]])
        }

        external <- sfcr::sfcr_set(equations_s_param[[1]])
        for (e in 2:length(equations_s_param)) {
          external <- sfcr::sfcr_set(external, equations_s_param[[e]])
        }

        x_external <- sfcr::sfcr_expand(
          external,
          !!p,
          seq(from = lower, to = upper, by = step)
        )

        tryCatch(
          {
            model_s_sen <- sfcr::sfcr_multis(x_external,
              eqs,
              periods = 100,
              hidden = hidden,
              method = met,
              max_iter = 350,
              .hidden_tol = 0.1,
              tol = 1e-05
            )
          },
          error = function(e) {}
        )

        message(paste0("Finished sfcr model: ", m, ", parameter: ", p, ", method: ", met))


        if (!exists("model_g_sen", inherits = F)) {
          model_g_sen <- list("Error" = paste(m, p, met, sep = "_"))
        }
        if (!exists("model_s_sen", inherits = F)) {
          model_s_sen <- list("Error" = paste(m, p, met, sep = "_"))
        }

        master_list <- append(
          master_list,
          list(model_g_sen)
        )

        master_list <- append(
          master_list,
          list(model_s_sen)
        )

        rm(model_g_sen)
        rm(model_s_sen)

        master_list <<- master_list
      }
    }
  }
  message("
*** COMPARING RESULTS ***
")
  nodifferr <- 0
  diff <- 0
  message_nodiff_err <- list()
  message_diff_list <- list()
  # porównanie wyników
  for (g in seq(1, length(master_list), 2)) {
    if (length(master_list[[g]]) == 1 & length(master_list[[g + 1]]) == 1) {
      message(paste0("No difference found for ", master_list[[g]]$Error, ", both generated errors"))
      nodifferr <- nodifferr + 1
      message_nodiff_err <- append(message_nodiff_err, paste0("No difference found for ", master_list[[g]]$Error, ", both generated errors, g ", g))
      next
    } else if (length(master_list[[g]]) == 1 & length(master_list[[g + 1]]) != 1) {
      message(paste0("Difference found for ", master_list[[g]]$Error, ", only godley generated errors, g ", g))
      diff <- diff + 1
      message_diff_list <- append(message_diff_list, paste0("Difference found for ", master_list[[g]]$Error, ", only godley generated errors, g ", g))
      next
    } else if (length(master_list[[g]]) != 1 & length(master_list[[g + 1]]) == 1) {
      message(paste0("Difference found for ", master_list[[g + 1]]$Error, ", only sfcr generated errors, g ", g))
      message_diff_list <- append(message_diff_list, paste0("Difference found for ", master_list[[g + 1]]$Error, ", only sfcr generated errors, g ", g))
      diff <- diff + 1
      next
    }

    for (i in 1:(((upper - lower) / step) + 1)) {
      table_godley <- tibble::tibble(master_list[[g]][[5 + i]]$result)
      table_godley <- table_godley %>% dplyr::select(sort(colnames(table_godley)))
      table_godley <- table_godley %>% dplyr::select(-time)
      table_godley[1, ] <- 0

      table_sfcr <- dplyr::select(tibble::tibble(master_list[[g + 1]][[i]]), -c(period, simulation))
      table_sfcr <- table_sfcr %>% dplyr::select(sort(colnames(table_sfcr)))

      # komunikaty
      if (!all(colnames(table_godley) == colnames(table_sfcr))) {
        warning(paste0("Tables ", g, " and ", g + 1, " subset ", 5 + i, " and ", i, " inherited different variable sets"))
        next
      }

      if (sum(round(table_godley, precision) - round(table_sfcr, precision)) == 0) {
        message(paste0("No difference found for tables ", g, " and ", g + 1, " subset ", 5 + i, " and ", i))
      } else {
        difference <- sum(round(table_godley, precision) - round(table_sfcr, precision))
        message(paste0("Difference found for tables ", g, " and ", g + 1, " subset ", 5 + i, " and ", i, ", difference ", difference))
        diff <- diff + 1
        message_diff_list <- append(message_diff_list, paste0("Difference found for tables ", g, " and ", g + 1, " subset ", 5 + i, " and ", i, ", difference ", difference))
      }
    }
  }
  message("
*** REPORT SUMMARY ***")
  if (diff == 0) {
    message("
All results checked, no differences found
            ")
  } else {
    message("
All results checked, ", diff, " differences found:
            ")
    for (mes in message_diff_list) {
      message(mes)
    }
  }
  if (nodifferr != 0) {
    message("
Those results generated no difference but gave errors:
")
    for (mes in message_nodiff_err) {
      message(mes)
    }
  }
  message(" ")
}
