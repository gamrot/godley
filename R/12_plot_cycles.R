#' Network plot of the model
#'
#' @param model SFC model object created with \code{\link{create_model}}
#' @param save_file name and path to save the plot as html file
#'
#' @details This function creates a representation of a model as a directed graph.
#' Additionally it shows cycles in the model including these with lagged variables.
#' Graph can be saved as html file.
#'
#' @return visNetwork object
#' @export
#'
#' @examples
#' model <- godley::create_model(name = "SFC model", template = "BMW")
#' plot_cycles(model)
#'
plot_cycles <- function(model, save_file = NULL) {
  # Argument check
  checkmate::assert_class(model, "SFC")
  checkmate::assert_character(save_file, len = 1, null.ok = TRUE)

  res <- validate_model_input(model, verbose = FALSE)
  calls <- attr(model$prepared, "calls")
  if (is.null(calls)) {
    model <- prepare(model)
    calls <- attr(model$prepared, "calls")
  }

  equations_sep <- res[[1]]
  variables_exo <- res[[2]]
  functions <- res[[3]]

  km <- find_adjacency(equations_sep)
  blocks <- unique(sort(calls$block))
  equations_id <- lapply(blocks, function(x) {
    calls[, "id"][calls[, "block"] == x]
  })

  cycles <- equations_id[lapply(equations_id, length) > 1]
  for (i in seq_along(cycles)) {
    cycles[[i]] <- calls[calls$id %in% cycles[[i]], ]$lhs
  }

  # Create graph and extract data
  graph <- igraph::graph_from_adjacency_matrix(km, mode = "directed")
  visgraph <- visNetwork::toVisNetworkData(graph)

  # Customize edges
  visgraph$edges$arrows <- "from"
  visgraph$edges$smooth <- TRUE # Add curvature to edges
  visgraph$edges$color <- lapply(1:nrow(visgraph$edges), function(x) list(color = "gray", highlight = "black"))

  # Customize nodes
  visgraph$nodes$title <- visgraph$nodes$id # Tooltips for nodes
  visgraph$nodes <- visgraph$nodes[c("id", "title")]
  visgraph$nodes$group <- "X"

  # Assign groups to nodes
  for (i in seq_along(cycles)) {
    visgraph$nodes$group[visgraph$nodes$id %in% cycles[[i]]] <- LETTERS[i]
  }

  # Generate custom colors for groups
  custom_colors <- c(
    "#1f77b4",
    "#ff7f0e",
    "#2ca02c",
    "#d62728",
    "#9467bd",
    "#8c564b",
    "#e377c2",
    "#7f7f7f",
    "#bcbd22",
    "#17becf",
    "#577590",
    "#F9C74F"
  )
  unique_groups <- unique(visgraph$nodes$group)
  num_groups <- length(unique_groups)
  color_palette <- if (num_groups <= length(custom_colors)) {
    custom_colors[1:num_groups]
  } else {
    grDevices::colorRampPalette(custom_colors)(num_groups)
  }
  group_colors <- stats::setNames(color_palette, unique_groups)

  visgraph$nodes$color <- group_colors[visgraph$nodes$group]

  # Scale node sizes by degree
  node_degrees <- igraph::degree(graph)
  visgraph$nodes$value <- node_degrees[visgraph$nodes$id] * 5 # Scale node size

  # Create the network plot
  network <- visNetwork::visNetwork(
    nodes = visgraph$nodes,
    edges = visgraph$edges,
    width = "100%", height = "800px"
  ) %>%
    visNetwork::visNodes(
      shape = "dot",
      color = list(
        background = "#0085AF",
        border = "#013848",
        highlight = "#FF8000"
      ),
      shadow = list(enabled = TRUE, size = 10) # Shadow settings
    ) %>%
    visNetwork::visEdges(smooth = TRUE) %>%
    visNetwork::visOptions(
      highlightNearest = list(enabled = TRUE, hover = TRUE)
    ) %>%
    visNetwork::visLayout(randomSeed = 42, improvedLayout = TRUE) # Improve layout

  # Save as HTML if needed
  if (!is.null(save_file)) {
    if (!grepl(".html$", save_file)) {
      save_file <- paste0(save_file, ".html")
    }
    visNetwork::visSave(network, file = save_file)
  }

  return(network)
}
