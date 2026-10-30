# Context tree ----------------------------------------------------------------
#
# The flat matrix listing carries no classification, which is why
# tempo_toc(full_description = TRUE) has to ask about every matrix in turn.  The
# service also exposes its navigation tree, in two shapes:
#
#   GET context/         an array of branch nodes, each wrapped as
#                        {parentCode, level, context: {name, code, childrenUrl,
#                        comment, url}}.  One request returns every branch:
#                        currently 8 domains, 70 sub-domains and 262 surveys.
#
#   GET context/{code}   a single node, as
#                        {context: {...}, ancestors: [...], children: [...]},
#                        where children are bare nodes.
#
# A node's "childrenUrl" names the endpoint serving its children: "context" for
# a branch, "matrix" for a statistical matrix.  Matrices do not appear in the
# root listing, so enumerating them costs one request per deepest branch.
#
# Both shapes were confirmed against the live service, but TEMPO Online
# publishes no API contract, so the parsing below accepts the plausible field
# spellings and reports an unexpected shape instead of guessing.

.tempo_context_node <- function(node) {
  if (!is.list(node)) {
    return(NULL)
  }

  code <- .tempo_first_field(node, c("code", "id", "nodeId"))
  if (is.na(code)) {
    return(NULL)
  }

  list(
    code = code,
    name = .tempo_first_field(node, c("name", "label", "context")),
    children_url = .tempo_first_field(node, c("childrenUrl", "url"))
  )
}

# The root listing: wrapped nodes carrying their own parent and level.
.tempo_context_root <- function(parsed) {
  if (!is.list(parsed) || !length(parsed)) {
    return(NULL)
  }

  entries <- lapply(parsed, function(entry) {
    if (!is.list(entry)) {
      return(NULL)
    }

    # Tolerate an unwrapped node as well as the {parentCode, level, context}
    # wrapper the service currently sends.
    node <- .tempo_context_node(entry$context %||% entry)
    if (is.null(node)) {
      return(NULL)
    }

    level <- suppressWarnings(as.integer(entry$level))
    parent <- .tempo_first_field(entry, c("parentCode", "parent"))

    c(node, list(
      parent_code = parent,
      level = if (length(level) == 1L && !is.na(level)) level else NA_integer_
    ))
  })

  entries <- entries[!vapply(entries, is.null, logical(1))]
  if (!length(entries)) {
    return(NULL)
  }

  frame <- data.frame(
    code = vapply(entries, function(x) x$code, character(1)),
    name = .tempo_strip_html(vapply(entries, function(x) x$name, character(1))),
    parent_code = vapply(entries, function(x) x$parent_code, character(1)),
    level = vapply(entries, function(x) x$level, integer(1)),
    children_url = vapply(entries, function(x) x$children_url, character(1)),
    stringsAsFactors = FALSE
  )

  # The service reports the root's parent as "0", which is not a node.
  frame$parent_code[frame$parent_code %in% c("0", "")] <- NA_character_
  # Levels are 0-based at the service; 1-based is friendlier to report.
  frame$level <- frame$level + 1L
  frame
}

# A single node's children, which arrive unwrapped.
.tempo_context_children <- function(parsed) {
  children <- parsed$children
  if (!is.list(children) || !length(children)) {
    return(NULL)
  }

  nodes <- lapply(children, .tempo_context_node)
  nodes <- nodes[!vapply(nodes, is.null, logical(1))]
  if (!length(nodes)) {
    return(NULL)
  }

  data.frame(
    code = vapply(nodes, function(x) x$code, character(1)),
    name = .tempo_strip_html(vapply(nodes, function(x) x$name, character(1))),
    children_url = vapply(nodes, function(x) x$children_url, character(1)),
    stringsAsFactors = FALSE
  )
}

# Build each node's full path from its parent links, level by level.
.tempo_context_paths <- function(nodes) {
  path <- rep(NA_character_, nrow(nodes))

  for (level in sort(unique(nodes$level))) {
    rows <- which(nodes$level == level)
    parent_path <- path[match(nodes$parent_code[rows], nodes$code)]
    path[rows] <- ifelse(
      is.na(parent_path),
      nodes$name[rows],
      paste(parent_path, nodes$name[rows], sep = " > ")
    )
  }

  path
}

#' Retrieve the TEMPO Online classification tree
#'
#' Returns the service's own navigation tree: the statistical domains,
#' sub-domains and surveys, and optionally every matrix beneath them, each with
#' its parent and its full classification path.
#'
#' This is the cheap way to learn where a matrix sits in the classification.
#' The branch structure arrives in a single request.
#' `tempo_toc(full_description = TRUE)` recovers the same three levels by
#' requesting the metadata of roughly 1500 matrices, one at a time, because it
#' also reports each matrix's publication date.
#'
#' @param language A single string: `"ro"` for Romanian (the default) or `"en"`
#'   for English.
#' @param matrices A single logical value. If `TRUE` (the default), also list
#'   the matrices under each survey. The service does not include them in the
#'   tree listing, so this costs one additional request per deepest branch,
#'   currently 262 of them; they are issued concurrently and cached. Set
#'   `FALSE` for the branch structure alone, which is one request.
#' @param cache A single logical value. If `TRUE` (the default), reuse and
#'   populate the response cache described in [tempo_cache_dir()].
#'
#' @return A data frame with one row per node and the columns
#'   \describe{
#'     \item{`code`}{Node code. For a matrix this is the code accepted by
#'       [tempo_get()] and [tempo_bulk()].}
#'     \item{`name`}{Node label, as plain text.}
#'     \item{`parent_code`}{Code of the parent node, `NA` at the root.}
#'     \item{`level`}{Depth, 1 for a statistical domain.}
#'     \item{`is_matrix`}{`TRUE` for statistical matrices.}
#'     \item{`path`}{Full classification path, levels separated by `" > "`.}
#'   }
#'
#' @section Stability:
#' TEMPO Online publishes no API contract. The two response shapes this
#' function reads have been confirmed against the live service, and both the
#' field names and the wrapper are matched tolerantly, but a change at the
#' service could still break the walk. It then raises a `tempo_api_error`
#' rather than returning something half-built, and
#' `tempo_toc(full_description = TRUE)` remains available as the
#' longer-established route to the same three classification levels.
#'
#' @seealso [tempo_toc()], [tempo_search()], [tempo_meta()]
#'
#' @examples
#' \dontrun{
#' # One request: the classification itself.
#' branches <- tempo_hierarchy(language = "en", matrices = FALSE)
#' table(branches$level)
#' subset(branches, level == 1, select = c(code, name))
#'
#' # The whole tree, matrices included.
#' tree <- tempo_hierarchy(language = "en")
#'
#' # Where does one matrix sit?
#' tree[tree$code == "POP105A", c("level", "path")]
#'
#' # Every matrix under one domain, without describing all 1500 tables.
#' population <- tree[tree$is_matrix & grepl("POPULATIE", tree$path), ]
#' head(population[, c("code", "name")])
#' }
#' @export
tempo_hierarchy <- function(language = "ro", matrices = TRUE, cache = TRUE) {
  language <- .tempo_validate_language(language)
  matrices <- .tempo_validate_flag(matrices, "matrices")
  cache <- .tempo_validate_flag(cache, "cache")

  unexpected <- function() {
    .tempo_abort(
      paste0(
        "The TEMPO Online service returned an unexpected context-tree ",
        "format. Use tempo_toc(full_description = TRUE) instead, and please ",
        "report this at https://github.com/RProjectRomania/TEMPO/issues"
      ),
      class = "tempo_api_error"
    )
  }

  walk <- function() {
    response <- tryCatch(
      .tempo_request(.tempo_url("context/", language = language)),
      tempo_http_error = function(condition) {
        if (identical(condition$status_code, 404L)) {
          unexpected()
        }
        stop(condition)
      }
    )

    nodes <- .tempo_context_root(
      .tempo_parse_json(response, "context-tree", simplify_vector = FALSE)
    )
    if (is.null(nodes) || !nrow(nodes)) {
      unexpected()
    }

    nodes$path <- .tempo_context_paths(nodes)
    nodes$is_matrix <- !is.na(nodes$children_url) &
      nodes$children_url == "matrix"

    columns <- c("code", "name", "parent_code", "level", "is_matrix", "path")
    if (!matrices) {
      result <- nodes[, columns, drop = FALSE]
      rownames(result) <- NULL
      return(result)
    }

    collected <- list(nodes[, columns, drop = FALSE])

    # Expand the branches whose children the root listing did not include.
    # Bounded, so an unexpectedly deep or cyclic tree cannot loop for ever.
    frontier <- nodes[!nodes$is_matrix & !nodes$code %in% nodes$parent_code, ,
                      drop = FALSE]

    for (round in seq_len(8L)) {
      if (!nrow(frontier)) {
        break
      }

      urls <- vapply(
        frontier$code,
        function(code) {
          .tempo_url(paste0("context/", code), language = language)
        },
        character(1),
        USE.NAMES = FALSE
      )

      responses <- .tempo_request_many(
        urls,
        progress_label = sprintf("Listing matrices (level %d)", round + 1L)
      )

      found <- list()
      for (index in seq_along(responses)) {
        children <- .tempo_context_children(
          .tempo_parse_json(responses[[index]], "context-node",
                            simplify_vector = FALSE)
        )
        if (is.null(children)) {
          next
        }

        found[[length(found) + 1L]] <- data.frame(
          code = children$code,
          name = children$name,
          parent_code = frontier$code[[index]],
          level = frontier$level[[index]] + 1L,
          is_matrix = !is.na(children$children_url) &
            children$children_url == "matrix",
          path = paste(frontier$path[[index]], children$name, sep = " > "),
          stringsAsFactors = FALSE
        )
      }

      if (!length(found)) {
        break
      }

      discovered <- do.call(rbind, found)
      collected[[length(collected) + 1L]] <- discovered
      frontier <- discovered[!discovered$is_matrix, , drop = FALSE]
    }

    result <- do.call(rbind, collected)
    rownames(result) <- NULL
    result
  }

  if (isTRUE(cache)) {
    .tempo_cached(
      .tempo_cache_key("hierarchy", language, matrices, .tempo_base_url()),
      walk
    )
  } else {
    walk()
  }
}
