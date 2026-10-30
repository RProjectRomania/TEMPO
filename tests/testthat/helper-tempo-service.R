# A fake TEMPO Online service -------------------------------------------------
#
# The real service is a public HTTP endpoint, so none of it can be exercised on
# CRAN or offline.  This helper reimplements just enough of it with webfakes to
# test the request layer, chunked downloads, caching and the end-to-end
# functions without touching the network.

tempo_fake_app <- function() {
  tempo_fake_metadata <- function(code, n1, n2, update = "05-11-2025") {
    list(
      matrixName = paste("Fake matrix", code),
      ultimaActualizare = update,
      ancestors = list(
        list(name = "ROOT"),
        list(name = paste0(
          'AGRICULTURE <a href="https://insse.ro/press?x=1" target="_blank">',
          "Press releases</a>"
        )),
        list(name = "Sub-domain"),
        list(name = "Survey name")
      ),
      details = list(matMaxDim = 2L, matUMSpec = 0L),
      dimensionsMap = list(
        list(
          label = "Judete",
          options = lapply(seq_len(n1), function(i) {
            list(nomItemId = 1000L + i, label = paste0("County ", i))
          })
        ),
        list(
          label = "Ani",
          options = lapply(seq_len(n2), function(i) {
            list(nomItemId = 2000L + i, label = paste0("Anul ", 2019L + i))
          })
        )
      )
    )
  }
  app <- webfakes::new_app()
  app$use(webfakes::mw_json())
  app$locals$pivot_calls <- 0L
  app$locals$toc_calls <- 0L
  app$locals$flaky_calls <- 0L
  app$locals$context_calls <- 0L

  json <- function(res, x, status = 200L) {
    res$set_status(status)
    res$set_header("Content-Type", "application/json;charset=UTF-8")
    res$send(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))
  }

  # Flat list of matrices.  One name carries HTML, to check stripping.
  app$get("/tempo-ins/matrix/matrices", function(req, res) {
    req$app$locals$toc_calls <- req$app$locals$toc_calls + 1L
    json(res, list(
      list(name = "Small fake table", code = "SML101A",
           childrenUrl = "matrix", comment = NULL, url = "matrix"),
      list(name = "Big fake table", code = "BIG101A",
           childrenUrl = "matrix", comment = "a comment", url = "matrix"),
      list(name = 'Tagged table <a href="https://insse.ro/x">Press releases</a>',
           code = "TAG101A", childrenUrl = "matrix", comment = NULL,
           url = "matrix")
    ))
  })

  # Fails twice with 500, then succeeds: exercises retry and backoff.
  app$get("/tempo-ins/flaky/matrices", function(req, res) {
    req$app$locals$flaky_calls <- req$app$locals$flaky_calls + 1L
    if (req$app$locals$flaky_calls < 3L) {
      res$set_status(500L)
      res$send("upstream wobbled")
      return()
    }
    json(res, list(list(name = "Recovered", code = "REC101A",
                        childrenUrl = "matrix")))
  })

  app$get("/tempo-ins/matrix/:code", function(req, res) {
    code <- req$params$code
    sizes <- switch(
      code,
      SML101A = c(2L, 3L),
      BIG101A = c(7L, 5L),
      TAG101A = c(2L, 2L),
      BADHDR  = c(4L, 1L),
      ODDDATE = c(2L, 2L),
      NULL
    )

    if (is.null(sizes)) {
      res$set_status(404L)
      res$send("no such matrix")
      return()
    }

    update <- if (identical(code, "ODDDATE")) "2025.11.05" else "05-11-2025"
    json(res, tempo_fake_metadata(code, sizes[[1L]], sizes[[2L]], update))
  })

  # Context tree, reproducing the live service's two shapes exactly.
  #
  #   context/        array of {parentCode, level, context:{...}} for every
  #                   branch, across all branch levels, matrices excluded.
  #   context/{code}  {context, ancestors, children} with bare child nodes.
  #
  # Branches here: domain "1" -> sub-domain "10" -> survey "1010", whose
  # children are the two fake matrices.
  app$get("/tempo-ins/context/", function(req, res) {
    req$app$locals$context_calls <- req$app$locals$context_calls + 1L
    node <- function(name, code) {
      list(name = name, code = code, childrenUrl = "context",
           comment = NULL, url = "context")
    }
    json(res, list(
      list(parentCode = "0", level = 0L,
           context = node(paste0(
             'AGRICULTURE <a href="https://insse.ro/p" target="_blank">',
             "Press releases</a>"
           ), "1")),
      list(parentCode = "1", level = 1L,
           context = node("Land use", "10")),
      list(parentCode = "10", level = 2L,
           context = node("Areas and yields", "1010"))
    ))
  })

  app$get("/tempo-ins/context/:code", function(req, res) {
    code <- req$params$code
    bare <- function(name, nodecode, kind) {
      list(name = name, code = nodecode, childrenUrl = kind,
           comment = NULL, url = kind)
    }

    children <- if (identical(code, "1010")) {
      list(
        bare("Small fake table", "SML101A", "matrix"),
        bare("Big fake table", "BIG101A", "matrix")
      )
    } else if (identical(code, "10")) {
      list(bare("Areas and yields", "1010", "context"))
    } else if (identical(code, "1")) {
      list(bare("Land use", "10", "context"))
    } else {
      list()
    }

    json(res, list(
      context = bare("Node", code, "context"),
      ancestors = list(bare("home", "", "context")),
      children = children
    ))
  })

  # The pivot endpoint returns one CSV row per selected combination, plus a
  # footer line, so that chunk reassembly can be verified by content.
  app$post("/tempo-ins/pivot", function(req, res) {
    req$app$locals$pivot_calls <- req$app$locals$pivot_calls + 1L
    payload <- req$json

    selections <- strsplit(payload$encQuery, ":", fixed = TRUE)[[1L]]
    ids <- lapply(selections, function(part) trimws(strsplit(part, ",")[[1L]]))

    header <- if (identical(payload$matCode, "BADHDR") &&
                  req$app$locals$pivot_calls %% 2L == 0L) {
      "Judete,Ani,UM: Numar,TOTAL ALTERED"
    } else {
      "Judete,Ani,UM: Numar,Valoare"
    }

    grid <- expand.grid(d2 = ids[[2L]], d1 = ids[[1L]], stringsAsFactors = FALSE)
    rows <- sprintf(
      "County %d,Anul %d,Numar,%s",
      as.integer(grid$d1) - 1000L,
      as.integer(grid$d2) - 2000L + 2019L,
      ifelse(seq_len(nrow(grid)) == 1L, ":", seq_len(nrow(grid)))
    )

    res$set_header("Content-Type", "text/csv;charset=UTF-8")
    res$send(paste0(
      paste(c(header, rows, ",,,(c) INS 1998-2026"), collapse = "\n"),
      "\n"
    ))
  })

  app
}

# Start the fake service and point TEMPO at it for the duration of the caller.
local_tempo_service <- function(env = parent.frame()) {
  testthat::skip_if_not_installed("webfakes")

  process <- webfakes::local_app_process(tempo_fake_app(), .local_envir = env)
  base <- paste0(sub("/$", "", process$url()), "/tempo-ins")

  old <- options(
    TEMPO.base_url = base,
    TEMPO.cache_dir = file.path(tempfile("tempo-cache-"), "cache"),
    TEMPO.progress = FALSE,
    TEMPO.backoff = 0.01,
    TEMPO.throttle = 0
  )
  withr::defer(options(old), envir = env)

  .tempo_reset_warnings()
  process
}
