#' @title
#' Start Neo4j Database
#' @seealso
#'  \code{\link[readr]{read_lines}}
#'  \code{\link[neo4jshell]{neo4j_start}}
#'  \code{\link[glue]{glue}}
#'  \code{\link[stringr]{str_replace}}
#' @rdname start_neo4j
#' @export
#' @importFrom readr read_lines
#' @importFrom neo4jshell neo4j_start
#' @importFrom glue glue
#' @importFrom stringr str_replace

start_neo4j <-
  function(db_key     = "dbms-3e732dbc-5fd2-4f54-b6d5-dfaca7494d7c",
           db_name,
           neo4j_home = "~/Library/Application Support/com.Neo4j.Relate/Data/dbmss",
           uid        = "neo4j",
           pwd        = "admin",
           verbose    = TRUE) {

    if (missing(db_key) & missing(db_name)) {
      stop("Either `db_key` and `db_name` must be provided.")
    }


    if (!missing(db_name)) {

      fetched_db_key <- fetch_db_key(db_name = db_name)$db_key

      if (is.null(fetched_db_key) & missing(db_key)) {

        stop("`db_name` not recognized.")


      }

    }

    # Get paths
    db_home <- path.expand(file.path(neo4j_home,
                                     db_key))
    db_conf_file <- file.path(db_home, "conf", "neo4j.conf")

    neo4j_path <- file.path(db_home,
                            "bin",
                            "neo4j")

    # Load log in current state
    log_file <-
      file.path(db_home,
                "logs",
                "neo4j.log")
    log <-
      readr::read_lines(log_file)

    if (verbose) {

      cli::cli_progress_step("Starting...")

    }

    results <-
    capture.output(
    neo4jshell::neo4j_start(neo4j_path = neo4j_path)
    )

    if (any(grepl("Neo4j is already running", x = results))) {
      cli::cli_alert_danger(grep("Neo4j is already running",
                          x = results,
                          value = TRUE))

      list(conn_details = NULL,
           http_address = NULL,
           db_key = db_key,
           neo4j_home = neo4j_home,
           log =
             list(session = NULL,
                  initial = log))
    } else {


    for (i in 1:100) {

      if (verbose) {
        cli::cli_progress_update()
      }

      new_log <-
        readr::read_lines(log_file)
      new_log_lines <-
        new_log[!(new_log %in% log)]
      newest_line <-
        new_log_lines[length(new_log_lines)]

      Sys.sleep(1)

      if (length(newest_line)>0) {

        if (grepl("Started[.]{1}$", x = newest_line)) {

          bolt_address <-
            new_log_lines %>%
            grep(pattern = "Bolt enabled on",
                 value = TRUE) %>%
            stringr::str_replace(pattern = "(^.*Bolt enabled on )(.*?)([.]{1}.*$)",
                        replacement = "bolt://\\2") %>%
            unique()

          bolt_address <-
            bolt_address[length(bolt_address)]
          stopifnot(length(bolt_address) == 1)

          http_address <-
            new_log_lines %>%
            grep(pattern = "Remote interface available at ",
                 value = TRUE) %>%
            stringr::str_replace(pattern = "(^.*Remote interface available at )(.*)([/]{1}.*$)",
                        replacement = "\\2") %>%
            unique()

          http_address <-
            http_address[length(http_address)]
          stopifnot(length(http_address) == 1)

          conn_details <-
            list(address = bolt_address,
                 uid     = uid,
                 pwd     = pwd)

          # if (verbose) {
          #   cli::cli_text(newest_line)
          # }

          return(
          list(conn_details = conn_details,
               http_address = http_address,
               db_key = db_key,
               neo4j_home = neo4j_home,
               log =
                 list(session = new_log_lines,
                      initial = log))
          )
      } else if (grepl("Stopped[.]{1}$", x = newest_line)) {

        if (verbose) {
          cli::cli_text(new_log_lines)
        }

        stop(
          glue::glue(
            "Neo4j failed to start.\n\tCheck logs at {log_file}."
            ), call. = FALSE
          )

      }

      }

    }

    }

}



#' @title
#' Stop Neo4j Database
#' @seealso
#'  \code{\link[neo4jshell]{neo4j_stop}}
#' @rdname stop_neo4j
#' @export
#' @importFrom neo4jshell neo4j_stop
stop_neo4j <-
  function(conn) {

    db_home <- path.expand(file.path(conn$neo4j_home,
                                     conn$db_key))

    neo4j_path <- file.path(db_home,
                            "bin",
                            "neo4j")

    invisible(neo4jshell::neo4j_stop(neo4j_path = neo4j_path))


  }
