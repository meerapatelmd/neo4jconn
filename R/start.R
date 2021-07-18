


#' @title
#' Fetch Database Name to Key Map
#' @seealso
#'  \code{\link[R.cache]{loadCache}}
#' @rdname fetch_db_name
#' @export
#' @importFrom R.cache loadCache
fetch_db_name <-
  function() {

    R.cache::loadCache(
      dirs = "neo4jconn",
      key  = list("db_name")
    )

  }


#' @title
#' Fetch Database Key for a Name
#' @seealso
#'  \code{\link[dplyr]{filter}}
#' @rdname fetch_db_key
#' @export
#' @importFrom dplyr filter
fetch_db_key <-
  function(db_name) {
      fetch_db_name() %>%
      dplyr::filter(db_name == db_name)

  }


#' @title
#' Store Database Name
#' @seealso
#'  \code{\link[R.cache]{findCache}},\code{\link[R.cache]{saveCache}},\code{\link[R.cache]{loadCache}}
#'  \code{\link[tibble]{c("tibble", "tibble")}},\code{\link[tibble]{as_tibble}}
#'  \code{\link[dplyr]{mutate_all}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{coalesce}}
#'  \code{\link[stringr]{str_remove}}
#' @rdname store_db_name
#' @export
#' @importFrom R.cache findCache saveCache loadCache
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr mutate_all mutate coalesce
#' @importFrom stringr str_remove_all
store_db_name <-
  function(new_db_name,
           new_db_key,
           neo4j_home = "~/Library/Application Support/com.Neo4j.Relate/Data/dbmss") {

    cached_file <-
    R.cache::findCache(
      dirs = "neo4jconn",
      key  = list("db_name")
    )

    if (is.null(cached_file)) {

      db_df <-
        tibble::tibble(
          db_key = list.files(neo4j_home,pattern = "^dbms-")) %>%
        mutate(db_name = NA_character_)

      R.cache::saveCache(
        object = db_df,
        dirs   = "neo4jconn",
        key    = list("db_name")
      )


    }

    db_name <-
      R.cache::loadCache(
        dirs = "neo4jconn",
        key  = list("db_name")
      ) %>%
      tibble::as_tibble() %>%
      dplyr::mutate_all(as.character) %>%
      dplyr::mutate(join_key.x = stringr::str_remove_all(db_key, "[-]{1}"))

    new_db_name <-
      tibble::tibble(
        db_name = new_db_name,
        db_key  = new_db_key
      ) %>%
      dplyr::mutate(join_key.y = stringr::str_remove_all(new_db_key,"[-]{1}"))

    updated_db_df <-
    db_name %>%
      left_join(new_db_name,
                keep = TRUE,
                by = c("join_key.x" = "join_key.y")) %>%
      transmute(db_key = dplyr::coalesce(db_key.y, db_key.x),
                db_name = dplyr::coalesce(db_name.y, db_name.x))


    R.cache::saveCache(
      object = updated_db_df,
      dirs   = "neo4jconn",
      key    = list("db_name")
    )

    R.cache::loadCache(
      dirs = "neo4jconn",
      key  = list("db_name")
    )


  }




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
  function(db_key,
           neo4j_home = "~/Library/Application Support/com.Neo4j.Relate/Data/dbmss",
           uid = "neo4j",
           pwd = "admin",
           verbose = TRUE) {

    # Get paths
    #neo4j_home <- "~/Library/Application Support/com.Neo4j.Relate/Data/dbmss"
    #db_key <- "dbms-3e732dbc-5fd2-4f54-b6d5-dfaca7494d7c"
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
    log <- readr::read_lines(log_file)

    neo4jshell::neo4j_start(neo4j_path = neo4j_path)

    for (i in 1:100) {

      if (verbose) {
      cat(glue::glue("{i*5} secs..."),
          sep = "\n")
      }
      Sys.sleep(5)
      new_log <-
        readr::read_lines(log_file)
      new_log_lines <-
        new_log[!(new_log %in% log)]
      newest_line <-
        new_log_lines[length(new_log_lines)]
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

        if (verbose) {
        cat(newest_line,
            sep = "\n")
        }
        break
      } else if (grepl("Stopped[.]{1}$", x = newest_line)) {

        if (verbose) {
        cat(new_log_lines,
            sep = "\n")
        }
        stop(glue::glue("Neo4j failed to start.\n\tCheck logs at {log_file}."), call. = FALSE)

      }
    }


    list(conn_details = conn_details,
         http_address = http_address,
         log =
           list(session = new_log_lines,
                initial = log))
}



#' @title
#' Stop Neo4j Database
#' @seealso
#'  \code{\link[neo4jshell]{neo4j_stop}}
#' @rdname stop_neo4j
#' @export
#' @importFrom neo4jshell neo4j_stop
stop_neo4j <-
  function(db_key,
           neo4j_home = "~/Library/Application Support/com.Neo4j.Relate/Data/dbmss") {

    db_home <- path.expand(file.path(neo4j_home,
                                     db_key))

    neo4j_path <- file.path(db_home,
                            "bin",
                            "neo4j")

    neo4jshell::neo4j_stop(neo4j_path = neo4j_path)


  }
