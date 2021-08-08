#' @title
#' Fetch Database Name to Key Map
#' @seealso
#'  \code{\link[R.cache]{loadCache}}
#' @rdname fetch_db_name
#' @export
#' @importFrom R.cache loadCache
fetch_db_name <-
  function(db_key) {

    if (missing(db_key)) {
      R.cache::loadCache(
        dirs = "neo4jconn",
        key  = list("db_name")
      )
    } else {


      x <-
        R.cache::loadCache(
          dirs = "neo4jconn",
          key  = list("db_name")) %>%
        split(.$db_key) %>%
        pluck(db_key)

      if (length(x)==0) {
        cli::cli_alert_danger("'{db_key}' not found.")
      } else {
        x
      }

    }

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
      split(.$db_name) %>%
      pluck(db_name)

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

      origin_timestamp <- as.character(Sys.time())
      db_df <-
        tibble::tibble(
          db_key = list.files(neo4j_home,
                              pattern = "^dbms-")) %>%
        mutate(db_name = NA_character_,
               db_datetime = origin_timestamp) %>%
        transmute(db_datetime,
                  db_key,
                  db_name,
                  db_exists = TRUE)


      R.cache::saveCache(
        object = db_df,
        dirs   = "neo4jconn",
        key    = list("db_name")
      )


    }

    call_timestamp <- as.character(Sys.time())

    db_name0 <-
      R.cache::loadCache(
        dirs = "neo4jconn",
        key  = list("db_name")) %>%
      tibble::as_tibble() %>%
      dplyr::mutate_all(as.character)

    # db_name0 <-
    #   db_name0 %>%
    #   dplyr::mutate(db_datetime = as.character(Sys.time())) %>%
    #   dplyr::select(db_datetime,
    #                 dplyr::everything())

    db_name <-
      db_name0 %>%
      dplyr::mutate(db_datetime = call_timestamp) %>%
      dplyr::mutate(
        db_exists =
          file.exists(
            file.path(neo4j_home,
                      db_key)))


    updated_db_name <-
      dplyr::full_join(
        db_name0,
        db_name,
        by = c("db_key",
               "db_name")) %>%
      dplyr::mutate(
        db_datetime =
          ifelse(
            db_exists.y != db_exists.x,
            db_datetime.y,
            db_datetime.x)) %>%
      dplyr::mutate(
        db_exists =
          ifelse(
            db_exists.y != db_exists.x,
            db_exists.y,
            db_exists.x)) %>%
      dplyr::select(
        -db_exists.y,
        -db_exists.x,
        -db_datetime.x,
        -db_datetime.y) %>%
      dplyr::distinct()

    new_db_name_df <-
      tibble::tibble(
        db_datetime = call_timestamp,
        db_name     = new_db_name,
        db_key      = new_db_key,
        db_exists   = as.character(file.exists(file.path(neo4j_home,
                                            new_db_key))))


    mutate_join_key <-
      function(data) {

        data %>%
          dplyr::mutate(
            join_key =
              stringr::str_remove_all(pattern = "[-]{1}",
                                      string = db_key))

      }

    updated_db_df <-
      dplyr::full_join(
        updated_db_name %>%
          mutate_join_key(),
        new_db_name_df %>%
          mutate_join_key(),
        by = "join_key",
        keep = TRUE) %>%
      dplyr::transmute(
        db_datetime = dplyr::coalesce(db_datetime.y, db_datetime.x),
        db_key      = dplyr::coalesce(db_key.y, db_key.x),
        db_name     = dplyr::coalesce(db_name.y, db_name.x),
        db_exists   = dplyr::coalesce(db_exists.y, db_exists.x))


    R.cache::saveCache(
      object = updated_db_df,
      dirs   = "neo4jconn",
      key    = list("db_name")
    )

    R.cache::loadCache(
      dirs = "neo4jconn",
      key  = list("db_name")
    )

    updated_db_df


  }

#' @title
#' Get All Database Name-Key Pairs
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
get_db_key_values <-
  function(neo4j_home = "~/Library/Application Support/com.Neo4j.Relate/Data/dbmss") {

    cached_file <-
      R.cache::findCache(
        dirs = "neo4jconn",
        key  = list("db_name")
      )


    if (is.null(cached_file)) {

      origin_timestamp <- as.character(Sys.time())
      db_df <-
        tibble::tibble(
          db_key = list.files(neo4j_home,
                              pattern = "^dbms-")) %>%
        mutate(db_name = NA_character_,
               db_datetime = origin_timestamp) %>%
        transmute(db_datetime,
                  db_key,
                  db_name,
                  db_exists = TRUE)


      R.cache::saveCache(
        object = db_df,
        dirs   = "neo4jconn",
        key    = list("db_name")
      )


    }

    call_timestamp <- as.character(Sys.time())

    db_name0 <-
      R.cache::loadCache(
        dirs = "neo4jconn",
        key  = list("db_name")) %>%
      tibble::as_tibble() %>%
      dplyr::mutate_all(as.character)

    # db_name0 <-
    #   db_name0 %>%
    #   dplyr::mutate(db_datetime = as.character(Sys.time())) %>%
    #   dplyr::select(db_datetime,
    #                 dplyr::everything())

    db_name <-
      db_name0 %>%
      dplyr::mutate(db_datetime = call_timestamp) %>%
      dplyr::mutate(
        db_exists =
          file.exists(
            file.path(neo4j_home,
                      db_key)))


    updated_db_name <-
      dplyr::full_join(
        db_name0,
        db_name,
        by = c("db_key",
               "db_name")) %>%
      dplyr::mutate(
        db_datetime =
          ifelse(
            db_exists.y != db_exists.x,
            db_datetime.y,
            db_datetime.x)) %>%
      dplyr::mutate(
        db_exists =
          ifelse(
            db_exists.y != db_exists.x,
            db_exists.y,
            db_exists.x)) %>%
      dplyr::select(
        -db_exists.y,
        -db_exists.x,
        -db_datetime.x,
        -db_datetime.y) %>%
      dplyr::distinct()

    R.cache::saveCache(
      object = updated_db_name,
      dirs   = "neo4jconn",
      key    = list("db_name")
    )

    R.cache::loadCache(
      dirs = "neo4jconn",
      key  = list("db_name")
    )

    updated_db_name


  }
