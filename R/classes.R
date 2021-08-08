#' Neo4jConnection S4 Class

Neo4jConnection <-
  setClass(
    Class = "Neo4jConnection",
    slots = list(conn_details = "list",
                 http_address = "character",
                 db_key       = "character",
                 neo4j_home   = "character",
                 log          = "list")
  )
