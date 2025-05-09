#
#  Arch REST API
#
#
#





#* Service information
#* 
#* @get /api/info
#* 
#* @response 200 OK
#* @response 401 Unauthorized
#* @response 403 Forbidden
#* @response 500 Internal Error
#* 

function( req, res ) {
  
  
  cfg <- cxapp::.cxappconfig()
  
  
  log_attributes <- c( arch.service::dev_appnode(),
                       base::toupper(req$REQUEST_METHOD), 
                       req$REMOTE_ADDR, 
                       req$PATH_INFO )
  
  
  
  # -- Authorization
  
  if ( ! "HTTP_AUTHORIZATION" %in% names(req) ) {
    cxapp::cxapp_log("Authorization header missing", attr = log_attributes)
    res$status <- 401  # Unauthorized
    return("Authorization header missing")
  }
  
  
  auth_result <- try( cxapp::cxapp_authapi( req$HTTP_AUTHORIZATION ), silent = TRUE )
  
  if ( inherits( auth_result, "try-error" ) ) {
    cxapp::cxapp_log("Authorization failed", attr = log_attributes)
    res$status <- 401  # Unauthorized
    return("Authorization failed")
  }
  
  
  if ( ! auth_result ) {
    cxapp::cxapp_log("Access denied", attr = log_attributes)
    res$status <- 403  # Forbidden
    return("Access denied")
  }
  
  
  # - log authentication
  
  cxapp::cxapp_log( paste( "Authorized", 
                           ifelse( ! is.null( attr(auth_result, "principal") ), attr(auth_result, "principal"), "unkown" ) ),
                    attr = log_attributes )
  
  
  # - add principal to log attributes
  if ( ! is.null( attr(auth_result, "principal") ) )
    log_attributes <- append( log_attributes, attr(auth_result, "principal") )
  
  
  # -- assemble information
  lst <- list( "service" = "arch", 
               "version" = as.character(utils::packageVersion("arch.service")), 
               "database" = list() )
  
  
  # - add database pool details
  
  lst_pool <- list()
  
  dbpool <- base::get(".arch.dbpool", envir = .GlobalEnv) 
  pool_details <- capture.output(dbpool)
  
  if ( any(grepl( "checked\\s+out:", pool_details, ignore.case = TRUE)) )
    lst_pool[["active.connections"]] <- gsub( ".*checked\\s+out:\\s+(\\d+)", "\\1", 
                                              pool_details[ grepl( "checked\\s+out:", pool_details, ignore.case = TRUE) ],
                                              ignore.case = TRUE ) 
  
  if ( any(grepl( "available\\s+in\\s+pool:", pool_details, ignore.case = TRUE)) )
    lst_pool[["available.connections"]] <- gsub( ".*available\\s+in\\s+pool:\\s+(\\d+)", "\\1",
                                                 pool_details[ grepl( "available\\s+in\\s+pool:", pool_details, ignore.case = TRUE) ],
                                                 ignore.case = TRUE ) 
  
  if ( any(grepl( "max\\s+size:", pool_details, ignore.case = TRUE)) )
    lst_pool[["max.connections"]] <- gsub( ".*max\\s+size:\\s+(\\d+)", "\\1",
                                           pool_details[ grepl( "max\\s+size:", pool_details, ignore.case = TRUE) ],
                                           ignore.case = TRUE ) 
  
  
  if ( length(lst_pool) > 0 )
    lst[["database"]][["pool"]] <- lst_pool
  
  
  # - add database configuration details
  
  lst_dbcfg <- list()
  
  for( xopt in c( "db.vendor", "db.driver", "db.host", "db.port", "db.username" ) )
    lst_dbcfg[[ xopt ]] <- cfg$option( xopt, unset = "<not set>" )
  
  lst_dbcfg[["db.password"]] <- ifelse( is.na( cfg$option( "db.password", unset = NA) ), "<not set>", "<set>" )
  
  
  for( xopt in c( "db.pool.minsize", "db.pool.maxsize", "db.pool.idletimeout" ) )
    lst_dbcfg[[ xopt ]] <- cfg$option( xopt, unset = "<default>" )
  
  
  lst[["database"]][["configuration"]] <- lst_dbcfg
  
  
  
  
  res$status <- 200  # OK
  
  res$setHeader( "content-type", "application/json" )
  res$body <- jsonlite::toJSON( lst, auto_unbox = TRUE, pretty = TRUE )
  
  cxapp::cxapp_log( "Audit service information", attr = log_attributes )
  
  
  return(res)  
  
}




#* Ping service
#* 
#* @get /api/ping
#* @head /api/ping
#* 
#* @response 200 OK
#* @response 500 Internal Error
#* 

function( req, res ) {
  
  # -- truly OK
  res$status <- 200
  
}

