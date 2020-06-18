

setGeneric("db_add", function(dbm, db) standardGeneric("db_add"))
setMethod("db_add",
          signature(dbm = "dbm", db = "db"),
          function(dbm,db){
            nam <- names(dbm@db)
            if(!db@name %in% names(dbm@db)){
              if(is.na(db@path)){ #if no directory is set with file Make one
                newdbpath <- paste0(dbm@path,db@name,"/")
                if(!dir.exists(newdbpath)){
                  dir.create(newdbpath)
                }
                db@path <- newdbpath
              }
              dbm@db <- c(dbm@db, db)
              names(dbm@db) <- c(nam, db@name)
              dbm <- updateClass(dbm)
              saveRDS(db,file = paste0(dbm@path,db@name,".rds"))
            } else {
              warning(paste0(db@name," already exists in dbm"))
            }
            return(dbm)
})
setMethod("db_add",
          signature(dbm = "dbm", db = "character"),
          function(dbm,db){
            path <- paste0(dbm@path,db,".rds")
            if(file.exists(path)){
              if(!db %in% names(dbm@db)){
                db <- readRDS(path)
                if(is.na(db@path)){ #if no directory is set with file Make one
                  newdbpath <- paste0(dbm@path,db@name,"/")
                  if(!dir.exists(newdbpath)){
                    dir.create(newdbpath)
                  }
                  db@path <- newdbpath
                }
                nam <- names(dbm@db)
                dbm@db <- c(dbm@db, db)
                names(dbm@db) <- c(nam, db@name)
                dbm <- updateClass(dbm)
              } else {
                warning(paste0(db," already exists in dbm"))
              }
              return(dbm)
            } else {
              stop(paste0("could not find db pointer: ",path))
            }
          })

setGeneric("db_rm", function(dbm, db) standardGeneric("db_rm"))
setMethod("db_rm",
          signature(dbm = "dbm", db = "db"),
          function(dbm,db){
            nam <- db@name
            if(nam %in% names(dbm@db)){
              saveRDS(db,file = paste0(dbm@path,db@name,".rds"))
              dbm@db <- dbm@db[!names(dbm@db)%in%nam]
              dbm <- updateClass(dbm)
            } else {
              warning(paste0("db \"", nam,"\" is not present in dbm"))
            }
            return(dbm)
          })
setMethod("db_rm",
          signature(dbm = "dbm", db = "character"),
          function(dbm,db){
            if(db %in% names(dbm@db)){
              .db <- dbm@db[[db]]
              saveRDS(.db,file = paste0(dbm@path,.db@name,".rds"))
              dbm@db <- dbm@db[!names(dbm@db)%in%db]
              dbm <- updateClass(dbm)
            } else {
              warning(paste0("db \"",db,"\" is not present in dbm"))
            }
            return(dbm)
          })


setGeneric("db_colsummary", function(object) standardGeneric("db_colsummary"))
setMethod("db_colsummary",
          signature(object = "db"),
          function(object){
            n <- length(object@col_names)
            data.frame(db.names = rep(object@name, n),
                       col_names = object@col_names,
                       col_types = object@col_types,
                       key = object@col_names%in%object@key)
          })
setMethod("db_colsummary",
          signature(object = "dbm"),
          function(object){
            suppressWarnings({
              dplyr::bind_rows(lapply(object@db, db_colsummary))
            })
          })


setGeneric("db_read", function(db, ...) standardGeneric("db_read"))
setMethod("db_read",
          signature(db = "db"),
          function(db){
            path <- paste0(db@path,db@name,".tsv")
            if(!file.exists(path)){
              tib <- db@col_names %>%
                rlang::rep_named(list(character())) %>%
                tibble::as_tibble()
              
            } else {
              tib <- vroom(path, 
                           col_types = paste0(db@col_types,collapse = ""),
                           col_names = TRUE)
            }
            return(tib)
          })
setMethod("db_read",
          signature(db = "dbm"),
          function(db, db.name){
            dbm <- db
            if("db" %in% class(db.name)){
              db <- db.name
            } else if("character" %in%class(db.name)){
              if(db.name%in%names(db@db)){
                db <- db@db[[db.name]]
              } else {
                stop(paste0(db.name, " is not managed by ", dbm@name))
              }
            }
            path <- paste0(db@path,db@name,".tsv")
            if(!file.exists(path)){
              tib <- db@col_names %>%
                rlang::rep_named(list(character())) %>%
                tibble::as_tibble()
              
            } else {
              tib <- vroom(path, 
                           col_types = paste0(db@col_types,collapse = ""),
                           col_names = TRUE)
            }
            return(tib)
          })
setGeneric("db_write", function(db, data, append = TRUE, ...) standardGeneric("db_write"))
setMethod("db_write",
          signature(db  = "db",data = "ANY", append = "ANY"),
          function(db, data, append){
            path <- paste0(db@path,db@name,".tsv")
            if(!all(db@col_names %in%colnames(data))){
              stop("data must contain all col_names of db")
            }
            data <- data[,db@col_names]
            vroom::vroom_write(x = data, path = path, append = append)
          })
setMethod("db_write",
          signature(db  = "dbm",data = "ANY", append = "ANY"),
          function(db, db.name, data, append){
            dbm <- db
            if("character" %in%class(db.name)&&db.name%in%names(db@db)){
              db <- db@db[[db.name]]
            } else if("db" %in% class(db.name)){
              db <- db.name
            }
            path <- paste0(db@path,db@name,".tsv")
            if(!all(db@col_names %in%colnames(data))){
              stop("data must contain all col_names of db")
            }
            data <- data[,db@col_names]
            #before writing --- check each dependency if new data (keys) are represented
            #if not -- append changes to dependent db inserting NAs to other columns.
            
            if(length(db@dependency)>0){
              for(dep in names(db@dependency)){
                depen <- db_read(dbm, db.name = dep) 
                depen <- depen %>%
                  select(db@dependency[[dep]]) %>% distinct()
                .tmp <- data %>% select(names(db@dependency[[dep]])) %>% distinct()
                which_match <- mapply(all_within, x = depen, y = .tmp)
                if(!all(which_match)){
                  .tmpdepen <- full_join(db_read(dbm, db.name = dep),
                                         .tmp, by = invertnames(db@dependency[[dep]]))
                  db_write(dbm@db[[dep]], .tmpdepen, append = F)
                }
              }
              
            }
            vroom::vroom_write(x = data, path = path, append = append)
          })


setGeneric("db_setdependency", function(dbm, db, dependency, links) standardGeneric("db_setdependency"))
setMethod("db_setdependency",
          signature(dbm = "dbm", db = "db", dependency = "db", links = "character"),
          function(dbm, db, dependency, links){
            current.list <- dbm@db[[db@name]]@dependency
            if(length(current.list)!=0&&
               dependency@name %in% names(current.list)){
              warning(paste0(dependency@name, " is already a dependency of ", db@name,". Overwriting."))
              dbm@db[[db@name]]@dependency[[dependency@name]] <- links
            } else {
              dbm@db[[db@name]]@dependency <- c(current.list, list(links))
              names(dbm@db[[db@name]]@dependency) <- c(names(dbm@db[[db@name]]@dependency), dependency@name)
            }
            return(dbm)
          })



