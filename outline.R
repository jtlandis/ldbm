

#db to record projects

#db to record how samples are agumented (This will be the main Sample tracker)

#A separate db for each protocol
# Always takes input -
# May report an output if protocol results in an agumentation of a sample
# Most will just contain information relavent to protocol
# 

# db for sample submission? should it be considered a database?
#   links: sample.id, project.id

set_db_path <- function(path = here::here("ezdb/")){
  if(!dir.exists(paths = path)){
    dir.create(path=path)
  }
  assign(x = ".db_path",value = path,envir = globalenv())
}

get_db_path <- function(){
  if(!exists(".db_path",envir = globalenv())){
    message(".db_path is not set. assigned to default")
    set_db_path()
  }
  return(get(".db_path", envir = globalenv()))
}


#start ---- start

set_db_path(here::here("core_db/"))

dbm <- new("dbm", name = "core_tracker")
dbm <- db_add(dbm, new("db",
                       name = "project",
                       col_names = c("project.id","proj.description","contact","pi"),
                       col_types = c("c","c","c","c"),
                       key = "project.id"))
dbm <- db_add(dbm, new("db",
                       name = "sample_tracker",
                       col_names = c("origin","result","protocol","description"),
                       key = c("origin","result","protocol")))
dbm <- db_add(dbm, new("db",
                       name = "sample_submission",
                       col_names = c("project.id","submission.id","sample.id", "submission.name",
                                     "sample.sub.indx","submitter","expected.protocols"),
                       key = c("project.id","submission.id","sample.id")))
dbm <- db_add(dbm, new("db",
                       name = "test",
                       col_names = c("project.id","submission.id","sample.id","randomLetter"),
                       key = c("project.id","submission.id","sample.id")))
db_write(dbm@db$test, data = samp %>% select(project.id, submission.id, sample.id) %>% mutate(randomLetter= sample(LETTERS,12)),append = F)
proj <- tibble::tibble(project.id = sprintf("Pr20%04d",1:4),
                       proj.description = NA,
                       contact = "Brent",pi = "Dirk")
db_write(dbm@db$project, proj, F)
samp <- tibble::tibble(project.id = rep(proj$project.id, each = 3),
                       submission.id = rep(paste0("sub20000",1:4),each=3),
                       sample.id = sprintf("VS20%04d",1:12),
                       submission.name = rep(paste0("Sample",1:6),2), sample.sub.indx = rep(1:3,4), 
                       submitter = rep(c("brent","justin"), each = 6), expected.protocols = NA_character_)
db_write(dbm@db$sample_submission, samp, F)

dbm <- db_setdependency(dbm, dbm@db$sample_submission, dbm@db$project, links = c("project.id"="project.id"))

#testing db link

newsub <- tibble(project.id = rep(sprintf("Pr20%04d",4:5),each = 2),
                 submission.id = "sub200005",
                 sample.id = sprintf("VS20%04d",13:16),
                 submission.name = sample(letters,4),
                 sample.sub.indx = 1:4,
                 submitter = "rachele",
                 expected.protocols = NA_character_)
db_write(dbm, db.name = "sample_submission", data = newsub, append = T)




setClass("cdate", contains = "character", slots = c(date = "Date"))
setGeneric("cc", function(n, ...) standardGeneric("cc"))
setMethod("cc",
          signature(n = "character"),
          function(n){
            paste(n)
          })
setMethod("cc",
          signature(n = "numeric"),
          function(n, time){
            paste(n, "the time is:", time)
          })
