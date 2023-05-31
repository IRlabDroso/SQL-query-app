env <- Sys.getenv("MODE")
if (env == 'production') {
  setwd('/app')
  options("plumber.port" = 8000)
  print("PRODUCTION MODE")
} else {
  options("plumber.port" = 11004)
  print("DEV MODE")
}  

source("Functions_SQL.R")
#library(RMySQL)
# con <- dbConnect(RMariaDB::MariaDB(),
#                  user="rodriguez_lumar_update",
#                  password="HzET7ex3wx4Mu",
#                  host="lsbioweb2.unige.ch",
#                  dbname="rodriguez_lumar",
#                  port="3306")

#* getLevels
#* @post /getLevels
getLevels = function(column){
  
  # if(column %in% c("olfactory_receptor","promotor","reporter")){
  #   query = paste("SELECT DISTINCT",column,"FROM conditions_info")
  # }else{
  #   query = paste("SELECT DISTINCT",column,"FROM trial_info")
  # }
  # 
  # res = dbSendQuery(con,query)
  # output = dbFetch(res)
  if(column =="olfactory_receptor"){
    return(c("DmOR2","DmOR52","DmOR9"))
  }else  if(column == "promotor"){
    return(c("ORCO","DmOR52","DmOR9"))
  }else if(column == "reporter"){
    return(c("GCaMP7F","GFP"))
  }
  
  
}

#* getQuery 
#* 
#* @post /getQuery
getQuery = function(OR =NULL,Driver=NULL,Reporter=NULL){

  or_query = c()
  driver_query = c()
  reporter_query = c()
  
  if(OR != "null"){
    or_query = paste0(" AND ci.olfactory_receptor IN (",paste(paste0("'",OR,"'"),collapse = ","),")")
  }
  if(Driver != "null"){
    print(Driver)
    driver_query = paste0(" AND ci.promotor IN (",paste(paste0("'",Driver,"'"),collapse = ","),")")
  }
  if(Reporter != "null"){
    reporter_query = paste0(" AND ci.reporter IN (",paste(paste0("'",Reporter,"'"),collapse = ","),")")
  }
  
  condition_query = paste0(or_query,driver_query,reporter_query)
  
  query = sprintf('SELECT f.max_zscore, f.pulse_id as pulse,f.antenna_id as variable, ci.transgene_name as conditions, ti.odor as odorant, ci.olfactory_receptor 
  FROM formatted_data AS f 
   INNER JOIN trial_info AS ti
   ON f.trial_id = ti.trial_id
   INNER JOIN conditions_info AS ci
   ON f.condition_id = ci.condition_id %s',condition_query)
  
  return(query)
}


#* getSamescale
#* @post /getSamescale
getSamescale = function(OR =NULL,Driver=NULL,Reporter=NULL,export = F,raw = F) {
  
  # query = getQuery(OR,Driver,Reporter)
  
  # res = dbSendQuery(con,query)
  # formatted_data = dbFetch(res)
  # formatted_data = data.frame(formatted_data,stringsAsFactors = T)
  formatted_data = read.csv("output_formatted.csv")
  
  cond_names = lapply(strsplit(formatted_data$conditions,"[_]"),`[`,c(1,7,8))
  formatted_data$short_cond_names = sapply(cond_names,function(x) paste(unlist(x, use.names = T),collapse = "_"))
  formatted_data$variable = as.factor(formatted_data$variable)
  
  
  if(export){
    if(raw){
      return(formatted_data)
    }else{
      print(head(formatted_data))
      return(formatted_data)
    }
    
  }else {
    samescale_summary(formatted_data)
  }
}








