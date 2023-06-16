env <- Sys.getenv("MODE")
if (env == 'production') {
  setwd('/app')
  options("plumber.port" = 8000)
  print("PRODUCTION MODE")
} else {
  options("plumber.port" = 11004)
  print("DEV MODE")
}  
setwd("C:/Users/irlab/Desktop/SQL-query-app/R api/")
source("Functions_SQL.R")
library(ggplot2)
library(reshape2)
library(dplyr)
library(tseries)
library(patchwork)
library(hash)
library(zoo)
library(rlang)
library(ggforce)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(RMySQL)


con <- dbConnect(RMariaDB::MariaDB(),
                 user="rodriguez_lumar_update",
                 password="HzET7ex3wx4Mu",
                 host="lsbioweb2.unige.ch",
                 dbname="rodriguez_lumar",
                 port="3306")

#* getLevels
#* @post /getLevels
getLevels = function(column,OR =NULL,Odorant=NULL){
  
  if(column %in% c("olfactory_receptor","promotor","reporter")){
    query = paste("SELECT DISTINCT",column,"FROM conditions_info")
    
    res = dbSendQuery(con,query)
    output = dbFetch(res)
    output = output[!is.na(output[,1]),1]
    
  }else{
    print("entered here")
    if(!is.null(OR) & OR != "null"){
      if(!is.null(Odorant)& Odorant != "null"){
        string = sprintf('AND ci.olfactory_receptor IN ("%s") AND ti.odor IN ("%s")',OR,Odorant)
      }else{string = sprintf('AND ci.olfactory_receptor IN ("%s")',OR)}
      query= paste0('SELECT DISTINCT ti.odor, ti.dilution 
  FROM trial_info AS ti
   INNER JOIN conditions_info AS ci
   ON ti.exp_id = ci.exp_id ',string)
      
      res = dbSendQuery(con,query)
      output = dbFetch(res)
      output = output[!is.na(output[,1]),]
      output = output[,column]
    }else{
      query = paste("SELECT DISTINCT",column,"FROM trial_info")
      res = dbSendQuery(con,query)
      output = dbFetch(res)
      output = output[!is.na(output[,1]),1]
    }
    
  }
  
  return(output)
  
  
  # if(column =="olfactory_receptor"){
  #   return(c("DmOR2","DmOR52","DmOR9"))
  # }else  if(column == "promotor"){
  #   return(c("ORCO","DmOR52","DmOR9"))
  # }else if(column == "reporter"){
  #   return(c("GCaMP7F","GFP"))
  # }
  
  
}

#* getQuery 
#* 
#* @post /getQuery
getQuery = function(OR =NULL,Driver=NULL,Reporter=NULL,Odorant=NULL,Dilution=NULL){

  or_query = c()
  driver_query = c()
  reporter_query = c()
  odorant_query = c()
  dilution_query = c()
  
  if(!is.null(OR) & OR != "null"){
    or_query = paste0(" AND ci.olfactory_receptor IN (",paste(paste0("'",OR,"'"),collapse = ","),")")
  }
  if(!is.null(Driver) & Driver != "null"){
    driver_query = paste0(" AND ci.promotor IN (",paste(paste0("'",Driver,"'"),collapse = ","),")")
  }
  if(!is.null(Reporter) & Reporter != "null"){
    reporter_query = paste0(" AND ci.reporter IN (",paste(paste0("'",Reporter,"'"),collapse = ","),")")
  }
  if(!is.null(Odorant) & Odorant != "null"){
    odorant_query = paste0(" AND ti.odor IN (",paste(paste0("'",Odorant,"'"),collapse = ","),")")
  }
  if(!is.null(Dilution) & Dilution != "null"){
    dilution_query = paste0(" AND ti.dilution IN (",paste(paste0("'",Dilution,"'"),collapse = ","),")")
  }
  
  
  condition_query = paste0(or_query,driver_query,reporter_query,odorant_query,dilution_query)
  
  query = sprintf('SELECT f.exp_id, f.max_zscore, f.pulse_id as pulse,f.antenna_id as variable, ci.transgene_name as conditions, ti.odor as odorant, ci.olfactory_receptor, ci.remarks 
  FROM formatted_data AS f 
   INNER JOIN trial_info AS ti
   ON f.trial_id = ti.trial_id
   INNER JOIN conditions_info AS ci
   ON f.condition_id = ci.condition_id %s',condition_query)
  
  return(query)
}


#* getSamescale
#* @post /getSamescale
getSamescale = function(OR =NULL,Driver=NULL,Reporter=NULL,Odorant=NULL,Dilution=NULL,
                        export = F,raw = F) {
  
  query = getQuery(OR,Driver,Reporter,Odorant,Dilution)
  print(query)
  res = dbSendQuery(con,query)
  formatted_data = dbFetch(res)
  formatted_data = data.frame(formatted_data,stringsAsFactors = T)
  print(head(formatted_data))
  #formatted_data = read.csv("output_formatted.csv")
  
 
  formatted_data$variable = as.factor(formatted_data$variable)
  formatted_data$remarks = NA
  
  
  if(export){
    if(raw){
      cond_names = lapply(strsplit(formatted_data$conditions,"[_]"),`[`,c(1,7,8))
      formatted_data$short_cond_names = sapply(cond_names,function(x) paste(unlist(x, use.names = T),collapse = "_"))
      return(formatted_data)
    }else{
      cond_names = lapply(strsplit(formatted_data$conditions,"[_]"),`[`,c(1,7,8))
      formatted_data$short_cond_names = sapply(cond_names,function(x) paste(unlist(x, use.names = T),collapse = "_"))
      return(formatted_data)
    }
    
  }else {
    print("entered plot")
    print(head(formatted_data))
    samescale_summary(formatted_data)
  }
}








