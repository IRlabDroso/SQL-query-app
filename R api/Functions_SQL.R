####################### Read exp_info file ########################
Read_Exp_info = function(file=NULL){
  if(is.null(file)){
    doss = list.files(getwd(), pattern = "*.csv")
    file = doss[grep("info",doss,ignore.case = T)]
  }
  
  info.exp<-read.csv(file)
  condNum<-info.exp[1,"Conditions"]
  numOdorant<-info.exp[1,"Odorant.Trials"]
  only.trial<-dplyr::select(info.exp, contains("Trial."))
  odorant<-unlist( only.trial[1:numOdorant])
  conditions = unlist(info.exp[1,c(3:(2+condNum))])
  return(list(exp_info = info.exp,exp_id = info.exp$Exp_id,conditions =conditions ,condNum=condNum,numOdorant=numOdorant,odorant=odorant))
}

####################### Split raw csv by odors ########################
Split_CSV = function(directory,length_exp = 184){
  ### Open all csv files in the directory
  setwd(directory)
  doss <- list.files(directory, pattern = "*.csv")
  
  ### open Exp_info ###
  info.exp = Read_Exp_info()
  
  ### open Raw data CSV ###
  doss <- doss[!(doss %in% c("Exp_Infos.csv"))]
  csv = doss
  
  tabl <- read.table(csv, header=TRUE, sep=",")[-1,]
  tabl <- na.omit(tabl)
  
  ### Keep only the useful columns and rename them ###
  list_of_cols = colnames(tabl)[1]
  new_colnames = c("Timing")
  for(i in 1:((ncol(tabl)-1)/4)){
    list_of_cols = c(list_of_cols,sprintf("Channel1_R%s_IntensityMeanThrs..EGFP_R%s_IntensityMeanThrs..R",i,i))
    new_colnames = c(new_colnames,sprintf("Antenna_%s",i))
  }
  
  tabl = tabl[,list_of_cols]
  colnames(tabl) = new_colnames
  
  
  ### Add new column odorant to specify in which odorant we are for each time point ###
  create_odorant_col = function(num,input_length_exp=length_exp){
    if(num == info.exp$numOdorant){
      theory_timing_start = (input_length_exp*num - input_length_exp) / 60
      theory_timing_stop = tabl$Timing[nrow(tabl)]# take last time point if last odor
    }else{
      theory_timing_start = (input_length_exp*num - input_length_exp) / 60
      theory_timing_stop = (input_length_exp*num) / 60
    }
    
    y = rep(as.character(info.exp$odorant[num]),nrow(tabl[tabl$Timing>=theory_timing_start & tabl$Timing<=theory_timing_stop,]))
    return(y)
  }
  
  tabl$odorant = NA
  tabl$odorant = unlist(lapply((1:info.exp$numOdorant),function(x){create_odorant_col(x)}))
  
  return(tabl)
}

####################### Format Raw data set ########################

format_csv = function(csv_input,delta_F_dan = F){
  
  ### read csv ###
  exp.info = Read_Exp_info()
  #csv = read.csv(csv)
  csv = csv_input[,-ncol(csv_input)]
  
  ### Normalize with the last two ROI (forehead of flies) ###
  csv[,-1] = csv[,-1] / rowMeans(csv[,c(ncol(csv)-1,ncol(csv))]) #divide all the values by the mean of last two ROI aka foreheads ROI
  
  ### delta F ###
  if(delta_F_dan){
    ### Dan
    mean_water1 = apply(csv[csv$Timing<=((csv$Timing[1]+(184/60))),-1],2,mean,na.rm=TRUE)
    f = sweep(csv[,-1],2,mean_water1,"-")
    DF = sweep(f,2,mean_water1,"/")
  }else{
    ### Dood
    colmean = apply(csv[,-1],2,mean,na.rm=TRUE)
    f = sweep(csv[,-1],2,colmean,"-")
    DF = sweep(f,2,colmean,"/")
  }
  
  melted = melt(csv,id="Timing")
  melted = melted %>% group_by(variable) %>% mutate(ma=rollmean(as.numeric(value),25, align = "right", fill = NA))
  rollwithdf<-mutate(melted, deltaf=((as.numeric(value)-ma)/ma)) 
  rollwithdf2 <- dplyr::select(rollwithdf, Timing,variable,deltaf) 
  rollwithdf2$deltaf[is.na(rollwithdf2$deltaf)] = 0
  new_csv = pivot_wider(rollwithdf2,names_from=variable,values_from = deltaf, names_sort=FALSE  )
  
  ### Add Timing column that was not included for calculate delta F ###
  csv_DF = cbind(csv$Timing,DF)
  colnames(csv_DF)[1] = "Timing"
  
  ### set first value as 0 to have comparables values ###
  csv[,-1] = sweep(csv[,-1],2,unlist(colMeans(csv[1:20,-1])),"-")
  csv_DF[,-1] = sweep(csv_DF[,-1],2,unlist(colMeans(csv_DF[1:20,-1])),"-")
  new_csv[,-1] = sweep(new_csv[,-1],2,unlist(colMeans(new_csv[1:20,-1])),"-")
  
  print(ggplot(csv_DF,aes(x=Timing,y=Antenna_8,colour="Normalized scale"))+
          geom_line()+
          geom_line(data=csv,aes(x=Timing,y=Antenna_8,colour="Original scale"))+
          geom_line(data=new_csv,aes(Timing,Antenna_8,colour="rolling mean"))+
          labs(title = exp.info$exp_id,
               subtitle = "Comparaison original scale vs Normalized scale")+
          ylab("Antenna 8 intensity")+
          scale_color_manual(name = "Legend", values = c("Normalized scale" = "blue","Original scale"="black","rolling mean"="orange")))
  
  csv_DF = cbind(csv_DF,"odorant" = csv_input$odorant)
  new_csv = cbind(new_csv,"odorant" = csv_input$odorant)
  
  return(list(normalized = csv_DF,rolling_mean=new_csv,raw=csv))
}

####################### Finding pics ########################
rollingSlope.lm <- function(vector) {
  
  a <- coef(lm(vector ~ seq(vector)))[2]
  return(a)
  
}

acf_computation = function(csv,csv_complete,pulse_csv){
  
  i = 2
  time_csv = csv_complete[,1]
  csv = as.data.frame(cbind(Timing = time_csv,csv))
  colnames(csv)[2]= colnames(csv_complete[which(csv_complete[100,] == csv[100,2])])
  name_antenna =colnames(csv)[2]
  
  no_response = T# to know if we have a response for this odor or not 
  
  ### Finding the max autocorelation for the first pics ###
  ACF = acf(csv[,i],pl=F,lag = 500)
  ACF = as.data.frame(cbind(ACF$lag,ACF$acf))
  
  # Find the max value
  max_1 = which(ACF$V2 == max(ACF$V2[ACF$V1>380]))
  if(max_1 < 380 | max(ACF$V2[ACF$V1>380]) < 0.35){
    no_response = T
    # 409 = 184 / 60 / 3 / 0.0025 aka how much frame are in each pulse test
    max_1 = 409 #default value that seems to works properly, This is used only for no response signals so don't care much
  }
  
  ### Finding the max autocorelation for the second and third pics ###
  ACF = acf(csv[csv$Timing>csv$Timing[max_1],i],pl=F,lag = 500)
  ACF = as.data.frame(cbind(ACF$lag,ACF$acf))
  max_2 = which(ACF$V2 == max(ACF$V2[ACF$V1>100]))
  
  if(max_2 < 380 | max(ACF$V2[ACF$V1>380]) < 0.35){
    # 409 = 184 / 60 / 3 / 0.0025 aka how much frame are in each pulse test
    no_response = T
    max_2 = 409 #default value that seems to works properly, This is used only for no response signals so don't care much
  }
  
  First_pic = csv[csv$Timing<=csv$Timing[max_1],]
  Second_pic = csv[csv$Timing>csv$Timing[max_1] & csv$Timing<=csv$Timing[(max_1+max_2)],]
  Third_pic = csv[csv$Timing>csv$Timing[(max_1+max_2)],]
  
  ### Calculate rolling slope for each pulse ###
  First_pic = First_pic %>%mutate(Slope.lm = First_pic[[name_antenna]])%>% mutate(Slope.lm = rollapply(Slope.lm, width=10, FUN=rollingSlope.lm, fill=NA))
  Second_pic = Second_pic %>%mutate(Slope.lm = Second_pic[[name_antenna]])%>% mutate(Slope.lm = rollapply(Slope.lm, width=10, FUN=rollingSlope.lm, fill=NA))
  Third_pic = Third_pic %>%mutate(Slope.lm = Third_pic[[name_antenna]])%>% mutate(Slope.lm = rollapply(Slope.lm, width=10, FUN=rollingSlope.lm, fill=NA))
  
  ### Find the max slope and set as 0 to synchronize the pulses ###
  if(no_response){#theory should be each 409
    First_pic$Timing = First_pic$Timing - First_pic$Timing[(nrow(First_pic)/2)]
    Second_pic$Timing = Second_pic$Timing - Second_pic$Timing[(nrow(Second_pic)/2)]
    Third_pic$Timing = Third_pic$Timing - Third_pic$Timing[(nrow(Third_pic)/2)]
  }else{
    First_pic$Timing = First_pic$Timing - First_pic$Timing[which.max(First_pic$Slope.lm)]
    Second_pic$Timing = Second_pic$Timing - Second_pic$Timing[which.max(Second_pic$Slope.lm)]
    Third_pic$Timing = Third_pic$Timing - Third_pic$Timing[which.max(Third_pic$Slope.lm)]
  }
  
  
  ### Normalize the values with new 0 as reference ###
  First_0 = which(First_pic$Timing==0)
  Second_0 = which(Second_pic$Timing==0)
  Third_0 = which(Third_pic$Timing==0)
  First_pic[,i] = First_pic[,i] - mean(First_pic[(First_0-15):(First_0-2),i])
  Second_pic[,i] = Second_pic[,i] - mean(Second_pic[(Second_0-15):(Second_0-2),i])
  Third_pic[,i] = Third_pic[,i] - mean(Third_pic[(Third_0-15):(Third_0-2),i])
  
  
  ### Merge pics + create a melted DF ###
  dfs = list(First_pic[,c("Timing",name_antenna)],Second_pic[,c("Timing",name_antenna)],Third_pic[,c("Timing",name_antenna)])
  
  melted_CSV = Reduce(function(x,y) merge(x,y,by="Timing"),dfs)
  colnames(melted_CSV) = c("Timing",paste(name_antenna,c("pic_1","pic_2","pic_3"),no_response,sep = "/"))#use "/" to split in next function
  
  ### return the merged CSV  ###
  
  return(melted_CSV)
  
}

acf_computation_5 = function(csv,csv_complete,pulse_csv){
  
  i = 2
  time_csv = csv_complete[,1]
  csv = as.data.frame(cbind(Timing = time_csv,csv))
  colnames(csv)[2]= colnames(csv_complete[which(csv_complete[100,] == csv[100,2])])
  name_antenna =colnames(csv)[2]
  
  no_response = F# to know if we have a response for this odor or not 
  
  ### Finding the max autocorelation for the first pics ###
  ACF = acf(csv[,i],pl=T,lag = 400)
  ACF = as.data.frame(cbind(ACF$lag,ACF$acf))
  
  # Find the max value
  max_1 = which(ACF$V2 == max(ACF$V2[ACF$V1>100]))
  if(max_1 < 100 | max(ACF$V2[ACF$V1>100]) < 0.35){
    no_response = T
    # 409 = 184 / 60 / 3 / 0.0025 aka how much frame are in each pulse test
    max_1 = 210 #default value that seems to works properly, This is used only for no response signals so don't care much
  }else if(max_1 <300){
    max_1 = 210
  }
  
  ### Finding the max autocorelation for the second and third pics ###
  ACF = acf(csv[csv$Timing>csv$Timing[max_1],i],pl=F,lag = 400)
  ACF = as.data.frame(cbind(ACF$lag,ACF$acf))
  max_2 = which(ACF$V2 == max(ACF$V2[ACF$V1>100]))
  
  if(max_2 > 200 | max(ACF$V2[ACF$V1>100]) < 0.35){
    # 409 = 184 / 60 / 3 / 0.0025 aka how much frame are in each pulse test
    no_response = T
    max_2 = 141 #default value that seems to works properly, This is used only for no response signals so don't care much
  }
  
  ### Finding the max autocorelation for the second and third pics ###
  ACF = acf(csv[csv$Timing>csv$Timing[(max_1+max_2)],i],pl=F,lag = 300)
  ACF = as.data.frame(cbind(ACF$lag,ACF$acf))
  max_3 = which(ACF$V2 == max(ACF$V2[ACF$V1>100]))
  
  if(max_3 < 100 | max(ACF$V2[ACF$V1>100]) < 0.35){
    # 409 = 184 / 60 / 3 / 0.0025 aka how much frame are in each pulse test
    no_response = T
    max_3 = 141 #default value that seems to works properly, This is used only for no response signals so don't care much
  }
  
  
  ### Finding the max autocorelation for the second and third pics ###
  ACF = acf(csv[csv$Timing>csv$Timing[(max_1+max_2+max_3)],i],pl=F,lag = 300)
  ACF = as.data.frame(cbind(ACF$lag,ACF$acf))
  max_4 = which(ACF$V2 == max(ACF$V2[ACF$V1>100]))
  
  if(max_4 < 100 | max(ACF$V2[ACF$V1>100]) < 0.35){
    # 409 = 184 / 60 / 3 / 0.0025 aka how much frame are in each pulse test
    no_response = T
    max_4 = 141 #default value that seems to works properly, This is used only for no response signals so don't care much
  }
  
  First_pic = csv[csv$Timing<=csv$Timing[max_1],]
  Second_pic = csv[csv$Timing>csv$Timing[max_1] & csv$Timing<=csv$Timing[(max_1+max_2)],]
  Third_pic = csv[csv$Timing>csv$Timing[(max_1+max_2)] & csv$Timing<=csv$Timing[(max_1+max_2+max_3)],]
  Fourth_pic = csv[csv$Timing>csv$Timing[(max_1+max_2+max_3)] & csv$Timing<=csv$Timing[(max_1+max_2+max_3+max_4)],]
  Fifth_pic = csv[csv$Timing>csv$Timing[(max_1+max_2+max_3+max_4)],]
  
  
  ### Calculate rolling slope for each pulse ###
  First_pic = First_pic %>%mutate(Slope.lm = First_pic[[name_antenna]])%>% mutate(Slope.lm = rollapply(Slope.lm, width=10, FUN=rollingSlope.lm, fill=NA))
  Second_pic = Second_pic %>%mutate(Slope.lm = Second_pic[[name_antenna]])%>% mutate(Slope.lm = rollapply(Slope.lm, width=10, FUN=rollingSlope.lm, fill=NA))
  Third_pic = Third_pic %>%mutate(Slope.lm = Third_pic[[name_antenna]])%>% mutate(Slope.lm = rollapply(Slope.lm, width=10, FUN=rollingSlope.lm, fill=NA))
  Fourth_pic = Fourth_pic %>%mutate(Slope.lm = Fourth_pic[[name_antenna]])%>% mutate(Slope.lm = rollapply(Slope.lm, width=10, FUN=rollingSlope.lm, fill=NA))
  Fifth_pic = Fifth_pic %>%mutate(Slope.lm = Fifth_pic[[name_antenna]])%>% mutate(Slope.lm = rollapply(Slope.lm, width=10, FUN=rollingSlope.lm, fill=NA))
  
  
  
  ### Find the max slope and set as 0 to synchronize the pulses ###
  if(no_response){#theory should be each 409
    First_pic$Timing = First_pic$Timing - First_pic$Timing[(nrow(First_pic)/2)]
    Second_pic$Timing = Second_pic$Timing - Second_pic$Timing[(nrow(Second_pic)/2)]
    Third_pic$Timing = Third_pic$Timing - Third_pic$Timing[(nrow(Third_pic)/2)]
    Fourth_pic$Timing = Fourth_pic$Timing - Fourth_pic$Timing[(nrow(Fourth_pic)/2)]
    Fifth_pic$Timing = Fifth_pic$Timing - Fifth_pic$Timing[(nrow(Fifth_pic)/2)]
  }else{
    First_pic$Timing = First_pic$Timing - First_pic$Timing[which.max(First_pic$Slope.lm)]
    Second_pic$Timing = Second_pic$Timing - Second_pic$Timing[which.max(Second_pic$Slope.lm)]
    Third_pic$Timing = Third_pic$Timing - Third_pic$Timing[which.max(Third_pic$Slope.lm)]
    Fourth_pic$Timing = Fourth_pic$Timing - Fourth_pic$Timing[which.max(Fourth_pic$Slope.lm)]
    Fifth_pic$Timing = Fifth_pic$Timing - Fifth_pic$Timing[which.max(Fifth_pic$Slope.lm)]
  }
  
  
  ### Normalize the values with new 0 as reference ###
  First_0 = which(First_pic$Timing==0)
  Second_0 = which(Second_pic$Timing==0)
  Third_0 = which(Third_pic$Timing==0)
  Fourth_0 = which(Fourth_pic$Timing==0)
  Fifth_0 = which(Fifth_pic$Timing==0)
  First_pic[,i] = First_pic[,i] - mean(First_pic[(First_0-15):(First_0-2),i])
  Second_pic[,i] = Second_pic[,i] - mean(Second_pic[(Second_0-15):(Second_0-2),i])
  Third_pic[,i] = Third_pic[,i] - mean(Third_pic[(Third_0-15):(Third_0-2),i])
  Fourth_pic[,i] = Fourth_pic[,i] - mean(Fourth_pic[(Fourth_0-15):(Fourth_0-2),i])
  Fifth_pic[,i] = Fifth_pic[,i] - mean(Fifth_pic[(Fifth_0-15):(Fifth_0-2),i])
  
  
  ### Merge pics + create a melted DF ###
  dfs = list(First_pic,Second_pic,Third_pic,Fourth_pic,Fifth_pic)
  
  
  #use "/" to split in next function
  
  ### return the merged CSV  ###
  
  
  names(dfs) <- paste(name_antenna, seq_along(dfs), sep="")
  melted_CSV = data.table::dcast(data.table::rbindlist(dfs, idcol="id"), Timing ~ id, value.var = name_antenna)
  colnames(melted_CSV) = c("Timing",paste(name_antenna,c("pic_1","pic_2","pic_3","pic_4","pic_5"),no_response,sep = "/"))
  
  return(melted_CSV)
  
}


Finding_pics =function(csv_input,exp_odorant = NULL,only_trace = F,n_pulse=5){
  
  ### Read exp_info ###
  exp.info = Read_Exp_info()
  
  ### Load csv and modify it ###
  csv = csv_input[csv_input$odorant == exp_odorant,]
  csv = csv[,-c(ncol(csv))]
  csv[,-c(1,ncol(csv))] = as.data.frame(scale(csv[,-c(1,ncol(csv))],scale = F))
  csv$Timing = rank(csv$Timing)# to get matching timings between pulses
  
  ### Create new csv that keep pulse information ###
  pulse_csv = data.frame()
  
  ### apply each conditions to each antenna ### 
  for(i in 1:exp.info$condNum){
    if(i==1){
      colnames(csv)[2:(exp.info$exp_info$Antennas+1)] = paste0(colnames(csv[,-1])[1:exp.info$exp_info$Antennas],sprintf(".%s",exp.info$conditions[i]))
    }else{
      colnames(csv)[((i-1)*exp.info$exp_info$Antennas+2):(i*exp.info$exp_info$Antennas+1)] = paste0(colnames(csv)[((i-1)*exp.info$exp_info$Antennas+2):(i*exp.info$exp_info$Antennas+1)],sprintf(".%s",exp.info$conditions[i]))
    }
  }
  
  if(!only_trace & max(csv$Timing)>700 ){
    if(n_pulse ==3){
      computed_pics = apply(csv[,2:(ncol(csv)-2)], 2, function(x)as.data.frame(acf_computation(x,csv,pulse_csv)))
      pulse_csv = reduce(computed_pics,left_join,by="Timing")
      
    }else if(n_pulse ==5){
      computed_pics = apply(csv[,2:(ncol(csv)-2)], 2, function(x)as.data.frame(acf_computation_5(x,csv,pulse_csv)))
      pulse_csv = reduce(computed_pics,left_join,by="Timing")
    }
    
  }else{
    if(max(csv$Timing)<700){
      print("Error the experiment is not long enough !")
    }
  }
  
  return(list(csv=csv,pulse_csv=pulse_csv))
}

####################### Calculating the z_score #######################

Z_score_calculation = function(pulse_csv){
  ### Melt the dataset ###
  melted_csv= melt(pulse_csv,id.vars = "Timing")
  
  ### Formatting csv adding pulse information ###
  pulse = unique(sapply(strsplit(as.character(melted_csv$variable),"/"), `[`, 2))
  melted_csv$pulse = unlist(pulse[match(sapply(strsplit(as.character(melted_csv$variable),"/"), `[`, 2),pulse)])
  melted_csv$pulse = as.factor(melted_csv$pulse)
  
  ### Formatting csv adding conditions information ###
  melted_csv$variable = sapply(strsplit(as.character(melted_csv$variable),"/"), `[`, 1)
  conditions = unique(sapply(strsplit(as.character(melted_csv$variable),"[.]"), `[`, 2))
  melted_csv$conditions = unlist(conditions[match(sapply(strsplit(as.character(melted_csv$variable),"[.]"), `[`, 2),conditions)])
  melted_csv$conditions = factor(melted_csv$conditions,levels=unique(melted_csv$conditions))
  
  ### Taking rid of long names to keep antenna id ###
  melted_csv$variable = sapply(strsplit(as.character(melted_csv$variable),"[.]"), `[`, 1)
  melted_csv$variable = factor(melted_csv$variable,levels = unique(melted_csv$variable))
  melted_csv$value = melted_csv$value+1
  
  melted_csv = melted_csv[!is.na(melted_csv$value),]
  
  ### calculation of standard deviation + mean of each antenna + condition ###
  z_sd = melted_csv[melted_csv$Timing <0 & melted_csv$Timing >-150,] %>% group_by(variable,conditions) %>% summarize(sd = sd(value))
  z_mean = melted_csv[melted_csv$Timing <0 & melted_csv$Timing >-150,] %>% group_by(variable,conditions) %>% summarize(mean = mean(value))
  # z_sd = melted_csv %>% group_by(variable,pulse,conditions) %>% summarize(sd = sd(value))
  # z_mean = melted_csv %>% group_by(variable,pulse,conditions) %>% summarize(mean = mean(value))
  
  ### compute z_score with usual formula ###
  melted_csv = left_join(melted_csv,z_sd,by = c("variable","conditions")) %>% 
    left_join(z_mean,by = c("variable","conditions")) %>%
    mutate(z_score = (value-mean)/sd)
  
  ### New max value for normal + z_score ###
  
  # melted_csv = melted_csv %>% group_by(variable,pulse,conditions) %>% mutate(
  #   value_max_pos = which.max(value),
  #   zscore_max_pos = which.max(z_score))
  
  melted_csv = melted_csv %>% group_by(variable,pulse,conditions) %>% mutate(
    value_max_pos = (which.max(value[Timing>-10&Timing<50])-Timing[1]-9),
    zscore_max_pos = (which.max(z_score[Timing>-10&Timing<50])-Timing[1]-9))

  melted_csv = melted_csv %>% group_by(variable,pulse,conditions) %>% mutate(
    max_value = mean(value[(unique(value_max_pos)-5):(unique(value_max_pos)+5)],na.rm=T),
    max_zscore = mean(z_score[(unique(zscore_max_pos)-5):(unique(zscore_max_pos)+5)]),na.rm=T)
  
  ### add max value for normal + z_score ###
  #melted_csv = melted_csv %>% group_by(variable,pulse,conditions) %>% mutate(max_value = max(value),max_zscore = max(z_score))
  melted_csv = melted_csv %>% group_by(variable,pulse,conditions) %>% mutate(mean_value = mean(value[Timing>0&Timing<30]),mean_z_score = mean(z_score[Timing>-10&Timing<50]))
  
  return(melted_csv)
}

####################### Creating the plots #######################

### 1 First plots (plot_trace) for 1 odor ###
PlotTrace = function(csv,computed=T,combined = T,z_score = F) {
  ### read exp_info to know how many flies by conditions ###
  exp_info = Read_Exp_info()
  num_antenna = exp_info$exp_info$Antennas
  
  if(computed){
    ### load correct data ###
    melted_csv = csv$pulse_csv
    
    ### create xtitle variable aka conditions ###
    xtitle = unique(melted_csv$conditions)
    
    ### create ggplot ###
    if(z_score){
      if(combined){
        p1 = ggplot(melted_csv,aes(Timing,z_score,col=pulse))
      }else{
        melted_csv = melted_csv %>% group_by(variable) %>% mutate(Timing = index(Timing))
        p1 = ggplot(melted_csv,aes(Timing,z_score))
      }
    }else{
      if(combined){
        p1 = ggplot(melted_csv,aes(Timing,value,col=pulse))
      }else{
        melted_csv = melted_csv %>% group_by(variable) %>% mutate(Timing = index(Timing))
        p1 = ggplot(melted_csv,aes(Timing,value))
      }
    }
    
    
  }else{
    ### Take off forehead ROIs ###
    csv$csv = csv$csv[,-c(ncol(csv$csv)-1,ncol(csv$csv))]
    
    ### Melt the dataset ###
    melted_csv = melt(csv$csv,id.vars = "Timing")
    
    ### Taking rid of long names to keep antenna id ###
    xtitle = unique(sapply(strsplit(as.character(melted_csv$variable),"[.]"), `[`, 2))
    melted_csv$variable = sapply(strsplit(as.character(melted_csv$variable),"[.]"), `[`, 1)
    melted_csv$variable = factor(melted_csv$variable,levels = unique(melted_csv$variable))
    
    ### create ggplot ###
    p1 = ggplot(melted_csv,aes(Timing,value))
  }
  
  
  
  ### finish ggplot ###
  for(p in 1:length(xtitle)){#number of pages 
    print(p1+
            geom_line()+
            xlab(xtitle[p])+
            theme(axis.title.y = element_blank(),
                  axis.text.y = element_text(size=13),
                  axis.text.x = element_text(size=12),
                  #axis.ticks.y = element_blank(),
                  strip.text.y = element_text(angle = 0),
                  axis.title.x = element_text(size = 15, face="bold" ))+
            facet_wrap_paginate(~ variable,ncol = 2,nrow = ceiling(num_antenna/2),page = p)
    )
  }
}

####################### Creating the plots #######################
### 2 combined all antennas of same conditions ###

PlotTraceCondition = function(csv,groupBy = "pulse",z_score = T){
  ### load dataset ###
  melted_csv = csv$pulse_csv
  
  ### create xtitle variable aka conditions ###
  xtitle = unique(melted_csv$conditions)
  
  if(groupBy == "pulse"){
    if(z_score){
      p1 = ggplot(melted_csv,aes(Timing,z_score,group=interaction(pulse,variable),col=pulse))
      label = "z score"
    }else{
      p1 = ggplot(melted_csv,aes(Timing,value,group=interaction(pulse,variable),col=pulse))
      label = "Normalized score"
    }
    print(p1+
            geom_line(size=0.3)+
            labs(ylab = label)+
            theme(strip.text = element_text(size=5))+
            facet_wrap(~ conditions)
          
    )
  }else if(groupBy == "antenna"){
    ### create a list of ggplot for each conditions ###
    if(z_score){
      ggList <- lapply(split(melted_csv, melted_csv$conditions), function(i) {
        ggplot(i, aes(Timing,z_score,group=interaction(pulse,variable),colour=variable)) + 
          geom_line(size=0.3)+
          labs(ylab = "z_score",title = unique(i$conditions),colour = "Antenna ID")+
          theme(plot.title = element_text(size =5,face = "bold"))
      })
    }else{
      ggList <- lapply(split(melted_csv, melted_csv$conditions), function(i) {
        ggplot(i, aes(Timing,value,group=interaction(pulse,variable),colour=variable)) + 
          geom_line(size=0.3)+
          labs(ylab = "normalized score",title = unique(i$conditions),colour = "Antenna ID")+
          theme(plot.title = element_text(size =5,face = "bold"))
      })
    }
    
    ### grid them together ###
    print(cowplot::plot_grid(plotlist = ggList,align="h",ncol = 2))
  }
}

####################### Creating the plots #######################

### 3 resume all responses ###

PlotResume = function(csv,groupby = "antenna",z_score = T){
  
  ### load dataset ###
  melted_csv = csv$pulse_csv
  
  ### compute the maximum by each category ###
  if(z_score){
    max_value_csv = melted_csv%>% group_by(variable,pulse,conditions) %>% summarize(max = max(max_zscore))
    ylabel = "Z score max"
  }else{
    max_value_csv = melted_csv%>% group_by(variable,pulse,conditions) %>% summarize(max = max(max_value))
    ylabel = "normalized max"
  }
  
  ### plot the results based on users choices ###
  if(groupby=="antenna"){
    p1 = ggplot(max_value_csv,aes(variable,max))+
      geom_boxplot(coef=0)+
      geom_jitter(aes(colour=pulse),shape=16, position=position_jitter(0))+
      labs(ylab=ylabel)+
      theme(axis.text.x = element_text(angle = 45,hjust = 1),
            axis.title.x = element_blank())+
      facet_grid(~ conditions,scales="free_x")
    print(p1)
    
  }else{
    p1 = ggplot(max_value_csv,aes(conditions,max))+
      geom_boxplot()+
      geom_jitter(aes(colour=pulse),shape=16, position=position_jitter(0))+
      labs(ylab=ylabel)+
      theme(axis.text.x = element_text(angle = 45,hjust = 1),
            axis.title.x = element_blank())
    print(p1)
    
  }
  
  return(p1)
}

####################### Creating the plots #######################

### 4 plot before cutting ###
plot_before_cutting = function(raw=null,alone=F){
  
  ### open Exp_info ###
  info.exp = Read_Exp_info()
  
  ### open Raw data CSV ###
  if(alone){
    raw = Split_CSV(getwd())
    raw = raw[,-c((ncol(raw)-1):ncol(raw))]
  }
  
  raw_new = melt(raw[,-ncol(raw)],id.vars = "Timing")
  
  pdf("Plots/plot_before_cutting.pdf",width = 18,height = 15)
  print(ggplot(raw_new,aes(Timing,value,col=variable))+
          geom_line(size=0.1)+
          ylab("fluo")+
          theme(legend.position = "none"))
  
  print(ggplot(raw_new,aes(Timing,value))+
          geom_line(aes(color=variable),size=0.1)+
          facet_grid(variable~., scales = "free")+
          ylab("fluo")+
          theme(legend.position = "none",
                strip.text.y = element_blank()))
  dev.off()
}

####################### PDF Creation functions ########################

create_pdf = function(csv_DF,odorant,z_score = F,name_file,n_pulse){
  
  dataset = Finding_pics(csv_input = csv_DF$rolling_mean,exp_odorant = as.character(odorant),n_pulse=n_pulse)
  dataset_normalized = Finding_pics(csv_input = csv_DF$normalized,exp_odorant = as.character(odorant),only_trace = T,n_pulse=n_pulse)
  dataset$pulse_csv = Z_score_calculation(dataset$pulse_csv)
  
  ### Create PDF ###
  pdf(name_file)
  PlotTrace(csv = dataset_normalized,computed=F, combined = F,z_score = z_score)
  PlotTrace(csv = dataset,computed=T, combined = F,z_score = z_score)
  PlotTrace(csv = dataset,computed=T, combined = T,z_score = z_score)
  PlotTraceCondition(csv=dataset,groupBy = "pulse",z_score = z_score)
  PlotTraceCondition(csv=dataset,groupBy = "antenna",z_score = z_score)
  res_antenna = PlotResume(csv=dataset,groupby = "antenna",z_score = z_score)
  res_cond = PlotResume(csv=dataset,groupby = "condition",z_score = z_score)
  dev.off()
  name_antenna_resume = paste0("Antenna_",name_file)
  name_condition_resume = paste0("Condition_",name_file) 
  
  return(df = dataset$pulse_csv)
}


samescale_summary = function(for_resume_final,without_water=F){
  ### Create short conditions names ###
  cond_names =lapply(strsplit(for_resume_final$conditions,"[_]"), `[`, c(1,7,8))
  short_cond_names = sapply(cond_names,function(x) paste(unlist(x, use.names = TRUE), collapse = "_"))
 
  short_cond_names = paste(short_cond_names,for_resume_final$exp_id,sep = "_")
  
  #levels(for_resume_final$conditions) = short_cond_names
  for_resume_final$short_cond_names = as.factor(short_cond_names)
  levels(for_resume_final$short_cond_names) = unique(short_cond_names)
  print(for_resume_final$short_cond_names)
  
  ### get the remarks is some exist ###
  #exp.info = Read_Exp_info()$exp_info
  #remarks = unlist(lapply(strsplit(unlist(strsplit(exp.info$Remarks,"[/]")),"cond_[1-9]_"), `[`, 2))
  
  ### Convert odorant to factor + change antenna names by their number only ###
  for_resume_final$odorant = factor(for_resume_final$odorant,levels = unique(for_resume_final$odorant))
  levels(for_resume_final$variable) = c(1:length(levels(for_resume_final$variable)))
  
  print(table(for_resume_final$short_cond_names))
  ### eliminates duplicates ###
  for_resume_final = unique(for_resume_final)
  
  ### generate samescale plot ###
  png("../frontend/src/assets/images/samescale_summary_1.png",width = 18,height = 15,units = "in",res=100)
  
  
  print(ggplot(for_resume_final,aes(short_cond_names,max_zscore,fill=short_cond_names))+
          geom_boxplot()+
          theme(axis.text.x = element_text(angle=45,hjust=1),
                axis.title.x = element_blank(),
                legend.title = element_blank(),
                legend.position="bottom",
                legend.direction="horizontal",
                axis.text = element_text(size=20),
                strip.text = element_text(size=30),
                strip.text.x = element_text(angle=90),
                strip.background = element_blank(),
                legend.text = element_text(size=20),
                axis.title = element_text(size=20))+
          facet_wrap(~ odorant,scales="free_x",nrow = 1)
  )
  dev.off()
  
  png("../frontend/src/assets/images/samescale_summary_2.png",width = 18,height = 15,units = "in",res=100)
  plot_list = c()
  max_value = max(for_resume_final$max_zscore)

  for(i in 1:length(levels(for_resume_final$short_cond_names))){
    
    ### create new df and add score column aka if double the mean of water 1 value = green color ###
    df =for_resume_final[for_resume_final$short_cond_names == levels(for_resume_final$short_cond_names)[i],]
    color = df %>% group_by(odorant,variable,short_cond_names) %>% mutate(Mean = mean(max_zscore,na.rm=T))
    water_mean_sd = mean(color$Mean[grep("water",color$odorant)])+2*sd(color$Mean[grep("water",color$odorant)])
    water_mean_sd_4 = mean(color$Mean[grep("water",color$odorant)])+4*sd(color$Mean[grep("water",color$odorant)])
    color = color %>% mutate(score = as.factor(ifelse(Mean > water_mean_sd,
                                                      ifelse(Mean >water_mean_sd_4,1,0.5),0)))
    ### change max values as mean for waters ###
    #color$max_zscore[grep("water",color$odorant)] = color$mean_z_score[grep("water",color$odorant)]
    
    ### take out waters ###
    if(without_water){
      color = color[-grep("water",color$odorant),]
    }
    
    
    cols = c("0"="white","0.5"="yellow","1"="green")
    
    if(i == 1){
      p = ggplot(color,aes(variable,max_zscore,fill=score))+
        geom_boxplot()+
        ylab(levels(for_resume_final$short_cond_names)[i])+
        scale_y_continuous(limits = c(-1,max_value))+
        theme(axis.title.x = element_blank(),
              axis.text = element_text(size=20),
              strip.text = element_text(size=30),
              strip.text.x = element_text(angle=90),
              strip.background = element_blank(),
              legend.text = element_text(size=20),
              axis.title = element_text(size=20),
              legend.position = "none")+
        facet_grid(~odorant,scales="free")+
        scale_fill_manual(values = cols)
    }else{
      p = ggplot(color,aes(variable,max_zscore,fill=score))+
        geom_boxplot()+
        scale_y_continuous(limits = c(-1,max_value))+
        ylab(levels(for_resume_final$short_cond_names)[i])+
        theme(axis.title.x = element_blank(),
              strip.background = element_blank(),
              strip.text.x = element_blank(),
              axis.text = element_text(size=20),
              strip.text = element_text(size=20),
              legend.text = element_text(size=20),
              axis.title = element_text(size=20),
              legend.position = "none")+
        facet_grid(~odorant,scales="free")+
        scale_fill_manual(values = cols)
    }
    
    plot_list = c(plot_list,list(p))
  }
  
  ### assemble together each condition plot
  print(cowplot::plot_grid(plotlist = plot_list ,nrow =length(levels(for_resume_final$short_cond_names)),rel_heights = c(1,.7,.7) ))
  dev.off()
  ### create diff from endogenous plot 
  
  endo_cond_name = levels(for_resume_final$short_cond_names)[grep("endo",levels(for_resume_final$short_cond_names),ignore.case = T)]
  if(length(endo_cond_name)==0){
    
  }else{
    png("Plots/samescale_summary_3.png",width = 18,height = 15,units = "in",res=100)
    endo_normalized = for_resume_final %>% group_by(odorant) %>% mutate(fold_change = (max_zscore- max_zscore[short_cond_names == endo_cond_name])/max_zscore[short_cond_names == endo_cond_name])
    
    stat.test = endo_normalized %>% 
      group_by(odorant) %>% 
      t_test(fold_change ~ short_cond_names, ref.group = endo_cond_name) %>%
      adjust_pvalue(method = "bonferroni") %>%
      add_significance()%>%
      add_xy_position(x="short_cond_names")
    print(endo_normalized$short_cond_names)
    
    print(ggplot(endo_normalized,aes(x=short_cond_names,y=fold_change))+
            geom_boxplot(aes(fill=short_cond_names))+
            theme(axis.text.x = element_text(angle=45,hjust=1),
                  axis.title.x = element_blank(),
                  legend.title = element_blank(),
                  legend.position="bottom",
                  legend.direction="horizontal",
                  axis.text = element_text(size=20),
                  strip.text = element_text(size=30),
                  strip.text.x = element_text(angle=45),
                  strip.background = element_blank(),
                  legend.text = element_text(size=20),
                  axis.title = element_text(size=20))+
            facet_wrap(~ odorant,scales="free_x",nrow = 1)+
            stat_pvalue_manual(stat.test,hide.ns = T)
    )
    
    dev.off()
  }
  
  
  return(for_resume_final)
}

####################### Generating complete pipeline #######################

pipeline = function(z_score = F,n_pulse=3,length_exp=184){
  
  ### load all datasets needed ###
  exp.info = Read_Exp_info()
  csv = Split_CSV(getwd(),length_exp = length_exp)
  csv_DF = format_csv(csv=csv)
  
  ### create file names ###
  nametags <- LETTERS
  for(i in 1:25){
    nametags <- append(nametags,LETTERS)
  }
  nametags2 <- nametags
  nametags <- sort(nametags)
  for (i in 1:length(nametags)){
    nametags[i] <- paste(nametags[i], nametags2[i], sep="")
  }
  
  ### Create the directory of the plots if not exist ###
  if(!dir.exists("Plots")){
    dir.create("Plots")
  }
  
  ### compute graph for each odorant ###
  for_resume_final = c()
  for(i in 1:exp.info$numOdorant){
    ### Create PDF ###
    name_file = paste0("Plots/",paste(nametags[i],exp.info$odorant[i],exp.info$exp_id,sep = "_"),".pdf")
    print(name_file)
    for_resume = create_pdf(csv_DF,exp.info$odorant[i],z_score = z_score,name_file=name_file,n_pulse=n_pulse)
    
    ### for resume formatting + saved ###
    for_resume = for_resume %>% select(variable,conditions,pulse,max_zscore,mean_z_score,max_value,mean_value)
    for_resume$odorant = exp.info$odorant[i]
    for_resume_final = rbind(for_resume_final,for_resume)
    
  }
  
  plot_before_cutting(raw = csv)
  for_resume_return = samescale_summary(for_resume_final)
  return(for_resume_return)
}

### Load exp_info into DB ###
Export_exp_info = function(con){
  
  exp_info = Read_Exp_info()
  exp_info_db = exp_info[,c("Exp_id","Conditions","Antennas","Odorant.Trials")]
  names(exp_info_db) = c("exp_id","conditions","antennas","odorant_trials")
  
  query = dbSendQuery(con,paste0("SELECT * FROM raw_data WHERE exp_id = '",exp_info$exp_id,"'"))
  res = dbFetch(query)
  i = 0
  if(nrow(res)>0){
    while(nrow(res)>0){
      ### if id already present ###
      i = i +1
      print("raw_data already present")
      exp_info_change = exp_info$exp_info
      exp_info_change$exp_id = paste0(exp_info$exp_id,"_",i)
      query = dbSendQuery(con,paste0("SELECT * FROM raw_data WHERE exp_id = '",exp_info_change$exp_id,"'"))
      res = dbFetch(query)
    } 
    
    write.csv(exp_info_change,"Exp_infos_test.csv",row.names = F)
  }
  
  dbAppendTable(con,"exp_info",exp_info_db)
  
}

### Load trial_info into DB ###
Export_trial_info = function(con){
  exp_info = Read_Exp_info()$exp_info
  trial_info =as.data.frame(t(exp_info[,grep("Trial.",colnames(exp_info),fixed = T)]))
  names(trial_info)[1] = "odor"
  trial_info$exp_id = exp_info$Exp_id
  trial_info$trial_id = paste(trial_info$exp_id,"trial",1:nrow(trial_info),sep = "_")
  trial_info$dilution = ifelse(is.na(unlist(lapply(strsplit(trial_info$odor,"[_]"), `[`, 2))),
                               3,
                               unlist(lapply(strsplit(trial_info$odor,"[_]"), `[`, 2)))
  trial_info$dilution = ifelse(lapply(strsplit(trial_info$dilution,"e"),`[`,1) == "", 3,
                               unlist(lapply(strsplit(trial_info$dilution,"e"),`[`,1)))
  trial_info$odor = unlist(lapply(strsplit(trial_info$odor,"[_]"), `[`, 1))
  rownames(trial_info) = NULL
  
  dbAppendTable(con,"trial_info",trial_info)
  return(trial_info)
}

### Load condition_info into DB ###
Export_condition_info = function(con){
  exp_info = Read_Exp_info()$exp_info
  condition_info =as.data.frame(t(exp_info[,grep("Condition.",colnames(exp_info),fixed = T)]))
  names(condition_info)[1] = "transgene_name"
  condition_info$exp_id = exp_info$Exp_id
  condition_info$condition_id = paste(condition_info$exp_id,"cond",1:nrow(condition_info),sep = "_")
  condition_info = condition_info[!is.na(condition_info$transgene_name),]
  cond_names = t(as.data.frame(strsplit(condition_info$transgene_name,"[_]")))
  
  condition_info = cbind(condition_info,cond_names)
  names(condition_info)[4:ncol(condition_info)] = c("olfactory_receptor","promotor","driver","transgene","reporter","T2A","sex","age")
  
  dbAppendTable(con,"conditions_info",condition_info)
  return(condition_info)
}

### Load Formatted_data into DB ###
Export_formatted_data = function(con,for_resume_final){
  exp_info= Read_Exp_info()
  formatted = for_resume_final
  
  formatted$exp_id = exp_info$exp_id
  
  ### cond_id
  formatted$condition_id = formatted$conditions 
  levels(formatted$condition_id) = condition_info$condition_id[unlist(lapply(levels(formatted$condition_id), function(x)which(condition_info$transgene_name==x)))]
  
  ### trial_id
  formatted$trial_id = as.factor(unlist(lapply(strsplit(as.character(formatted$odorant),"[_]"), `[`, 1)))
  levels(formatted$trial_id) = trial_info$trial_id[unlist(lapply(levels(formatted$trial_id), function(x)which(trial_info$odor==x)))]
  
  ### antenna_id
  formatted$antenna_id = paste0(formatted$exp_id,"_antenna_",formatted$variable)
  
  ### pulse_id
  formatted$pulse_id = paste0(formatted$antenna_id,
                              "_pulse_",
                              unlist(lapply(strsplit(as.character(formatted$pulse),"[_]"), `[`, 2)))
  
  dbAppendTable(con,"formatted_data",formatted[,c("exp_id","condition_id","antenna_id","pulse_id","trial_id","max_zscore"),])
}

### Load Raw_data into DB ###
Export_raw_data =function(con){
  exp_info = Read_Exp_info()
  query = dbSendQuery(con,paste0("SELECT * FROM raw_data WHERE exp_id = '",exp_info$exp_id,"'"))
  res = dbFetch(query)
  if(nrow(res)>0){
    ### if id already present ###
    print("raw_data already present")
  }else{
    raw = Split_CSV(getwd())
    raw = cbind(exp_id = exp_info$exp_id,trial_id = NA, odorant = raw$odorant,raw[,-ncol(raw)])
    
    ### trial_id
    raw$trial_id = as.factor(unlist(lapply(strsplit(as.character(raw$odorant),"[_]"), `[`, 1)))
    levels(raw$trial_id) = trial_info$trial_id[unlist(lapply(levels(raw$trial_id), function(x)which(trial_info$odor==x)))]
    
    dbAppendTable(con,"raw_data",raw)
  }
  dbClearResult(query)
}

