oyster_accuracy<-function(oyster_file,temp_file,limit,step,show_plot){
    oyster_data<-read.csv(oyster_file,header=T)
    temp_data<-read.csv(temp_file,header=T)
    temp_data$Measurement.Time<-as.Date(temp_data$Measurement.Time)
    oyster_data$Measurement.Time<-as.Date(oyster_data$Measurement.Time)
    
    if(grepl('Sensor',temp_file)){
        temp_data<-temp_data[temp_data$Type=='Temperature',]
    }
    
    start_time<-max(c(min(temp_data$Measurement.Time,na.rm=T),min(oyster_data$Measurement.Time,na.rm=T)))
    end_time<-min(c(max(temp_data$Measurement.Time,na.rm=T),max(oyster_data$Measurement.Time,na.rm=T)))
    
    full_time<-oyster_data$Measurement.Time[oyster_data$Measurement.Time<=end_time & oyster_data$Measurement.Time>=start_time]
    full_time<-unique(full_time)
    
    full_time<-full_time[order(full_time)]
    
    #full_time<-oyster_data$Measurement.Time[oyster_data$Measurement.Time %in% full_time]
    
    time_pc<-integer(0)
    class(time_pc)<-'Date'
    time_pw<-integer(0)
    class(time_pw)<-'Date'
    
    temp_pc<-c()
    temp_pw<-c()
    
    for(i in 1:length(full_time)){
        time_here<-full_time[i]
        if(sum(oyster_data$Measurement.Time==time_here & oyster_data$Loc=='PC')!=0){
            temp_here<-temp_data$Value[temp_data$Measurement.Time==time_here & temp_data$Loc=='PC']
            temp_here<-mean(temp_here,na.rm=T)
            temp_pc<-c(temp_pc,temp_here)
            time_pc<-c(time_pc,time_here)
        }
        
        if(sum(oyster_data$Measurement.Time==time_here & oyster_data$Loc=='PW')!=0){
            temp_here<-temp_data$Value[temp_data$Measurement.Time==time_here & temp_data$Loc=='PW']
            temp_here<-mean(temp_here,na.rm=T)
            temp_pw<-c(temp_pw,temp_here)
            time_pw<-c(time_pw,time_here)
        }
    }
    
    
    POM<-oyster_data$Value[oyster_data$Measurement.Time %in% full_time]
    
    model_data<-data.frame(Temp=c(temp_pc,temp_pw),POM=POM,Time=c(time_pc,time_pw))
    
    model_data<-model_data[complete.cases(model_data),]
    
    accu_full<-c()
    
    for(A in seq(0,limit,step)){
        model_here<-model_data
        pom_here<-(model_here$POM)
        judge_here<-array(NA,dim=length(pom_here))
        judge_here[model_here$Temp>=A]<-2
        judge_here[!(model_here$Temp>=A) | is.na(model_here$Temp)]<-0
        judge_here<-(judge_here)
        accu<-sum(judge_here==pom_here)/length(judge_here)
        accu_full<-c(accu_full,accu)
    }
    oyster_accu<-list();
    oyster_accu$accuracy<-accu_full
    
    oyster_accu$best_threshold<-seq(0,limit,step)[accu_full==max(accu_full)][length(seq(0,limit,step)[accu_full==max(accu_full)])]
    
    oyster_accu$test_threshold<-seq(0,limit,step)
    
    
    
    if (isTRUE(show_plot)){
        data_plot<-data.frame(Threshold=oyster_accu$test_threshold,accuracy=oyster_accu$accuracy)
        g<-ggplot()+geom_line(aes(x=Threshold,y=accuracy),data=data_plot)+geom_text(aes(x=oyster_accu$best_threshold,y=max(oyster_accu$accuracy),label='Best'))
        
    }
    
    oyster_accu$plot<-g
    return(oyster_accu)
}