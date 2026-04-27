## figure 3. meta plot: combine overall, first interval only and last interval only
library(metafor)
ve_first <- readRDS("/Users/timtsang/SPH Dropbox/Tim Tsang/Shared COVID VE review/GRI/upload/fig/fig_3_date1.rds")
ve_last <- readRDS("/Users/timtsang/SPH Dropbox/Tim Tsang/Shared COVID VE review/GRI/upload/fig/fig_3_date2.rds")
ve <- readRDS("/Users/timtsang/SPH Dropbox/Tim Tsang/Shared COVID VE review/GRI/upload/fig/fig_3_date0.rds")
adjfac <- 0.4
## ----------------------------------------------------------------------
###meta analysis figure remove enrolment criteria
## using rr in regression but present by ve format in figure
metafun <- function(ve_data){
  figure_data <- data.frame(matrix(NA, 20, 9))
  names(figure_data) <- c("estimate","ci.ub", "ci.lb", "ci","n","I2","range.lb","range.ub","range")
  rownames(figure_data) <- c("Excluded", "Included", "mRNA vaccines", "Adenovirus vector vaccines", "Inactivated virus vaccines",
                             "pre-Delta and Delta", "late-Delta","Omicron (BA.1/BA.2)","Omicron (BA.4/BA.5) and afterwards",
                             "Clinical","Non-clinical","str.index<=45","str.index45-55","str.index>=55",
                             "con.index<=45","con.index45-55","con.index>=55","gov.index<=45","gov.index45-55","gov.index>=55")
  
  ## for excluding covid history
  if (nrow(ve_data[ve_data$hist==1,])!=0){
    figure_data[1,1] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$hist==1,])$b))*100
    figure_data[1,2] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$hist==1,])$ci.lb))*100
    figure_data[1,3] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$hist==1,])$ci.ub))*100
    figure_data[1,6] <-rma(yi=logrr,sei=logse, data=ve_data[ve_data$hist==1,])$I2
    figure_data[1,7] <-format(round(min(ve_data[ve_data$hist==1,]$VE),0),nsmall=0)
    figure_data[1,8] <-format(round(max(ve_data[ve_data$hist==1,]$VE),0),nsmall=0)
  }else{figure_data[1,] <- NA}
  
  ## for including covid history
  if (nrow(ve_data[ve_data$hist==0,])!=0){
    figure_data[2,1] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$hist==0,])$b))*100
    figure_data[2,2] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$hist==0,])$ci.lb))*100
    figure_data[2,3] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$hist==0,])$ci.ub))*100
    figure_data[2,6] <-rma(yi=logrr,sei=logse, data=ve_data[ve_data$hist==0,])$I2
    figure_data[2,7] <-format(round(min(ve_data[ve_data$hist==0,]$VE),0),nsmall=0)
    figure_data[2,8] <-format(round(max(ve_data[ve_data$hist==0,]$VE),0),nsmall=0)
  }else{figure_data[2,] <- NA}
  
  ## for vaccine mRNA
  if (nrow(ve_data[ve_data$vaccine=="mRNA",])!=0){
    figure_data[3,1] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$vaccine=="mRNA",])$b))*100
    figure_data[3,2] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$vaccine=="mRNA",])$ci.lb))*100
    figure_data[3,3] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$vaccine=="mRNA",])$ci.ub))*100
    figure_data[3,6] <-rma(yi=logrr,sei=logse,  data=ve_data[ve_data$vaccine=="mRNA",])$I2
    figure_data[3,7] <-format(round(min(ve_data[ve_data$vaccine=="mRNA",]$VE),0),nsmall=0)
    figure_data[3,8] <-format(round(max(ve_data[ve_data$vaccine=="mRNA",]$VE),0),nsmall=0)
  }else{figure_data[3,] <- NA}
  
  ## for vaccine Adenovirus vector vaccines
  if (nrow(ve_data[ve_data$vaccine=="Adenovirus vector vaccines",])!=0){
    figure_data[4,1] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$vaccine=="Adenovirus vector vaccines",])$b))*100
    figure_data[4,2] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$vaccine=="Adenovirus vector vaccines",])$ci.lb))*100
    figure_data[4,3] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$vaccine=="Adenovirus vector vaccines",])$ci.ub))*100
    figure_data[4,6] <-rma(yi=logrr,sei=logse, data=ve_data[ve_data$vaccine=="Adenovirus vector vaccines",])$I2
    figure_data[4,7] <-format(round(min(ve_data[ve_data$vaccine=="Adenovirus vector vaccines",]$VE),0),nsmall=0)
    figure_data[4,8] <-format(round(max(ve_data[ve_data$vaccine=="Adenovirus vector vaccines",]$VE),0),nsmall=0)
  }else{figure_data[4,] <- NA}
  
  ## for vaccine Inactivated virus vaccines
  if(nrow(ve_data[ve_data$vaccine=="Inactivated virus vaccines",])!=0){
    figure_data[5,1] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$vaccine=="Inactivated virus vaccines",])$b))*100
    figure_data[5,2] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$vaccine=="Inactivated virus vaccines",])$ci.lb))*100
    figure_data[5,3] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$vaccine=="Inactivated virus vaccines",])$ci.ub))*100
    figure_data[5,6] <-rma(yi=logrr,sei=logse, data=ve_data[ve_data$vaccine=="Inactivated virus vaccines",])$I2
    figure_data[5,7] <-format(round(min(ve_data[ve_data$vaccine=="Inactivated virus vaccines",]$VE),0),nsmall=0)
    figure_data[5,8] <-format(round(max(ve_data[ve_data$vaccine=="Inactivated virus vaccines",]$VE),0),nsmall=0)
  }else{figure_data[5,] <- NA}
  
  ## for variants "Variants other than Delta and Omicron" & "Delta with other variants"
  if(nrow(ve_data[ve_data$variant_col%in%c("Variants other than Delta and Omicron","Delta with other variants"),])!=0){
    figure_data[6,1] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$variant_col%in%c("Variants other than Delta and Omicron","Delta with other variants"),])$b))*100
    figure_data[6,2] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$variant_col%in%c("Variants other than Delta and Omicron","Delta with other variants"),])$ci.lb))*100
    figure_data[6,3] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$variant_col%in%c("Variants other than Delta and Omicron","Delta with other variants"),])$ci.ub))*100
    figure_data[6,6] <-rma(yi=logrr,sei=logse, data=ve_data[ve_data$variant_col%in%c("Variants other than Delta and Omicron","Delta with other variants"),])$I2
    figure_data[6,7] <-format(round(min(ve_data[ve_data$variant_col%in%c("Variants other than Delta and Omicron","Delta with other variants"),]$VE),0),nsmall=0)
    figure_data[6,8] <-format(round(max(ve_data[ve_data$variant_col%in%c("Variants other than Delta and Omicron","Delta with other variants"),]$VE),0),nsmall=0)
  }else{figure_data[6,] <- NA}
  
  ## for variants "Delta, Delta or Omicron" 
  if(nrow(ve_data[ve_data$variant_col=="Delta, Delta or Omicron",])!=0){
    figure_data[7,1] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$variant_col=="Delta, Delta or Omicron",])$b))*100
    figure_data[7,2] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$variant_col=="Delta, Delta or Omicron",])$ci.lb))*100
    figure_data[7,3] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$variant_col=="Delta, Delta or Omicron",])$ci.ub))*100
    figure_data[7,6] <-rma(yi=logrr,sei=logse, data=ve_data[ve_data$variant_col=="Delta, Delta or Omicron" ,])$I2
    figure_data[7,7] <-format(round(min(ve_data[ve_data$variant_col=="Delta, Delta or Omicron",]$VE),0),nsmall=0)
    figure_data[7,8] <-format(round(max(ve_data[ve_data$variant_col=="Delta, Delta or Omicron",]$VE),0),nsmall=0)
  }else{figure_data[7,] <- NA}
  
  ## for variants "Omicron (BA.1/BA.2)"
  if(nrow(ve_data[ve_data$variant_col=="Omicron (BA.1/BA.2)",])!=0){
    figure_data[8,1] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$variant_col=="Omicron (BA.1/BA.2)",])$b))*100
    figure_data[8,2] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$variant_col=="Omicron (BA.1/BA.2)",])$ci.lb))*100
    figure_data[8,3] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$variant_col=="Omicron (BA.1/BA.2)",])$ci.ub))*100
    figure_data[8,6] <-rma(yi=logrr,sei=logse, data=ve_data[ve_data$variant_col=="Omicron (BA.1/BA.2)",])$I2
    figure_data[8,7] <-format(round(min(ve_data[ve_data$variant_col=="Omicron (BA.1/BA.2)",]$VE),0),nsmall=0)
    figure_data[8,8] <-format(round(max(ve_data[ve_data$variant_col=="Omicron (BA.1/BA.2)",]$VE),0),nsmall=0)
  }else{figure_data[8,] <- NA}
  
  ## for variants "Omicron (BA.4/BA.5) and afterwards"
  if(nrow(ve_data[ve_data$variant_col=="Omicron (BA.4/BA.5) and afterwards",])!=0){
    figure_data[9,1] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$variant_col=="Omicron (BA.4/BA.5) and afterwards",])$b))*100
    figure_data[9,2] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$variant_col=="Omicron (BA.4/BA.5) and afterwards",])$ci.lb))*100
    figure_data[9,3] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$variant_col=="Omicron (BA.4/BA.5) and afterwards",])$ci.ub))*100
    figure_data[9,6] <-rma(yi=logrr,sei=logse, data=ve_data[ve_data$variant_col=="Omicron (BA.4/BA.5) and afterwards",])$I2
    figure_data[9,7] <-format(round(min(ve_data[ve_data$variant_col=="Omicron (BA.4/BA.5) and afterwards",]$VE),0),nsmall=0)
    figure_data[9,8] <-format(round(max(ve_data[ve_data$variant_col=="Omicron (BA.4/BA.5) and afterwards",]$VE),0),nsmall=0)
  }else{figure_data[9,] <- NA}
  
  ## for Recruitment criteria clinical
  if(nrow(ve_data[ve_data$by_clinical==1&!is.na(ve_data$by_clinical==1),])!=0){
    figure_data[10,1] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$by_clinical==1,])$b))*100
    figure_data[10,2] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$by_clinical==1,])$ci.lb))*100
    figure_data[10,3] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$by_clinical==1,])$ci.ub))*100
    figure_data[10,6] <-rma(yi=logrr,sei=logse, data=ve_data[ve_data$by_clinical==1,])$I2
    figure_data[10,7] <-format(round(min(ve_data[ve_data$by_clinical==1,]$VE),0),nsmall=0)
    figure_data[10,8] <-format(round(max(ve_data[ve_data$by_clinical==1,]$VE),0),nsmall=0)
  }else{figure_data[10,] <- NA}
  
  ## for Recruitment criteria non-clinical
  if (nrow(ve_data[ve_data$by_clinical==0&!is.na(ve_data$by_clinical==0),])!=0){
    figure_data[11,1] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$by_clinical==0,])$b))*100
    figure_data[11,2] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$by_clinical==0,])$ci.lb))*100
    figure_data[11,3] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$by_clinical==0,])$ci.ub))*100
    figure_data[11,6] <-rma(yi=logrr,sei=logse, data=ve_data[ve_data$by_clinical==0,])$I2
    figure_data[11,7] <-format(round(min(ve_data[ve_data$by_clinical==0,]$VE),0),nsmall=0)
    figure_data[11,8] <-format(round(max(ve_data[ve_data$by_clinical==0,]$VE),0),nsmall=0)
  }else{figure_data[11,] <- NA}
  
  ## for stringencyindex <=45
  if(nrow(ve_data[ve_data$group_ave_stri_index=="<=45",])!=0){
    figure_data[12,1] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_stri_index=="<=45",])$b))*100
    figure_data[12,2] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_stri_index=="<=45",])$ci.lb))*100
    figure_data[12,3] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_stri_index=="<=45",])$ci.ub))*100
    figure_data[12,6] <-rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_stri_index=="<=45",])$I2
    figure_data[12,7] <-format(round(min(ve_data[ve_data$group_ave_stri_index=="<=45",]$VE),0),nsmall=0)
    figure_data[12,8] <-format(round(max(ve_data[ve_data$group_ave_stri_index=="<=45",]$VE),0),nsmall=0)
  }else{figure_data[12,] <- NA}
  
  ## for stringencyindex 45-55
  if(nrow(ve_data[ve_data$group_ave_stri_index=="45-55",])!=0){
    figure_data[13,1] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_stri_index=="45-55",])$b))*100
    figure_data[13,2] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_stri_index=="45-55",])$ci.lb))*100
    figure_data[13,3] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_stri_index=="45-55",])$ci.ub))*100
    figure_data[13,6] <-rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_stri_index=="45-55",])$I2
    figure_data[13,7] <-format(round(min(ve_data[ve_data$group_ave_stri_index=="45-55",]$VE),0),nsmall=0)
    figure_data[13,8] <-format(round(max(ve_data[ve_data$group_ave_stri_index=="45-55",]$VE),0),nsmall=0)
  }else{figure_data[13,] <- NA}
  
  
  ## for stringencyindex >=55
  if(nrow(ve_data[ve_data$group_ave_stri_index==">=55",])!=0){
    figure_data[14,1] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_stri_index==">=55",])$b))*100
    figure_data[14,2] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_stri_index==">=55",])$ci.lb))*100
    figure_data[14,3] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_stri_index==">=55",])$ci.ub))*100
    figure_data[14,6] <-rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_stri_index==">=55",])$I2
    figure_data[14,7] <-format(round(min(ve_data[ve_data$group_ave_stri_index==">=55",]$VE),0),nsmall=0)
    figure_data[14,8] <-format(round(max(ve_data[ve_data$group_ave_stri_index==">=55",]$VE),0),nsmall=0)
  }else{figure_data[14,] <- NA}
  
  
  ## for containmenthealthindex <=45
  if (nrow(ve_data[ve_data$group_ave_con_index=="<=45",])!=0){
    figure_data[15,1] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_con_index=="<=45",])$b))*100
    figure_data[15,2] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_con_index=="<=45",])$ci.lb))*100
    figure_data[15,3] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_con_index=="<=45",])$ci.ub))*100
    figure_data[15,6] <-rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_con_index=="<=45",])$I2
    figure_data[15,7] <-format(round(min(ve_data[ve_data$group_ave_con_index=="<=45",]$VE),0),nsmall=0)
    figure_data[15,8] <-format(round(max(ve_data[ve_data$group_ave_con_index=="<=45",]$VE),0),nsmall=0)
  }else{figure_data[15,] <- NA}
  
  ## for containmenthealthindex 45-55
  if (nrow(ve_data[ve_data$group_ave_con_index=="45-55",])!=0){
    figure_data[16,1] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_con_index=="45-55",])$b))*100
    figure_data[16,2] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_con_index=="45-55",])$ci.lb))*100
    figure_data[16,3] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_con_index=="45-55",])$ci.ub))*100
    figure_data[16,6] <-rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_con_index=="45-55",])$I2
    figure_data[16,7] <-format(round(min(ve_data[ve_data$group_ave_con_index=="45-55",]$VE),0),nsmall=0)
    figure_data[16,8] <-format(round(max(ve_data[ve_data$group_ave_con_index=="45-55",]$VE),0),nsmall=0)
  }else{figure_data[16,] <- NA}
  
  
  ## for containmenthealthindex >=55
  if (nrow(ve_data[ve_data$group_ave_con_index==">=55",])!=0){
    figure_data[17,1] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_con_index==">=55",])$b))*100
    figure_data[17,2] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_con_index==">=55",])$ci.lb))*100
    figure_data[17,3] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_con_index==">=55",])$ci.ub))*100
    figure_data[17,6] <-rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_con_index==">=55",])$I2
    figure_data[17,7] <-format(round(min(ve_data[ve_data$group_ave_con_index==">=55",]$VE),0),nsmall=0)
    figure_data[17,8] <-format(round(max(ve_data[ve_data$group_ave_con_index==">=55",]$VE),0),nsmall=0)
  }else{figure_data[17,] <- NA}
  
  
  ## for governmentresponseindex <=45
  if (nrow(ve_data[ve_data$group_ave_gov_index=="<=45",])!=0){
    figure_data[18,1] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_gov_index=="<=45",])$b))*100
    figure_data[18,2] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_gov_index=="<=45",])$ci.lb))*100
    figure_data[18,3] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_gov_index=="<=45",])$ci.ub))*100
    figure_data[18,6] <-rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_gov_index=="<=45",])$I2
    figure_data[18,7] <-format(round(min(ve_data[ve_data$group_ave_gov_index=="<=45",]$VE),0),nsmall=0)
    figure_data[18,8] <-format(round(max(ve_data[ve_data$group_ave_gov_index=="<=45",]$VE),0),nsmall=0)
  }else{figure_data[18,] <- NA}
  
  ## for governmentresponseindex 45-55
  if (nrow(ve_data[ve_data$group_ave_gov_index=="45-55",])!=0){
    figure_data[19,1] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_gov_index=="45-55",])$b))*100
    figure_data[19,2] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_gov_index=="45-55",])$ci.lb))*100
    figure_data[19,3] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_gov_index=="45-55",])$ci.ub))*100
    figure_data[19,6] <-rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_gov_index=="45-55",])$I2
    figure_data[19,7] <-format(round(min(ve_data[ve_data$group_ave_gov_index=="45-55",]$VE),0),nsmall=0)
    figure_data[19,8] <-format(round(max(ve_data[ve_data$group_ave_gov_index=="45-55",]$VE),0),nsmall=0)
  }else{figure_data[19,] <- NA}
  
  
  ## for governmentresponseindex >=55
  if (nrow(ve_data[ve_data$group_ave_gov_index==">=55",])!=0){
    figure_data[20,1] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_gov_index==">=55",])$b))*100
    figure_data[20,2] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_gov_index==">=55",])$ci.lb))*100
    figure_data[20,3] <-(1-exp(rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_gov_index==">=55",])$ci.ub))*100
    figure_data[20,6] <-rma(yi=logrr,sei=logse, data=ve_data[ve_data$group_ave_gov_index==">=55",])$I2
    figure_data[20,7] <-format(round(min(ve_data[ve_data$group_ave_gov_index==">=55",]$VE),0),nsmall=0)
    figure_data[20,8] <-format(round(max(ve_data[ve_data$group_ave_gov_index==">=55",]$VE),0),nsmall=0)
  }else{figure_data[20,] <- NA}
  
  
  for (i in 1:20) {
    figure_data[i,4] <- paste0(format(round(figure_data[i,1],0), nsmall=0),  " (", format(round(figure_data[i,3],0), nsmall=0),
                               ", ", format(round(figure_data[i,2],0), nsmall=0), ")")
    figure_data[i,9] <- paste0("(", figure_data[i,7], ", ", figure_data[i,8], ")")
  }
  
  figure_data[,5] <- c(nrow(ve_data[ve_data$hist==1,]),nrow(ve_data[ve_data$hist==0,]),
                       nrow(ve_data[ve_data$vaccine=="mRNA",]),nrow(ve_data[ve_data$vaccine=="Adenovirus vector vaccines",]),
                       nrow(ve_data[ve_data$vaccine=="Inactivated virus vaccines",]),
                       nrow(ve_data[ve_data$variant_col%in%c("Variants other than Delta and Omicron","Delta with other variants"),]),
                       nrow(ve_data[ve_data$variant_col=="Delta, Delta or Omicron",]),
                       nrow(ve_data[ve_data$variant_col=="Omicron (BA.1/BA.2)",]),
                       nrow(ve_data[ve_data$variant_col=="Omicron (BA.4/BA.5) and afterwards",]),
                       nrow(ve_data[ve_data$by_clinical==1,]),nrow(ve_data[ve_data$by_clinical==0,]),
                       nrow(ve_data[ve_data$group_ave_stri_index=="<=45",]),nrow(ve_data[ve_data$group_ave_stri_index=="45-55",]), nrow(ve_data[ve_data$group_ave_stri_index==">=55",]),
                       nrow(ve_data[ve_data$group_ave_con_index=="<=45",]),nrow(ve_data[ve_data$group_ave_con_index=="45-55",]),nrow(ve_data[ve_data$group_ave_con_index==">=55",]),
                       nrow(ve_data[ve_data$group_ave_gov_index=="<=45",]),nrow(ve_data[ve_data$group_ave_gov_index=="45-55",]),nrow(ve_data[ve_data$group_ave_gov_index==">=55",]))
  
  figure_data[,6] <- format(round(figure_data[,6],0), nsmall=0)
  
  figure_data
}

variant <- c("Variants other than Delta and Omicron", "Delta with other variants","Delta, Delta or Omicron","Delta, Delta or Omicron",
             "Omicron (BA.1/BA.2)","Omicron (BA.1/BA.2)","Omicron (BA.4/BA.5) and afterwards","Omicron (BA.4/BA.5) and afterwards")
label <- c("pre-delta&delta","late-delta","omicron1&2","omicron4&5aftereards")

## ------------------new figure: combine overall, first interval only and last interval only----------------
## only for 3 index

## overall
## inf
ve_data <- ve[ve$outcome3=="infection",]
figure_inf <- metafun(ve_data)
## sev
ve_data <- ve[ve$outcome3=="severe",]
figure_sev <- metafun(ve_data)

##add enrolment criteria
nrow(figure_inf)
figure_inf  <- figure_inf[c(12:20),]
nrow(figure_sev)
figure_sev  <- figure_sev[c(12:20),]

## first interval only
## inf
ve_data <- ve_first[ve_first$outcome3=="infection",]
figure_inf_first <- metafun(ve_data)
## sev
ve_data <- ve_first[ve_first$outcome3=="severe",]
figure_sev_first <- metafun(ve_data)

##add enrolment criteria
nrow(figure_inf_first)
figure_inf_first <- figure_inf_first[c(12:20),]
nrow(figure_sev_first)
figure_sev_first  <- figure_sev_first[c(12:20),]

## last interval only
## inf
ve_data <- ve_last[ve_last$outcome3=="infection",]
figure_inf_last <- metafun(ve_data)
## sev
ve_data <- ve_last[ve_last$outcome3=="severe",]
figure_sev_last <- metafun(ve_data)

##add enrolment criteria
nrow(figure_inf_last)
figure_inf_last <- figure_inf_last[c(12:20),]
nrow(figure_sev_last)
figure_sev_last  <- figure_sev_last[c(12:20),]


pdf("/Users/timtsang/SPH Dropbox/Tim Tsang/Shared COVID VE review/GRI/upload/fig/meta_comb_2023sept_alltiming_indexonly_v2(color).pdf",width=11.5, height=15)

par(mar=c(1.5,0,0,0))

plot(0,0,xlab="",ylab="", main="", axes=F, xlim=c(-5,25), ylim=c(3,64),type="n") ## reduce ylim to remove white space, original ylim=c(2,65.5) 
axis(1,at=15:19,labels=c(0,25,50,75,100),cex.axis=.95,pos=2)
mtext("Vaccine effectiveness (%)", side=1,cex=0.95, line=0.25, at=17)
colindex <- c(rep(c("#228B22","#4682B4","#B22222"),3))

metacomb_fun <- function(aa,bb,figure_inf,figure_sev,label){
  y1 <- aa-1:9
  y2 <- bb-1:9
  
  
  points(as.numeric(figure_inf$estimate)/25+15,y1[1:9]-0.2,pch=16,col=colindex)
  points(as.numeric(figure_sev$estimate)/25+15,y2[1:9]-0.2,pch=16,col=colindex)
  
  ##polygon to separate the studies
  
  adjfac <- 0.4
  polygon(c(-5.5,-5.5,25.5,25.5),c(aa+ adjfac,aa-1+ adjfac,aa-1+adjfac,aa+ adjfac),col=rgb(0,0,0,0.05),border=F)  
  polygon(c(-5.5,-5.5,25.5,25.5),c(bb+ adjfac,bb-1+ adjfac,bb-1+adjfac,bb+ adjfac),col=rgb(0,0,0,0.05),border=F)  
  
  
  for (i in 1:9) {
    #text(1.5,y1[i]-0.2-0.03,gsub(".*index","",rownames(figure_inf)[i]),las=1,font = 1.5,cex = 0.95)
    text(5,y1[i]-0.2-0.03,c(rep(c("below 45","45-55","above 55"),3))[i],las=1,font = 1.5,cex = 0.95)
    text(9,y1[i]-0.2-0.03,figure_inf$n[i],las=1,font = 1.5,cex = 0.95)
    
    lines(c(as.numeric(figure_inf$ci.lb[i])/25+15,as.numeric(figure_inf$ci.ub[i])/25+15),rep(y1[i]-0.2,2),col=colindex[i])
    
    text(21,y1[i]-0.2-0.03,figure_inf$ci[i],las=1,font = 1.5,cex = 0.95)
    text(13,y1[i]-0.2-0.03,figure_inf$range[i],las=1,font = 1.5,cex = 0.95)
    text(24,y1[i]-0.2-0.03,figure_inf$I2[i],las=1,font = 1.5,cex = 0.95)
    
    #text(1.5,y2[i]-0.2-0.03,gsub(".*index","",rownames(figure_sev)[i]),las=1,font = 1.5,cex = 0.95)
    text(5,y2[i]-0.2-0.03,c(rep(c("below 45","45-55","above 55"),3))[i],las=1,font = 1.5,cex = 0.95)
    text(9,y2[i]-0.2-0.03,figure_sev$n[i],las=1,font = 1.5,cex = 0.95)
    
    lines(c(as.numeric(figure_sev$ci.lb[i])/25+15,as.numeric(figure_sev$ci.ub[i])/25+15),rep(y2[i]-0.2,2),col=colindex[i])
    
    text(21,y2[i]-0.2-0.03,figure_sev$ci[i],las=1,font = 1.5,cex = 0.95)
    text(13,y2[i]-0.2-0.03,figure_sev$range[i],las=1,font = 1.5,cex = 0.95)
    text(24,y2[i]-0.2-0.03,figure_sev$I2[i],las=1,font = 1.5,cex = 0.95)
  } 
  
  
  if (label==1){
    lines(c(-5.5,25.5),c(bb+1+adjfac, bb+1+adjfac), col="black")
    lines(c(-5.5,25.5),c(aa+1+adjfac, aa+1+adjfac), col="black")
  }else{
    lines(c(-5.5,25.5),c(bb+adjfac, bb+adjfac), col="black")
    lines(c(-5.5,25.5),c(aa+adjfac, aa+adjfac), col="black")
  }
  
  
  
  text(0,y1[1]-0.2-0.03,"Stringency index",las=1,font = 1.5,cex = 0.95)
  text(0,y1[4]-0.2-0.03,"Containment health index",las=1,font = 1.5,cex = 0.95)
  text(0,y1[7]-0.2-0.03,"Government response index",las=1,font = 1.5,cex = 0.95)
  
  text(0,y2[1]-0.2-0.03,"Stringency index",las=1,font = 1.5,cex = 0.95)
  text(0,y2[4]-0.2-0.03,"Containment health index",las=1,font = 1.5,cex = 0.95)
  text(0,y2[7]-0.2-0.03,"Government response index",las=1,font = 1.5,cex = 0.95)
  
  text(-0.2,aa-0.2-0.03,c("Overall","First time period reported in studies","Last time period reported in studies")[label],las=1,font = 1.5,cex = 1.15)
  text(-0.2,bb-0.2-0.03,c("Overall","First time period reported in studies","Last time period reported in studies")[label],las=1, font = 1.5,cex = 1.15)
  
}

lines(c(-5.5,25.5),c(2+adjfac, 2+adjfac), col="black")
text(0,65,"Factors",las=1,font = 1.5,cex = 1.15)
#text(1,13,"",las=1,font = 1.5,cex = 1.25)
text(9,65,"No. of estimates",las=1,font = 1.5,cex = 1.15)
text(21,65-0.05,"VE (95% CI)",las=1, font = 1.5,cex = 1.15)
text(13,65-0.05,"VE range",las=1, font = 1.5,cex = 1.15)
text(24,65,expression("I"^2),las=1, font= 1.5,cex = 1.15)

metacomb_fun(63,32,figure_inf,figure_sev,1)
metacomb_fun(53,22,figure_inf_first,figure_sev_first,2)
metacomb_fun(43,12,figure_inf_last,figure_sev_last,3)

text(10,64-0.2,"VE against infection",las=1,font = 1.5,cex = 1.15)
text(10,33-0.2,"VE against severe disease",las=1,font = 1.5,cex = 1.15)

dev.off()
