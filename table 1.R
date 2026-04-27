# Table 1. Relationship between government response index and estimates of risk ratios against infection or severe disease
# -------------------------------------------------------------------------
## replace prior inf by 4 index for model A, A1
## (stringencyindex, containmenthealthindex, governmentresponseindex, economicsupportindex) 
library(metafor)
model_fun <- function(model_text,data){
  rma(yi=logrr,sei=logse, mods=as.formula(model_text), method="ML",data=data)
}

fun <- function(data){
  paste0(format(round(exp(data$b[i]),2),nsmall=2)," (", format(round(exp(data$ci.lb[i]),2),nsmall=2), ", ", format(round(exp(data$ci.ub[i]),2),nsmall=2), ")")
  
}

endpoint <- c("infection", "severe"   )

ve <- readRDS("/Users/timtsang/SPH Dropbox/Tim Tsang/Shared COVID VE review/GRI/upload/fig/table_1_data.rds")



## ave_stringencyindex
## model A(adjust prior inf): rr ~ stringencyindex + vaccine type + variants + mid-point of timing of dose intervals (group)
meta_str_a_adj <- data.frame(matrix(NA, 12 ,2))
names(meta_str_a_adj)[1:2] <- c("infection","severity")
rownames(meta_str_a_adj)[1:12] <- c("ave_stringencyindex","vaccineAdenovirus vector vaccines","vaccineInactivated virus vaccines", 
                                    "variants_t2late-delta","variants_t2Omicron (BA.1/BA.2)","variants_t2Omicron (BA.4/BA.5) and afterwards","time_intervals131-60","time_intervals161-90",
                                    "time_intervals191-120","time_intervals1121-180","time_intervals1181+","AIC")

for (k in 1:2){
  data <- ve[ve$outcome3==endpoint[k],]
  reg <- model_fun(model_text="rr~ave_stringencyindex+vaccine+variants_t2+time_intervals1+by_clinical+hist",data=data)
  for (i in which(rownames(reg$b)%in%rownames(meta_str_a_adj))){
    meta_str_a_adj[which(rownames(meta_str_a_adj)==rownames(reg$b)[i]),k] <- fun(reg)
  }
  meta_str_a_adj[12,k] <- format(round(AIC.rma(reg),1),nsmall=1)
}
meta_str_a_adj
#write.csv(meta_str_a_adj,"C:/Users/hiutung/Desktop/meta_reg_a_stringencyindex_adjust.csv")

## containmenthealthindex
## model A(adjust prior inf): rr ~ containmenthealthindex + vaccine type + variants + mid-point of timing of dose intervals (group)
meta_con_a_adj <- data.frame(matrix(NA, 12 ,2))
names(meta_con_a_adj)[1:2] <- c("infection","severity")
rownames(meta_con_a_adj)[1:12] <- c("ave_containmenthealthindex","vaccineAdenovirus vector vaccines","vaccineInactivated virus vaccines", 
                                    "variants_t2late-delta","variants_t2Omicron (BA.1/BA.2)","variants_t2Omicron (BA.4/BA.5) and afterwards","time_intervals131-60","time_intervals161-90",
                                    "time_intervals191-120","time_intervals1121-180","time_intervals1181+","AIC")

for (k in 1:2){
  data <- ve[ve$outcome3==endpoint[k],]
  reg <- model_fun(model_text="rr~ave_containmenthealthindex+vaccine+variants_t2+time_intervals1+by_clinical+hist",data=data)
  for (i in which(rownames(reg$b)%in%rownames(meta_con_a_adj))){
    meta_con_a_adj[which(rownames(meta_con_a_adj)==rownames(reg$b)[i]),k] <- fun(reg)
  }
  meta_con_a_adj[12,k] <- format(round(AIC.rma(reg),1),nsmall=1)
}
meta_con_a_adj
#write.csv(meta_con_a_adj,"C:/Users/hiutung/Desktop/meta_reg_a_containmenthealthindex_adjust.csv")

## ave_governmentresponseindex
## model A(adjust prior inf): rr ~ governmentresponseindex + vaccine type + variants + mid-point of timing of dose intervals (group)
meta_gov_a_adj <- data.frame(matrix(NA, 12 ,2))
names(meta_gov_a_adj)[1:2] <- c("infection","severity")
rownames(meta_gov_a_adj)[1:12] <- c("ave_governmentresponseindex","vaccineAdenovirus vector vaccines","vaccineInactivated virus vaccines", 
                                    "variants_t2late-delta","variants_t2Omicron (BA.1/BA.2)","variants_t2Omicron (BA.4/BA.5) and afterwards","time_intervals131-60","time_intervals161-90",
                                    "time_intervals191-120","time_intervals1121-180","time_intervals1181+","AIC")

for (k in 1:2){
  data <- ve[ve$outcome3==endpoint[k],]
  reg <- model_fun(model_text="rr~ave_governmentresponseindex+vaccine+variants_t2+time_intervals1+by_clinical+hist",data=data)
  for (i in which(rownames(reg$b)%in%rownames(meta_gov_a_adj))){
    meta_gov_a_adj[which(rownames(meta_gov_a_adj)==rownames(reg$b)[i]),k] <- fun(reg)
  }
  meta_gov_a_adj[12,k] <- format(round(AIC.rma(reg),1),nsmall=1)
}
meta_gov_a_adj
#write.csv(meta_gov_a_adj,"C:/Users/hiutung/Desktop/meta_reg_a_governmentresponseindex_adjust.csv")

