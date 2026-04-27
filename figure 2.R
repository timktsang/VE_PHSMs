library(ggplot2)
library(cowplot)
library(gridExtra)
# figure 2.correlation plot of SI, CHI, GRI and VE by infection/severe

pearson_alldata <- read.csv("/Users/timtsang/SPH Dropbox/Tim Tsang/Shared COVID VE review/GRI/upload/fig/fig2_data.csv")
spearman_alldata <- read.csv("/Users/timtsang/SPH Dropbox/Tim Tsang/Shared COVID VE review/GRI/upload/fig/fig2_data_2.csv")
ve <- readRDS("/Users/timtsang/SPH Dropbox/Tim Tsang/Shared COVID VE review/GRI/upload/fig/table_1_data.rds")


######  --------version: remove correlation in plot and generate supp to record correlation------------
h4 <- ggplot(ve[ve$outcome3=="infection",], aes(ave_stringencyindex*10, VE)) + 
  geom_point(data=ve[ve$outcome3=="infection"&ve$variants_t2=="pre-delta and delta",],size = 1.5, alpha = 0.5, shape = 16,col='cadetblue3')+
  geom_point(data=ve[ve$outcome3=="infection"&ve$variants_t2=="late-delta",],size = 1.5, alpha = 0.5, shape = 16,col="thistle3")+
  geom_point(data=ve[ve$outcome3=="infection"&ve$variants_t2%in%c("Omicron (BA.1/BA.2)"),],size = 1.5, alpha = 0.5, shape = 16,col="#c29f62")+
  geom_point(data=ve[ve$outcome3=="infection"&ve$variants_t2%in%c("Omicron (BA.4/BA.5) and afterwards"),],size = 1.5, alpha = 0.5, shape = 16,col='darkolivegreen4')+
  scale_x_continuous(name = "Average of index",limits = c(10,102), breaks = c(10,20,30,40,50,60,70,80,90,100)) +
  scale_y_continuous(name = "VE against infection",limits = c(10,105), breaks = c(10,20,30,40,50,60,70,80,90,100))+
  ##annotate(geom="text", x=120, y=55, label=bquote("r"[overall]~'='~.(pearson_alldata[2,4])),size=3.5,col="black")+
  ##annotate(geom="text", x=120, y=50, label= bquote(rho[overall]~'='~.(spearman_alldata[2,4])),size=3.5,col="black")+
  #annotate(geom="text", x=120, y=50, label= expression(paste(rho[overall]," = ","0.64 (0.60, 0.68)")),size=3.5,col="black")+
  ##annotate(geom="text", x=127, y=45, label=bquote("r"["pre-Delta and Delta"]~'='~.(pearson_alldata[3,4])),size=3.5,col='cadetblue3')+
  ##annotate(geom="text", x=127, y=40, label= bquote(rho["pre-Delta and Delta"]~'='~.(spearman_alldata[3,4])),size=3.5,col='cadetblue3')+
  #annotate(geom="text", x=127, y=40, label= expression(paste(rho["pre-Delta and Delta"]," = ","0.18 (0.06, 0.30)")),size=3.5,col='cadetblue3')+
  ##annotate(geom="text", x=122, y=35, label=bquote("r"["late-Delta"]~'='~.(pearson_alldata[4,4])),size=3.5,col="thistle3")+
  ##annotate(geom="text", x=122, y=30, label= bquote(rho["late-Delta"]~'='~.(spearman_alldata[4,4])),size=3.5,col="thistle3")+
  #annotate(geom="text", x=122, y=30, label= expression(paste(rho["late-delta"]," = ","0.32 (0.21, 0.42)")),size=3.5,col="thistle3")+
  ##annotate(geom="text", x=127, y=25, label=bquote("r"["Omicron (BA.1/BA.2)"]~'='~.(pearson_alldata[5,4])),size=3.5,col="#c29f62")+
  ##annotate(geom="text", x=127, y=20, label= bquote(rho["Omicron (BA.1/BA.2)"]~'='~.(spearman_alldata[5,4])),size=3.5,col="#c29f62")+
#annotate(geom="text", x=127, y=20, label= expression(paste(rho["Omicron (BA.1/BA.2)"]," = ","0.30 (0.19, 0.42)")),size=3.5,col="#c29f62")+
##annotate(geom="text", x=130, y=15, label=bquote("r"["Omicron (BA.4/BA.5)"]~'='~.(pearson_alldata[6,4])),size=3.5,col='darkolivegreen4')+
##annotate(geom="text", x=130, y=10, label= bquote(rho["Omicron (BA.4/BA.5)"]~'='~.(spearman_alldata[6,4])),size=3.5,col='darkolivegreen4')+
#annotate(geom="text", x=130, y=10, label= expression(paste(rho["Omicron (BA.4/BA.5)"]," = ","-0.25 (-0.42, -0.09)")),size=3.5,col='darkolivegreen4')+
geom_line(aes(ave_stringencyindex*10,predict(lm(VE~I(ave_stringencyindex*10)))),col="black",linetype=2,size=1)+
  geom_line(data=ve[ve$outcome3=="infection"&ve$variants_t2=="pre-delta and delta",],aes(ave_stringencyindex*10,predict(lm(VE~I(ave_stringencyindex*10)),newdata=ve[ve$outcome3=="infection"&ve$variants_t2=="pre-delta and delta",])),col='cadetblue3',linetype=2,size=1)+
  geom_line(data=ve[ve$outcome3=="infection"&ve$variants_t2=="late-delta",],aes(ave_stringencyindex*10,predict(lm(VE~I(ave_stringencyindex*10)),newdata=ve[ve$outcome3=="infection"&ve$variants_t2=="late-delta",])),col="thistle3",linetype=2,size=1)+
  geom_line(data=ve[ve$outcome3=="infection"&ve$variants_t2=="Omicron (BA.1/BA.2)",],aes(ave_stringencyindex*10,predict(lm(VE~I(ave_stringencyindex*10)),newdata=ve[ve$outcome3=="infection"&ve$variants_t2=="Omicron (BA.1/BA.2)",])),col="#c29f62",linetype=2,size=1)+
  geom_line(data=ve[ve$outcome3=="infection"&ve$variants_t2%in%c("Omicron (BA.4/BA.5) and afterwards"),],aes(ave_stringencyindex*10,predict(lm(VE~I(ave_stringencyindex*10)),newdata=ve[ve$outcome3=="infection"&ve$variants_t2%in%c("Omicron (BA.4/BA.5) and afterwards"),])),col='darkolivegreen4',linetype=2,size=1)+
  #geom_smooth(col="black",se=F,method = 'lm',linetype=2)+
  #geom_smooth(data=ve[ve$outcome3=="infection"&ve$variants_t2=="pre-delta and delta",],col='cadetblue3',se=F,method = 'lm',linetype=2)+
  #geom_smooth(data=ve[ve$outcome3=="infection"&ve$variants_t2=="late-delta",],col="thistle3",se=F,method = 'lm',linetype=2)+
  #geom_smooth(data=ve[ve$outcome3=="infection"&ve$variants_t2=="Omicron (BA.1/BA.2)",],col="#c29f62",se=F,method = 'lm',linetype=2)+
  #geom_smooth(data=ve[ve$outcome3=="infection"&ve$variants_t2%in%c("Omicron (BA.4/BA.5) and afterwards"),],col='darkolivegreen4',se=F,method = 'lm',linetype=2)+
  theme_cowplot()+
  labs(tag = "A",title = 'Stringency index')+
  theme(plot.title = element_text(hjust = 0.5,face = 'plain',size = 17),plot.tag = element_text(size = 22,face = 'bold'),
        axis.title.x = element_text(size=13,hjust=0.47),axis.title.y = element_text(size=13),axis.text = element_text(size=11.5),
        axis.line.x=element_blank())+
  geom_segment(aes(x=-Inf,xend=Inf,y=-Inf,yend=-Inf),col="black",size=0.5)

h5 <- ggplot(ve[ve$outcome3=="infection",], aes(ave_containmenthealthindex*10, VE)) +
  geom_point(data=ve[ve$outcome3=="infection"&ve$variants_t2=="pre-delta and delta",],size = 1.5, alpha = 0.5, shape = 16,col='cadetblue3')+
  geom_point(data=ve[ve$outcome3=="infection"&ve$variants_t2=="late-delta",],size = 1.5, alpha = 0.5, shape = 16,col="thistle3")+
  geom_point(data=ve[ve$outcome3=="infection"&ve$variants_t2%in%c("Omicron (BA.1/BA.2)"),],size = 1.5, alpha = 0.5, shape = 16,col="#c29f62")+
  geom_point(data=ve[ve$outcome3=="infection"&ve$variants_t2%in%c("Omicron (BA.4/BA.5) and afterwards"),],size = 1.5, alpha = 0.5, shape = 16,col='darkolivegreen4')+
  scale_x_continuous(name = "Average of index",limits = c(10,102), breaks = c(10,20,30,40,50,60,70,80,90,100)) +
  scale_y_continuous(name = "VE against infection",limits = c(10,105), breaks = c(10,20,30,40,50,60,70,80,90,100))+
  ##annotate(geom="text", x=120, y=55, label=bquote("r"[overall]~'='~.(pearson_alldata[2,8])),size=3.5,col="black")+
  ##annotate(geom="text", x=120, y=50, label= bquote(rho[overall]~'='~.(spearman_alldata[2,8])),size=3.5,col="black")+
  #annotate(geom="text", x=120, y=50, label= expression(paste(rho[overall]," = ","0.62 (0.58, 0.66)")),size=3.5,col="black")+
  ##annotate(geom="text", x=127, y=45, label=bquote("r"["pre-Delta and Delta"]~'='~.(pearson_alldata[3,8])),size=3.5,col='cadetblue3')+
  ##annotate(geom="text", x=127, y=40, label= bquote(rho["pre-Delta and Delta"]~'='~.(spearman_alldata[3,8])),size=3.5,col='cadetblue3')+
  #annotate(geom="text", x=127, y=40, label= expression(paste(rho["pre-Delta and Delta"]," = ","0.19 (0.05, 0.34)")),size=3.5,col='cadetblue3')+
  ##annotate(geom="text", x=122, y=35, label=bquote("r"["late-Delta"]~'='~.(pearson_alldata[4,8])),size=3.5,col="thistle3")+
  ##annotate(geom="text", x=122, y=30, label= bquote(rho["late-Delta"]~'='~.(spearman_alldata[4,8])),size=3.5,col="thistle3")+
  #annotate(geom="text", x=122, y=30, label= expression(paste(rho["late-delta"]," = ","0.28 (0.17, 0.39)")),size=3.5,col="thistle3")+
  ##annotate(geom="text", x=127, y=25, label=bquote("r"["Omicron (BA.1/BA.2)"]~'='~.(pearson_alldata[5,8])),size=3.5,col="#c29f62")+
  ##annotate(geom="text", x=127, y=20, label= bquote(rho["Omicron (BA.1/BA.2)"]~'='~.(spearman_alldata[5,8])),size=3.5,col="#c29f62")+
#annotate(geom="text", x=127, y=20, label= expression(paste(rho["Omicron (BA.1/BA.2)"]," = ","0.22 (0.10, 0.34)")),size=3.5,col="#c29f62")+
##annotate(geom="text", x=130, y=15, label=bquote("r"["Omicron (BA.4/BA.5)"]~'='~.(pearson_alldata[6,8])),size=3.5,col='darkolivegreen4')+
##annotate(geom="text", x=130, y=10, label= bquote(rho["Omicron (BA.4/BA.5)"]~'='~.(spearman_alldata[6,8])),size=3.5,col='darkolivegreen4')+
#annotate(geom="text", x=130, y=10, label= expression(paste(rho["Omicron (BA.4/BA.5)"]," = ","-0.33 (-0.49, -0.18)")),size=3.5,col='darkolivegreen4')+
geom_line(aes(ave_containmenthealthindex*10,predict(lm(VE~I(ave_containmenthealthindex*10)))),col="black",linetype=2,size=1)+
  geom_line(data=ve[ve$outcome3=="infection"&ve$variants_t2=="pre-delta and delta",],aes(ave_containmenthealthindex*10,predict(lm(VE~I(ave_containmenthealthindex*10)),newdata=ve[ve$outcome3=="infection"&ve$variants_t2=="pre-delta and delta",])),col='cadetblue3',linetype=2,size=1)+
  geom_line(data=ve[ve$outcome3=="infection"&ve$variants_t2=="late-delta",],aes(ave_containmenthealthindex*10,predict(lm(VE~I(ave_containmenthealthindex*10)),newdata=ve[ve$outcome3=="infection"&ve$variants_t2=="late-delta",])),col="thistle3",linetype=2,size=1)+
  geom_line(data=ve[ve$outcome3=="infection"&ve$variants_t2%in%c("Omicron (BA.1/BA.2)"),],aes(ave_containmenthealthindex*10,predict(lm(VE~I(ave_containmenthealthindex*10)),newdata=ve[ve$outcome3=="infection"&ve$variants_t2%in%c("Omicron (BA.1/BA.2)"),])),col="#c29f62",linetype=2,size=1)+
  geom_line(data=ve[ve$outcome3=="infection"&ve$variants_t2%in%c("Omicron (BA.4/BA.5) and afterwards"),],aes(ave_containmenthealthindex*10,predict(lm(VE~I(ave_containmenthealthindex*10)),newdata=ve[ve$outcome3=="infection"&ve$variants_t2%in%c("Omicron (BA.4/BA.5) and afterwards"),])),col='darkolivegreen4',linetype=2,size=1)+
  #geom_smooth(col="black",se=F,method = 'lm',linetype=2)+
  #geom_smooth(data=ve[ve$outcome3=="infection"&ve$variants_t2=="pre-delta and delta",],col='cadetblue3',se=F,method = 'lm',linetype=2)+
  #geom_smooth(data=ve[ve$outcome3=="infection"&ve$variants_t2=="late-delta",],col="thistle3",se=F,method = 'lm',linetype=2)+
  #geom_smooth(data=ve[ve$outcome3=="infection"&ve$variants_t2%in%c("Omicron (BA.1/BA.2)"),],col="#c29f62",se=F,method = 'lm',linetype=2)+
  #geom_smooth(data=ve[ve$outcome3=="infection"&ve$variants_t2%in%c("Omicron (BA.4/BA.5) and afterwards"),],col='darkolivegreen4',se=F,method = 'lm',linetype=2)+
  theme_cowplot()+
  labs(tag = "B",title = 'Containment health index')+
  theme(plot.title = element_text(hjust = 0.5,face = 'plain',size = 17),plot.tag = element_text(size = 22,face = 'bold'),
        axis.title.x = element_text(size=13,hjust=0.47),axis.title.y = element_text(size=13),axis.text = element_text(size=11.5),
        axis.line.x=element_blank())+
  geom_segment(aes(x=-Inf,xend=Inf,y=-Inf,yend=-Inf),col="black",size=0.5)

h6 <- ggplot(ve[ve$outcome3=="infection",], aes(ave_governmentresponseindex*10, VE)) +
  geom_point(data=ve[ve$outcome3=="infection"&ve$variants_t2=="pre-delta and delta",],size = 1.5, alpha = 0.5, shape = 16,col='cadetblue3')+
  geom_point(data=ve[ve$outcome3=="infection"&ve$variants_t2=="late-delta",],size = 1.5, alpha = 0.5, shape = 16,col="thistle3")+
  geom_point(data=ve[ve$outcome3=="infection"&ve$variants_t2%in%c("Omicron (BA.1/BA.2)"),],size = 1.5, alpha = 0.5, shape = 16,col="#c29f62")+
  geom_point(data=ve[ve$outcome3=="infection"&ve$variants_t2%in%c("Omicron (BA.4/BA.5) and afterwards"),],size = 1.5, alpha = 0.5, shape = 16,col='darkolivegreen4')+
  scale_x_continuous(name = "Average of index",limits = c(10,102), breaks = c(10,20,30,40,50,60,70,80,90,100)) +
  scale_y_continuous(name = "VE against infection",limits = c(10,105), breaks = c(10,20,30,40,50,60,70,80,90,100))+
  ##annotate(geom="text", x=120, y=55, label=bquote("r"[overall]~'='~.(pearson_alldata[2,12])),size=3.5,col="black")+
  ##annotate(geom="text", x=120, y=50, label= bquote(rho[overall]~'='~.(spearman_alldata[2,12])),size=3.5,col="black")+
  #annotate(geom="text", x=120, y=50, label= expression(paste(rho[overall]," = ","0.60 (0.55, 0.64)")),size=3.5,col="black")+
  ##annotate(geom="text", x=127, y=45, label=bquote("r"["pre-Delta and Delta"]~'='~.(pearson_alldata[3,12])),size=3.5,col='cadetblue3')+
  ##annotate(geom="text", x=127, y=40, label= bquote(rho["pre-Delta and Delta"]~'='~.(spearman_alldata[3,12])),size=3.5,col='cadetblue3')+
  #annotate(geom="text", x=127, y=40, label= expression(paste(rho["pre-Delta and Delta"]," = ","0.21 (0.07, 0.36)")),size=3.5,col='cadetblue3')+
  ##annotate(geom="text", x=122, y=35, label=bquote("r"["late-Delta"]~'='~.(pearson_alldata[4,12])),size=3.5,col="thistle3")+
  ##annotate(geom="text", x=122, y=30, label= bquote(rho["late-Delta"]~'='~.(spearman_alldata[4,12])),size=3.5,col="thistle3")+
  #annotate(geom="text", x=122, y=30, label= expression(paste(rho["late-delta"]," = ","0.24 (0.13, 0.36)")),size=3.5,col="thistle3")+
  ##annotate(geom="text", x=127, y=25, label=bquote("r"["Omicron (BA.1/BA.2)"]~'='~.(pearson_alldata[5,12])),size=3.5,col="#c29f62")+
  ##annotate(geom="text", x=127, y=20, label= bquote(rho["Omicron (BA.1/BA.2)"]~'='~.(spearman_alldata[5,12])),size=3.5,col="#c29f62")+
#annotate(geom="text", x=127, y=20, label= expression(paste(rho["Omicron (BA.1/BA.2)"]," = ","0.28 (0.17, 0.40)")),size=3.5,col="#c29f62")+
##annotate(geom="text", x=130, y=15, label=bquote("r"["Omicron (BA.4/BA.5)"]~'='~.(pearson_alldata[6,12])),size=3.5,col='darkolivegreen4')+
##annotate(geom="text", x=130, y=10, label= bquote(rho["Omicron (BA.4/BA.5)"]~'='~.(spearman_alldata[6,12])),size=3.5,col='darkolivegreen4')+
#annotate(geom="text", x=130, y=10, label= expression(paste(rho["Omicron (BA.4/BA.5)"]," = ","-0.24 (-0.41, -0.08)")),size=3.5,col='darkolivegreen4')+
geom_line(aes(ave_governmentresponseindex*10,predict(lm(VE~I(ave_governmentresponseindex*10)))),col="black",linetype=2,size=1)+
  geom_line(data=ve[ve$outcome3=="infection"&ve$variants_t2=="pre-delta and delta",],aes(ave_governmentresponseindex*10,predict(lm(VE~I(ave_governmentresponseindex*10)),newdata=ve[ve$outcome3=="infection"&ve$variants_t2=="pre-delta and delta",])),col='cadetblue3',linetype=2,size=1)+
  geom_line(data=ve[ve$outcome3=="infection"&ve$variants_t2=="late-delta",],aes(ave_governmentresponseindex*10,predict(lm(VE~I(ave_governmentresponseindex*10)),newdata=ve[ve$outcome3=="infection"&ve$variants_t2=="late-delta",])),col="thistle3",linetype=2,size=1)+
  geom_line(data=ve[ve$outcome3=="infection"&ve$variants_t2%in%c("Omicron (BA.1/BA.2)"),],aes(ave_governmentresponseindex*10,predict(lm(VE~I(ave_governmentresponseindex*10)),newdata=ve[ve$outcome3=="infection"&ve$variants_t2%in%c("Omicron (BA.1/BA.2)"),])),col="#c29f62",linetype=2,size=1)+
  geom_line(data=ve[ve$outcome3=="infection"&ve$variants_t2%in%c("Omicron (BA.4/BA.5) and afterwards"),],aes(ave_governmentresponseindex*10,predict(lm(VE~I(ave_governmentresponseindex*10)),newdata=ve[ve$outcome3=="infection"&ve$variants_t2%in%c("Omicron (BA.4/BA.5) and afterwards"),])),col='darkolivegreen4',linetype=2,size=1)+
  #geom_smooth(col="black",se=F,method = 'lm',linetype=2)+
  #geom_smooth(data=ve[ve$outcome3=="infection"&ve$variants_t2=="pre-delta and delta",],col='cadetblue3',se=F,method = 'lm',linetype=2)+
  #geom_smooth(data=ve[ve$outcome3=="infection"&ve$variants_t2=="late-delta",],col="thistle3",se=F,method = 'lm',linetype=2)+
  #geom_smooth(data=ve[ve$outcome3=="infection"&ve$variants_t2%in%c("Omicron (BA.1/BA.2)"),],col="#c29f62",se=F,method = 'lm',linetype=2)+
  #geom_smooth(data=ve[ve$outcome3=="infection"&ve$variants_t2%in%c("Omicron (BA.4/BA.5) and afterwards"),],col='darkolivegreen4',se=F,method = 'lm',linetype=2)+
  theme_cowplot()+
  labs(tag = "C",title = 'Government response index')+
  theme(plot.title = element_text(hjust = 0.5,face = 'plain',size = 17),plot.tag = element_text(size = 22,face = 'bold'),
        axis.title.x = element_text(size=13,hjust=0.47),axis.title.y = element_text(size=13),axis.text = element_text(size=11.5),
        axis.line.x=element_blank())+
  geom_segment(aes(x=-Inf,xend=Inf,y=-Inf,yend=-Inf),col="black",size=0.5)

h7 <- ggplot(ve[ve$outcome3=="severe",], aes(ave_stringencyindex*10, VE)) + 
  geom_point(data=ve[ve$outcome3=="severe"&ve$variants_t2=="pre-delta and delta",],size = 1.5, alpha = 0.5, shape = 16,col='cadetblue3')+
  geom_point(data=ve[ve$outcome3=="severe"&ve$variants_t2=="late-delta",],size = 1.5, alpha = 0.5, shape = 16,col="thistle3")+
  geom_point(data=ve[ve$outcome3=="severe"&ve$variants_t2%in%c("Omicron (BA.1/BA.2)"),],size = 1.5, alpha = 0.5, shape = 16,col="#c29f62")+
  geom_point(data=ve[ve$outcome3=="severe"&ve$variants_t2%in%c("Omicron (BA.4/BA.5) and afterwards"),],size = 1.5, alpha = 0.5, shape = 16,col='darkolivegreen4')+
  scale_x_continuous(name = "Average of index",limits = c(10,102), breaks = c(10,20,30,40,50,60,70,80,90,100)) +
  scale_y_continuous(name = "VE against severe disease",limits = c(10,105), breaks = c(10,20,30,40,50,60,70,80,90,100))+
  ##annotate(geom="text", x=120, y=55, label=bquote("r"[overall]~'='~.(pearson_alldata[7,4])),size=3.5,col="black")+
  ##annotate(geom="text", x=120, y=50, label= bquote(rho[overall]~'='~.(spearman_alldata[7,4])),size=3.5,col="black")+
  #annotate(geom="text", x=120, y=50, label= expression(paste(rho[overall]," = ","0.52 (0.44, 0.60)")),size=3.5,col="black")+
  ##annotate(geom="text", x=130, y=45, label=bquote("r"["pre-Delta and Delta"]~'='~.(pearson_alldata[8,4])),size=3.5,col='cadetblue3')+
  ##annotate(geom="text", x=129, y=40, label= bquote(rho["pre-Delta and Delta"]~'='~.(spearman_alldata[8,4])),size=3.5,col='cadetblue3')+
  #annotate(geom="text", x=128, y=40, label= expression(paste(rho["pre-Delta and Delta"]," = ","0.11 (-0.02, 0.24)")),size=3.5,col='cadetblue3')+
  ##annotate(geom="text", x=122, y=35, label=bquote("r"["late-Delta"]~'='~.(pearson_alldata[9,4])),size=3.5,col="thistle3")+
  ##annotate(geom="text", x=122, y=30, label= bquote(rho["late-Delta"]~'='~.(spearman_alldata[9,4])),size=3.5,col="thistle3")+
  #annotate(geom="text", x=122, y=30, label= expression(paste(rho["late-Delta"]," = ","0.53 (0.40, 0.65)")),size=3.5,col="thistle3")+
  ##annotate(geom="text", x=128, y=25, label=bquote("r"["Omicron (BA.1/BA.2)"]~'='~.(pearson_alldata[10,4])),size=3.5,col="#c29f62")+
  ##annotate(geom="text", x=129, y=20, label= bquote(rho["Omicron (BA.1/BA.2)"]~'='~.(spearman_alldata[10,4])),size=3.5,col="#c29f62")+
#annotate(geom="text", x=128, y=20, label= expression(paste(rho["Omicron (BA.1/BA.2)"]," = ","0.16 (-0.02, 0.33)")),size=3.5,col="#c29f62")+
##annotate(geom="text", x=129, y=15, label=bquote("r"["Omicron (BA.4/BA.5)"]~'='~.(pearson_alldata[11,4])),size=3.5,col='darkolivegreen4')+
##annotate(geom="text", x=129, y=10, label= bquote(rho["Omicron (BA.4/BA.5)"]~'='~.(spearman_alldata[11,4])),size=3.5,col='darkolivegreen4')+
#annotate(geom="text", x=128, y=10, label= expression(paste(rho["Omicron (BA.4/BA.5)"]," = ","0.26 (-0.15, 0.66)")),size=3.5,col='darkolivegreen4')+
geom_line(aes(ave_stringencyindex*10,predict(lm(VE~I(ave_stringencyindex*10)))),col="black",linetype=2,size=1)+
  geom_line(data=ve[ve$outcome3=="severe"&ve$variants_t2=="pre-delta and delta",],aes(ave_stringencyindex*10,predict(lm(VE~I(ave_stringencyindex*10)),newdata=ve[ve$outcome3=="severe"&ve$variants_t2=="pre-delta and delta",])),col='cadetblue3',linetype=2,size=1)+
  geom_line(data=ve[ve$outcome3=="severe"&ve$variants_t2=="late-delta",],aes(ave_stringencyindex*10,predict(lm(VE~I(ave_stringencyindex*10)),newdata=ve[ve$outcome3=="severe"&ve$variants_t2=="late-delta",])),col="thistle3",linetype=2,size=1)+
  geom_line(data=ve[ve$outcome3=="severe"&ve$variants_t2%in%c("Omicron (BA.1/BA.2)"),],aes(ave_stringencyindex*10,predict(lm(VE~I(ave_stringencyindex*10)),newdata=ve[ve$outcome3=="severe"&ve$variants_t2%in%c("Omicron (BA.1/BA.2)"),])),col="#c29f62",linetype=2,size=1)+
  geom_line(data=ve[ve$outcome3=="severe"&ve$variants_t2%in%c("Omicron (BA.4/BA.5) and afterwards"),],aes(ave_stringencyindex*10,predict(lm(VE~I(ave_stringencyindex*10)),newdata=ve[ve$outcome3=="severe"&ve$variants_t2%in%c("Omicron (BA.4/BA.5) and afterwards"),])),col='darkolivegreen4',linetype=2,size=1)+
  #geom_smooth(col="black",se=F,method = 'lm',linetype=2)+
  #geom_smooth(data=ve[ve$outcome3=="severe"&ve$variants_t2=="pre-delta and delta",],col='cadetblue3',se=F,method = 'lm',linetype=2)+
  #geom_smooth(data=ve[ve$outcome3=="severe"&ve$variants_t2=="late-delta",],col="thistle3",se=F,method = 'lm',linetype=2)+
  #geom_smooth(data=ve[ve$outcome3=="severe"&ve$variants_t2%in%c("Omicron (BA.1/BA.2)"),],col="#c29f62",se=F,method = 'lm',linetype=2)+
  #geom_smooth(data=ve[ve$outcome3=="severe"&ve$variants_t2%in%c("Omicron (BA.4/BA.5) and afterwards"),],col='darkolivegreen4',se=F,method = 'lm',linetype=2)+
  theme_cowplot()+
  labs(tag = "D",title = 'Stringency index')+
  theme(plot.title = element_text(hjust = 0.5,face = 'plain',size = 17),plot.tag = element_text(size = 22,face = 'bold'),
        axis.title.x = element_text(size=13,hjust=0.47),axis.title.y = element_text(size=13),axis.text = element_text(size=11.5),
        axis.line.x=element_blank())+
  geom_segment(aes(x=-Inf,xend=Inf,y=-Inf,yend=-Inf),col="black",size=0.5)

h8 <- ggplot(ve[ve$outcome3=="severe",], aes(ave_containmenthealthindex*10, VE)) + 
  geom_point(data=ve[ve$outcome3=="severe"&ve$variants_t2=="pre-delta and delta",],size = 1.5, alpha = 0.5, shape = 16,col='cadetblue3')+
  geom_point(data=ve[ve$outcome3=="severe"&ve$variants_t2=="late-delta",],size = 1.5, alpha = 0.5, shape = 16,col="thistle3")+
  geom_point(data=ve[ve$outcome3=="severe"&ve$variants_t2%in%c("Omicron (BA.1/BA.2)"),],size = 1.5, alpha = 0.5, shape = 16,col="#c29f62")+
  geom_point(data=ve[ve$outcome3=="severe"&ve$variants_t2%in%c("Omicron (BA.4/BA.5) and afterwards"),],size = 1.5, alpha = 0.5, shape = 16,col='darkolivegreen4')+
  scale_x_continuous(name = "Average of index",limits = c(10,102), breaks = c(10,20,30,40,50,60,70,80,90,100)) +
  scale_y_continuous(name = "VE against severe disease",limits = c(10,105), breaks = c(10,20,30,40,50,60,70,80,90,100))+
  ##annotate(geom="text", x=120, y=55, label=bquote("r"[overall]~'='~.(pearson_alldata[7,8])),size=3.5,col="black")+
  ##annotate(geom="text", x=120, y=50, label= bquote(rho[overall]~'='~.(spearman_alldata[7,8])),size=3.5,col="black")+
  #annotate(geom="text", x=120, y=50, label= expression(paste(rho[overall]," = ","0.58 (0.52, 0.64)")),size=3.5,col="black")+
  ##annotate(geom="text", x=127, y=45, label=bquote("r"["pre-Delta and Delta"]~'='~.(pearson_alldata[8,8])),size=3.5,col='cadetblue3')+
  ##annotate(geom="text", x=127, y=40, label= bquote(rho["pre-Delta and Delta"]~'='~.(spearman_alldata[8,8])),size=3.5,col='cadetblue3')+
  #annotate(geom="text", x=127, y=40, label= expression(paste(rho["pre-Delta and Delta"]," = ","0.30 (0.18, 0.43)")),size=3.5,col='cadetblue3')+
  ##annotate(geom="text", x=122, y=35, label=bquote("r"["late-Delta"]~'='~.(pearson_alldata[9,8])),size=3.5,col="thistle3")+
  ##annotate(geom="text", x=122, y=30, label= bquote(rho["late-Delta"]~'='~.(spearman_alldata[9,8])),size=3.5,col="thistle3")+
  #annotate(geom="text", x=122, y=30, label= expression(paste(rho["late-Delta"]," = ","0.53 (0.41, 0.66)")),size=3.5,col="thistle3")+
  ##annotate(geom="text", x=130, y=25, label=bquote("r"["Omicron (BA.1/BA.2)"]~'='~.(pearson_alldata[10,8])),size=3.5,col="#c29f62")+
  ##annotate(geom="text", x=129, y=20, label= bquote(rho["Omicron (BA.1/BA.2)"]~'='~.(spearman_alldata[10,8])),size=3.5,col="#c29f62")+
#annotate(geom="text", x=129, y=20, label= expression(paste(rho["Omicron (BA.1/BA.2)"]," = ","0.02 (-0.17, 0.20)")),size=3.5,col="#c29f62")+
##annotate(geom="text", x=129, y=15, label=bquote("r"["Omicron (BA.4/BA.5)"]~'='~.(pearson_alldata[11,8])),size=3.5,col='darkolivegreen4')+
##annotate(geom="text", x=129, y=10, label= bquote(rho["Omicron (BA.4/BA.5)"]~'='~.(spearman_alldata[11,8])),size=3.5,col='darkolivegreen4')+
#annotate(geom="text", x=129, y=10, label= expression(paste(rho["Omicron (BA.4/BA.5)"]," = ","0.002 (-0.41, 0.42)")),size=3.5,col='darkolivegreen4')+
geom_line(aes(ave_containmenthealthindex*10,predict(lm(VE~I(ave_containmenthealthindex*10)))),col="black",linetype=2,size=1)+
  geom_line(data=ve[ve$outcome3=="severe"&ve$variants_t2=="pre-delta and delta",],aes(ave_containmenthealthindex*10,predict(lm(VE~I(ave_containmenthealthindex*10)),newdata=ve[ve$outcome3=="severe"&ve$variants_t2=="pre-delta and delta",])),col='cadetblue3',linetype=2,size=1)+
  geom_line(data=ve[ve$outcome3=="severe"&ve$variants_t2=="late-delta",],aes(ave_containmenthealthindex*10,predict(lm(VE~I(ave_containmenthealthindex*10)),newdata=ve[ve$outcome3=="severe"&ve$variants_t2=="late-delta",])),col="thistle3",linetype=2,size=1)+
  geom_line(data=ve[ve$outcome3=="severe"&ve$variants_t2%in%c("Omicron (BA.1/BA.2)"),],aes(ave_containmenthealthindex*10,predict(lm(VE~I(ave_containmenthealthindex*10)),newdata=ve[ve$outcome3=="severe"&ve$variants_t2%in%c("Omicron (BA.1/BA.2)"),])),col="#c29f62",linetype=2,size=1)+
  geom_line(data=ve[ve$outcome3=="severe"&ve$variants_t2%in%c("Omicron (BA.4/BA.5) and afterwards"),],aes(ave_containmenthealthindex*10,predict(lm(VE~I(ave_containmenthealthindex*10)),newdata=ve[ve$outcome3=="severe"&ve$variants_t2%in%c("Omicron (BA.4/BA.5) and afterwards"),])),col='darkolivegreen4',linetype=2,size=1)+
  #geom_smooth(col="black",se=F,method = 'lm',linetype=2)+
  #geom_smooth(data=ve[ve$outcome3=="severe"&ve$variants_t2=="pre-delta and delta",],col='cadetblue3',se=F,method = 'lm',linetype=2)+
  #geom_smooth(data=ve[ve$outcome3=="severe"&ve$variants_t2=="late-delta",],col="thistle3",se=F,method = 'lm',linetype=2)+
  #geom_smooth(data=ve[ve$outcome3=="severe"&ve$variants_t2%in%c("Omicron (BA.1/BA.2)"),],col="#c29f62",se=F,method = 'lm',linetype=2)+
  #geom_smooth(data=ve[ve$outcome3=="severe"&ve$variants_t2%in%c("Omicron (BA.4/BA.5) and afterwards"),],col='darkolivegreen4',se=F,method = 'lm',linetype=2)+
  theme_cowplot()+
  labs(tag = "E",title = 'Containment health index')+
  theme(plot.title = element_text(hjust = 0.5,face = 'plain',size = 17),plot.tag = element_text(size = 22,face = 'bold'),
        axis.title.x = element_text(size=13,hjust=0.47),axis.title.y = element_text(size=13),axis.text = element_text(size=11.5),
        axis.line.x=element_blank())+
  geom_segment(aes(x=-Inf,xend=Inf,y=-Inf,yend=-Inf),col="black",size=0.5)

h9 <- ggplot(ve[ve$outcome3=="severe",], aes(ave_governmentresponseindex*10, VE)) + 
  geom_point(data=ve[ve$outcome3=="severe"&ve$variants_t2=="pre-delta and delta",],size = 1.5, alpha = 0.5, shape = 16,col='cadetblue3')+
  geom_point(data=ve[ve$outcome3=="severe"&ve$variants_t2=="late-delta",],size = 1.5, alpha = 0.5, shape = 16,col="thistle3")+
  geom_point(data=ve[ve$outcome3=="severe"&ve$variants_t2%in%c("Omicron (BA.1/BA.2)"),],size = 1.5, alpha = 0.5, shape = 16,col="#c29f62")+
  geom_point(data=ve[ve$outcome3=="severe"&ve$variants_t2%in%c("Omicron (BA.4/BA.5) and afterwards"),],size = 1.5, alpha = 0.5, shape = 16,col='darkolivegreen4')+
  scale_x_continuous(name = "Average of index",limits = c(10,102), breaks = c(10,20,30,40,50,60,70,80,90,100)) +
  scale_y_continuous(name = "VE against severe disease",limits = c(10,105), breaks = c(10,20,30,40,50,60,70,80,90,100))+
  ##annotate(geom="text", x=120, y=55, label=bquote("r"[overall]~'='~.(pearson_alldata[7,12])),size=3.5,col="black")+
  ##annotate(geom="text", x=120, y=50, label= bquote(rho[overall]~'='~.(spearman_alldata[7,12])),size=3.5,col="black")+
  #annotate(geom="text", x=120, y=50, label= expression(paste(rho[overall]," = ","0.53 (0.46, 0.60)")),size=3.5,col="black")+
  ##annotate(geom="text", x=127, y=45, label=bquote("r"["pre-delta and delta"]~'='~.(pearson_alldata[8,12])),size=3.5,col='cadetblue3')+
  ##annotate(geom="text", x=127, y=40, label= bquote(rho["pre-delta and delta"]~'='~.(spearman_alldata[8,12])),size=3.5,col='cadetblue3')+
  #annotate(geom="text", x=127, y=40, label= expression(paste(rho["pre-delta and delta"]," = ","0.35 (0.22, 0.47)")),size=3.5,col='cadetblue3')+
  ##annotate(geom="text", x=122, y=35, label=bquote("r"["late-delta"]~'='~.(pearson_alldata[9,12])),size=3.5,col="thistle3")+
  ##annotate(geom="text", x=122, y=30, label= bquote(rho["late-delta"]~'='~.(spearman_alldata[9,12])),size=3.5,col="thistle3")+
  #annotate(geom="text", x=122, y=30, label= expression(paste(rho["late-delta"]," = ","0.46 (0.33, 0.60)")),size=3.5,col="thistle3")+
  ##annotate(geom="text", x=129, y=25, label=bquote("r"["Omicron (BA.1/BA.2)"]~'='~.(pearson_alldata[10,12])),size=3.5,col="#c29f62")+
  ##annotate(geom="text", x=129, y=20, label= bquote(rho["Omicron (BA.1/BA.2)"]~'='~.(spearman_alldata[10,12])),size=3.5,col="#c29f62")+
#annotate(geom="text", x=129, y=20, label= expression(paste(rho["Omicron (BA.1/BA.2)"]," = ","-0.01 (-0.19, 0.17)")),size=3.5,col="#c29f62")+
##annotate(geom="text", x=128, y=15, label=bquote("r"["Omicron (BA.4/BA.5)"]~'='~.(pearson_alldata[11,12])),size=3.5,col='darkolivegreen4')+
##annotate(geom="text", x=128, y=10, label= bquote(rho["Omicron (BA.4/BA.5)"]~'='~.(spearman_alldata[11,12])),size=3.5,col='darkolivegreen4')+
#annotate(geom="text", x=128, y=10, label= expression(paste(rho["Omicron (BA.4/BA.5)"]," = ","0.26 (-0.15, 0.66)")),size=3.5,col='darkolivegreen4')+
geom_line(aes(ave_governmentresponseindex*10,predict(lm(VE~I(ave_governmentresponseindex*10)))),col="black",linetype=2,size=1)+
  geom_line(data=ve[ve$outcome3=="severe"&ve$variants_t2=="pre-delta and delta",],aes(ave_governmentresponseindex*10,predict(lm(VE~I(ave_governmentresponseindex*10)),newdata=ve[ve$outcome3=="severe"&ve$variants_t2=="pre-delta and delta",])),col='cadetblue3',linetype=2,size=1)+
  geom_line(data=ve[ve$outcome3=="severe"&ve$variants_t2=="late-delta",],aes(ave_governmentresponseindex*10,predict(lm(VE~I(ave_governmentresponseindex*10)),newdata=ve[ve$outcome3=="severe"&ve$variants_t2=="late-delta",])),col="thistle3",linetype=2,size=1)+
  geom_line(data=ve[ve$outcome3=="severe"&ve$variants_t2%in%c("Omicron (BA.1/BA.2)"),],aes(ave_governmentresponseindex*10,predict(lm(VE~I(ave_governmentresponseindex*10)),newdata=ve[ve$outcome3=="severe"&ve$variants_t2%in%c("Omicron (BA.1/BA.2)"),])),col="#c29f62",linetype=2,size=1)+
  geom_line(data=ve[ve$outcome3=="severe"&ve$variants_t2%in%c("Omicron (BA.4/BA.5) and afterwards"),],aes(ave_governmentresponseindex*10,predict(lm(VE~I(ave_governmentresponseindex*10)),newdata=ve[ve$outcome3=="severe"&ve$variants_t2%in%c("Omicron (BA.4/BA.5) and afterwards"),])),col='darkolivegreen4',linetype=2,size=1)+
  #geom_smooth(col="black",se=F,method = 'lm',linetype=2)+
  #geom_smooth(data=ve[ve$outcome3=="severe"&ve$variants_t2=="pre-delta and delta",],col='cadetblue3',se=F,method = 'lm',linetype=2)+
  #geom_smooth(data=ve[ve$outcome3=="severe"&ve$variants_t2=="late-delta",],col="thistle3",se=F,method = 'lm',linetype=2)+
  #geom_smooth(data=ve[ve$outcome3=="severe"&ve$variants_t2%in%c("Omicron (BA.1/BA.2)"),],col="#c29f62",se=F,method = 'lm',linetype=2)+
  #geom_smooth(data=ve[ve$outcome3=="severe"&ve$variants_t2%in%c("Omicron (BA.4/BA.5) and afterwards"),],col='darkolivegreen4',se=F,method = 'lm',linetype=2)+
  theme_cowplot()+
  labs(tag = "F",title = 'Government response index')+
  theme(plot.title = element_text(hjust = 0.5,face = 'plain',size = 17),plot.tag = element_text(size = 22,face = 'bold'),
        axis.title.x = element_text(size=13,hjust=0.47),axis.title.y = element_text(size=13),axis.text = element_text(size=11.5),
        axis.line.x=element_blank())+
  geom_segment(aes(x=-Inf,xend=Inf,y=-Inf,yend=-Inf),col="black",size=0.5)

pdf("/Users/timtsang/SPH Dropbox/Tim Tsang/Shared COVID VE review/GRI/upload/fig/VE_TND_index_plot_alltiming_v3(remove correlation).pdf",height = 8,width = 15)
grid.arrange(h4,h5,h6,h7,h8,h9,nrow = 2,ncol=3)
dev.off()





