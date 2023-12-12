#Replication Script for "Online Images Amplify Gender Bias"
#Douglas R. Guilbeault
#Haas School of Business
#University of California, Berkeley

rm(list=ls());gc();
library(dplyr);library(ggplot2);library(tidyverse);library(tidyr);library(aod);library(lme4);library(nlme);library(glmmTMB);
library(sjPlot);library(clinfun);library(multiwayvcov);library(lmtest);require(MASS);library(fmsb);library(Hmisc); 
library(sandwich); library(xtable)
library(irrCAC) #https://search.r-project.org/CRAN/refmans/irrCAC/html/gwet.ac1.raw.html
#library(remotes)
#remotes::install_github("Lakens/TOSTER") #load for wilcox equivalence test
library(TOSTER)

#functions
min_max_norm<-function(x){(x - min(x,na.rm=T))/(max(x,na.rm=T) - min(x,na.rm=T))}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Set path to folder containing data files
data_path<-""

####################
#Load/Organize Data#
####################
data_raw<-read.csv(paste(data_path, "data_obsv.csv", sep="")) #load main observational data set
data_clean<-subset(data_raw, Human.Face == "Yes" & Attention.Check)
data_binary<-subset(data_clean, Img.Gender != "Non-binary")
data_main<-subset(data_binary, !searchDEMO %in% c("Male", "Female"))
data_demo<-subset(data_binary, searchDEMO %in% c("Male", "Female"))

data_binary_mode_agg<-data_binary %>% 
  group_by(Data.Source, Social.Category, searchDEMO, face_id, image_id, cps_majorgroup, cps_group, cps_occupation, census.women, 
           Gendered.Category, Polysemy, Word.Frequency.Scaled, Word.Frequency.General, GoogleNews.300D.Gender,GoogleNews.300D.Gender.Norm,
           Google.Img.US.Search.Freq, Human.Gender.Judge,Wiki.50D.Gender, Wiki.50D.Gender.Norm, Wiki.100D.Gender, Wiki.100D.Gender.Norm, Wiki.200D.Gender, Wiki.200D.Gender.Norm, 
           Wiki.300D.Gender, Wiki.300D.Gender.Norm, Twitter.200D.Gender, Bert, Bert.Norm, ConceptNet, ConceptNet.Norm, FastText, FastText.Norm, Glove.Twitter, Glove.Twitter.Norm, 
           Glove.Wiki, Glove.Wiki.Norm, GoogleNews.300D.Gender.v2, GoogleNews.300D.Gender.v3, GoogleNews.300D.Gender.v4, GoogleNews.300D.Gender.v2.Norm, GoogleNews.300D.Gender.v3.Norm, 
           GoogleNews.300D.Gender.v4.Norm, GPT3.gender, GPT3.gender.norm, retrained.word2vec.300, retrained.word2vec.300.Norm) %>% 
  dplyr::summarise(Img.Gender.Mode=getmode(Img.Gender))

data_binary_mode_agg_macro<-data_binary_mode_agg %>% 
  group_by(Data.Source, Social.Category, searchDEMO, cps_majorgroup, cps_group, cps_occupation, census.women, 
           Gendered.Category, Polysemy, Word.Frequency.Scaled, Word.Frequency.General, GoogleNews.300D.Gender,GoogleNews.300D.Gender.Norm,
           Google.Img.US.Search.Freq, Human.Gender.Judge,Wiki.50D.Gender, Wiki.50D.Gender.Norm, Wiki.100D.Gender, Wiki.100D.Gender.Norm, Wiki.200D.Gender, Wiki.200D.Gender.Norm, 
           Wiki.300D.Gender, Wiki.300D.Gender.Norm, Twitter.200D.Gender, Bert, Bert.Norm, ConceptNet, ConceptNet.Norm, FastText, FastText.Norm, Glove.Twitter, Glove.Twitter.Norm, 
           Glove.Wiki, Glove.Wiki.Norm, GoogleNews.300D.Gender.v2, GoogleNews.300D.Gender.v3, GoogleNews.300D.Gender.v4, GoogleNews.300D.Gender.v2.Norm, GoogleNews.300D.Gender.v3.Norm, 
           GoogleNews.300D.Gender.v4.Norm, GPT3.gender, GPT3.gender.norm, retrained.word2vec.300, retrained.word2vec.300.Norm, Img.Gender.Mode) %>% 
  dplyr::summarise(num_faces=length(unique(face_id))) %>% group_by(Social.Category, searchDEMO, Data.Source) %>% 
  dplyr::mutate(frac_faces = num_faces/sum(num_faces)) %>% mutate(freq=1)

#Account for categories without faces for a particular gender
data_binary_mode_agg_macro_split<-subset(data_binary_mode_agg_macro, frac_faces<1)
data_binary_mode_agg_macro_max<-subset(data_binary_mode_agg_macro, frac_faces==1)
data_binary_mode_agg_macro_max_rev<-data_binary_mode_agg_macro_max
data_binary_mode_agg_macro_max_rev$Img.Gender.Mode<-as.factor(data_binary_mode_agg_macro_max_rev$Img.Gender.Mode)
levels(data_binary_mode_agg_macro_max_rev$Img.Gender.Mode)<-c("Male","Female")
data_binary_mode_agg_macro_max_rev$num_faces<-0
data_binary_mode_agg_macro_max_rev$frac_faces<-0
data_binary_mode_agg_macro_org<-rbind(data_binary_mode_agg_macro_split, data_binary_mode_agg_macro_max, data_binary_mode_agg_macro_max_rev)

#Get Image Gender Parity
data_binary_mode_agg_macro_final<-data_binary_mode_agg_macro_org %>% 
  group_by(Data.Source, Social.Category, searchDEMO, cps_majorgroup, cps_group, cps_occupation, census.women, 
           Gendered.Category, Polysemy, Word.Frequency.Scaled, Word.Frequency.General, GoogleNews.300D.Gender,GoogleNews.300D.Gender.Norm,
           Google.Img.US.Search.Freq, Human.Gender.Judge,Wiki.50D.Gender, Wiki.50D.Gender.Norm, Wiki.100D.Gender, Wiki.100D.Gender.Norm, Wiki.200D.Gender, Wiki.200D.Gender.Norm, 
           Wiki.300D.Gender, Wiki.300D.Gender.Norm, Twitter.200D.Gender, Bert, Bert.Norm, ConceptNet, ConceptNet.Norm, FastText, FastText.Norm, Glove.Twitter, Glove.Twitter.Norm, 
           Glove.Wiki, Glove.Wiki.Norm, GoogleNews.300D.Gender.v2, GoogleNews.300D.Gender.v3, GoogleNews.300D.Gender.v4, GoogleNews.300D.Gender.v2.Norm, GoogleNews.300D.Gender.v3.Norm, 
           GoogleNews.300D.Gender.v4.Norm, GPT3.gender, GPT3.gender.norm, retrained.word2vec.300, retrained.word2vec.300.Norm) %>% 
  dplyr::summarise(Img.Gender.Parity = frac_faces[Img.Gender.Mode=="Male"] - frac_faces[Img.Gender.Mode=="Female"],
                   Img.Prop.Male = frac_faces[Img.Gender.Mode=="Male"],
                   num_faces=sum(num_faces)) %>%
  mutate(Img.Gender.Bias.Str=abs(Img.Gender.Parity), Text.Gender.Bias.Str=abs(GoogleNews.300D.Gender.Norm))

comparison_data_main_Google<-subset(data_binary_mode_agg_macro_final, searchDEMO=="None" & Data.Source=="Google")
comparison_data_main_Google$Img.Gender.Parity.Bin<-ntile(comparison_data_main_Google$Img.Gender.Parity, 10)

##################
###Main Figures###
##################
savepath_main<-""  #Set the path for where you want the figures to be saved. 

##########
##FIG 1A##
##########
fig1A_data_agg<-comparison_data_main_Google %>% group_by(Img.Gender.Parity.Bin) %>% 
  dplyr::summarise(cilow=t.test(GoogleNews.300D.Gender.Norm)$conf.int[1],
                   cihi=t.test(GoogleNews.300D.Gender.Norm)$conf.int[2],
                   GoogleNews.300D.Gender.Norm = mean(GoogleNews.300D.Gender.Norm, na.rm=T), 
                   Img.Gender.Parity = mean(Img.Gender.Parity, na.rm=T))

cor.test(fig1A_data_agg$GoogleNews.300D.Gender.Norm, fig1A_data_agg$Img.Gender.Parity)
cor.test(comparison_data_main_Google$GoogleNews.300D.Gender.Norm, comparison_data_main_Google$Img.Gender.Parity)

ggplot(fig1A_data_agg, aes(x=Img.Gender.Parity, y=GoogleNews.300D.Gender.Norm))+
  geom_point(size=15, color="black") + geom_line(size=3) +
  geom_errorbar(aes(ymin=cilow, ymax=cihi), linetype="solid",width=0, size=2)+
  xlab("Gender Association (Images)") + ylab("Gender Association (Text)") + theme_bw() + 
  theme(axis.text.x = element_text(size=75), axis.text.y = element_text(size=75),
        axis.title.x = element_text(size=70),axis.title.y = element_text(size=70),
        plot.title = element_blank(), 
        legend.position = "none", legend.text=element_text(size=30),
        legend.title=element_text(size=30, face="bold"),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  geom_hline(yintercept = 0, linetype="dotted", size=3) + 
  geom_vline(xintercept = 0, linetype="dotted", size=3) + 
  coord_cartesian(ylim=c(-0.3, 0.3))

#ggsave('Fig1A.png', width=16, height=16, path = savepath_main)

##########
##FIG 1B##
##########
Google.Images.Fem<-subset(comparison_data_main_Google, Img.Gender.Parity<0)
Google.Images.Fem$fig1b_cond<-"Female\nCategories"
Google.Images.Fem$fig1b_measure<-"Images"
Google.Images.Fem$fig1b_gender_assoc<-Google.Images.Fem$Img.Gender.Parity
Google.Images.Fem.simp<-Google.Images.Fem[,c("Social.Category", "searchDEMO","fig1b_cond","fig1b_measure","fig1b_gender_assoc")]
Google.Images.Male<-subset(comparison_data_main_Google, Img.Gender.Parity>0)
Google.Images.Male$fig1b_cond<-"Male\nCategories"
Google.Images.Male$fig1b_measure<-"Images"
Google.Images.Male$fig1b_gender_assoc<-Google.Images.Male$Img.Gender.Parity
Google.Images.Male.simp<-Google.Images.Male[,c("Social.Category", "searchDEMO","fig1b_cond","fig1b_measure","fig1b_gender_assoc")]

Google.Text.Fem<-subset(comparison_data_main_Google, GoogleNews.300D.Gender.Norm<0)
Google.Text.Fem$fig1b_cond<-"Female\nCategories"
Google.Text.Fem$fig1b_measure<-"Text"
Google.Text.Fem$fig1b_gender_assoc<-Google.Text.Fem$GoogleNews.300D.Gender.Norm
Google.Text.Fem.simp<-Google.Text.Fem[,c("Social.Category", "searchDEMO","fig1b_cond","fig1b_measure","fig1b_gender_assoc")]
Google.Text.Male<-subset(comparison_data_main_Google, GoogleNews.300D.Gender.Norm>0)
Google.Text.Male$fig1b_cond<-"Male\nCategories"
Google.Text.Male$fig1b_measure<-"Text"
Google.Text.Male$fig1b_gender_assoc<-Google.Text.Male$GoogleNews.300D.Gender.Norm
Google.Text.Male.simp<-Google.Text.Male[,c("Social.Category", "searchDEMO","fig1b_cond","fig1b_measure","fig1b_gender_assoc")]

fig1B<-rbind(Google.Text.Fem.simp, Google.Text.Male.simp, Google.Images.Fem.simp, Google.Images.Male.simp)
fig1B$figb_combo_cond<-paste(fig1B$fig1b_cond, fig1B$fig1b_measure, sep="_")
fig1B_main<-subset(fig1B, searchDEMO=="None")

ggplot(fig1B_main, aes(x = fig1b_gender_assoc, y = figb_combo_cond, fill =  fig1b_measure, group=figb_combo_cond))+
  geom_boxplot(size=2, outlier.size=0, outlier.colour = "white") +  xlab("Gender Association") + theme_bw() + 
  scale_fill_manual(values=c("white", "grey")) + 
  theme(axis.text.x = element_text(size=74), axis.text.y = element_blank(),
        axis.title.y = element_blank(),axis.title.x = element_text(size=75),
        legend.position = c(0.2, 0.85), legend.text=element_text(size=82),
        legend.title=element_blank(),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + geom_vline(xintercept=0,size=1, linetype="dotted")

#ggsave('Fig1B.png', width=14, height=16, path = savepath_main)

#############
##Figure 1C##
#############
female_occupations<-c("cosmetologist",  "nutritionist", "beauty consultant", "ballet dancer", "model",  "dental hygienist", "dietician", "hairstylist", "cheerleader",  "event planner",  "nurse practitioner", 
                     "interior decorator", "housekeeper", "art teacher", "cashier", "cook", "tv reporter", "legal assistant", "flight attendant")
male_occupations<-c("blacksmith",  "developer",  "plumber", "detective", "football player", "police chief", "investment banker", "monk", "electrician", "carpenter",  
                    "farmer",   "comedian", "heart surgeon", "mathematician",  "philosopher", "military officer", "ceo",  "mechanic", "programmer")

########################
fig1C<-subset(comparison_data_main_Google, Social.Category %in% female_occupations | Social.Category %in% male_occupations)
fig1C<-fig1C %>% arrange(Img.Gender.Parity)
fig1C$id<-1:nrow(fig1C)
fig1C<-fig1C[,c("Social.Category", "GoogleNews.300D.Gender.Norm", "Img.Gender.Parity", "id")]
fig1C<-gather(fig1C, measure, measurement, GoogleNews.300D.Gender.Norm:Img.Gender.Parity)
fig1C$measure<-as.factor(fig1C$measure)
levels(fig1C$measure)<-c("Text","Images")
fig1C_male<-subset(fig1C, measurement>0) 
male_order<-subset(fig1C_male, measure != "Text") %>% arrange(measurement)
male_order_labels<-as.character(male_order$Social.Category)
fig1C_female<-subset(fig1C, measurement<0) 
female_order<-subset(fig1C_female, measure != "Text") %>% arrange(measurement)
female_order_labels<-as.character(female_order$Social.Category)
label_order<-unique(unlist(c(female_order_labels, male_order_labels)))
fig1C$Social.Category<-factor(fig1C$Social.Category, levels=label_order)

ggplot(fig1C, aes(y=Social.Category, x=measurement, shape=measure, group=measure)) +
  geom_point(size=6) + scale_shape_manual(values=c(17,9)) + xlab("Gender Association") + 
  theme_bw() + theme(axis.text.x = element_text(size=30),
                     axis.text.y = element_text(size=18),
                     axis.title.x = element_text(size=35),
                     axis.title.y = element_blank(),
                     strip.text.x = element_text(size=40),
                     legend.position = c(0.8,0.15), 
                     legend.title=element_blank(),
                     legend.text = element_text(size=30),
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  geom_vline(xintercept = 0, linetype="dotted", size=1) + 
  coord_cartesian(xlim=c(-1,1))

#ggsave('Fig1C.png', width=10, height=10, path = savepath_main)

############
###Fig 2A###
############
fig2A_raw<-rbind(data.frame(Measure="Text", Gender.Parity = comparison_data_main_Google$GoogleNews.300D.Gender.Norm, 
                        Social.Category=comparison_data_main_Google$Social.Category), 
             data.frame(Measure="Images", Gender.Parity = comparison_data_main_Google$Img.Gender.Parity, 
                        Social.Category=comparison_data_main_Google$Social.Category))

fig2A<-fig2A_raw[complete.cases(fig2A_raw),]
fig2A<-subset(fig2A, Social.Category %in% unique(subset(fig2A, Measure=="Text")$Social.Category))

ggplot(fig2A, aes(x = Gender.Parity, fill=Measure)) +
  geom_density(alpha=0.6, size=1.2) + theme_bw() + 
  scale_fill_manual(values=c("purple", "forestgreen")) + ylab("Source") + xlab("Gender Association") + 
  theme(legend.text=element_text(size=50),
        legend.position=c(0.2,0.9),
        legend.title = element_blank(),
        plot.title=element_blank(),axis.title.y=element_blank(),
        axis.title.x=element_text(size = 50, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.6),
        axis.text.y=element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +   
  scale_x_continuous(breaks=c(-1,-0.5, 0, 0.5, 1), labels =c(-1,-0.5, 0, 0.5, 1), limits = c(-1.4, 1.4)) + 
  geom_vline(xintercept = 0, linetype="dotted", size=4) + 
  geom_vline(xintercept = mean(subset(fig2A, Measure=="Text")$Gender.Parity), linetype="solid", size=4, color="forestgreen") + 
  geom_vline(xintercept = mean(subset(fig2A, Measure!="Text")$Gender.Parity), linetype="solid", size=4, color="purple") + 
  annotate(geom="text", x=-1, y=0.8, size=15, label="Female",color="Black") + 
  annotate(geom="text", x=1.1, y=0.8, size=15, label="Male",color="Black")

#ggsave('Fig2A.png', width=11, height=11, path = savepath_main)
fig2A %>% group_by(Measure) %>% dplyr::summarise(maleskew=sum(Gender.Parity>0)/length(Gender.Parity))

############
###Fig 2B###
############
comparison_data_main_Google$Human.Judge.Bin<-ntile(comparison_data_main_Google$Human.Gender.Judge, 25)
fig2B<-comparison_data_main_Google %>% group_by(Human.Judge.Bin) %>% 
  dplyr::summarise(Images = mean(Img.Gender.Parity, na.rm=T), 
                   Text = mean(GoogleNews.300D.Gender.Norm, na.rm=T),
                   Human.Judgments = mean(Human.Gender.Judge, na.rm=T))
fig2B<-rbind(data.frame(Human.Judge.Bin = fig2B$Human.Judge.Bin, Parity=fig2B$Images, Method="Images"), 
             data.frame(Human.Judge.Bin = fig2B$Human.Judge.Bin, Parity=fig2B$Text, Method="Text"), 
             data.frame(Human.Judge.Bin = fig2B$Human.Judge.Bin, Parity=fig2B$Human.Judgments, Method="Human.Judgments"))

ggplot(fig2B, aes(x=Human.Judge.Bin, y=Parity, color = Method, shape=Method, group=Method))+
  geom_point(size=12, alpha=0.6) + 
  geom_smooth(aes(color=Method, fill=Method), size=1, alpha=0.3) + 
  scale_color_manual(values=c("blue","purple", "forestgreen")) + 
  scale_fill_manual(values=c("blue","purple", "forestgreen")) + 
  xlab("Gender Association\n(Binned Human Judgments)") + 
  ylab("Gender Association") +
  theme_bw() + theme(axis.text.x = element_text(size=60),
                     axis.text.y = element_text(size=60),
                     axis.title.x = element_text(size=60),
                     axis.title.y = element_text(size=60),
                     legend.position = c(0.27, 0.9), 
                     legend.text=element_text(size=45),
                     legend.title=element_blank(),
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black")) + 
  geom_hline(yintercept = 0, linetype="solid", size=2) + 
  coord_cartesian(xlim=c(0,25), ylim=c(-0.45,0.45)) + 
  annotate(geom="text", x=20, y=0.45, size=15, label="Male Bias",color="Black") + 
  annotate(geom="text", x=20, y=-0.45, size=15, label="Female Bias",color="Black") 

#ggsave('Fig2B.png', width=14, height=14, path = savepath_main)

wilcox.test(comparison_data_main_Google$GoogleNews.300D.Gender.Norm, 
            comparison_data_main_Google$Human.Gender.Judge, paired=T)

wilcox.test(comparison_data_main_Google$Img.Gender.Parity, 
            comparison_data_main_Google$Human.Gender.Judge, paired=T)

diff1<-c(comparison_data_main_Google$Img.Gender.Parity - comparison_data_main_Google$Human.Gender.Judge)
diff2<-c(comparison_data_main_Google$GoogleNews.300D.Gender.Norm - comparison_data_main_Google$Human.Gender.Judge)
wilcox.test(diff1, diff2, paired=T)
mean(comparison_data_main_Google$Img.Gender.Parity - comparison_data_main_Google$Human.Gender.Judge, na.rm=T)
mean(comparison_data_main_Google$GoogleNews.300D.Gender.Norm - comparison_data_main_Google$Human.Gender.Judge, na.rm=T)

############
###Fig 2C###
############
comparison_data_main_Google$census.male<-1 - comparison_data_main_Google$census.women
comparison_data_main_Google$census.gender.parity<- (comparison_data_main_Google$census.male - comparison_data_main_Google$census.women)

fig2C<-subset(comparison_data_main_Google, !is.na(census.gender.parity))

fig2C_long<-rbind(
  data.frame(Measure="Images", Social.Category = fig2C$Social.Category, Gender.Association=fig2C$Img.Gender.Parity),
  data.frame(Measure="Census", Social.Category = fig2C$Social.Category, Gender.Association=fig2C$census.gender.parity),
  data.frame(Measure="Text", Social.Category = fig2C$Social.Category, Gender.Association=fig2C$GoogleNews.300D.Gender.Norm))

fig2C_long_agg<-fig2C_long %>% group_by(Measure) %>% 
  dplyr::summarise(cilow=t.test(Gender.Association)$conf.int[1],
                   cihi=t.test(Gender.Association)$conf.int[2],
                   Gender.Association=mean(Gender.Association, na.rm=T))

fig2C_long_agg$Measure<-factor(fig2C_long_agg$Measure, levels=rev(c("Text", "Census", "Images")))

ggplot(fig2C_long_agg, aes(x = Gender.Association, y = Measure, color =  Measure, group=Measure))+
  geom_point(size=22) +  xlab("Gender Association") + theme_bw() + 
  geom_errorbar(aes(xmin=cilow, xmax=cihi), linetype="solid",width=0.4, size=8)+
  scale_color_manual(values=c("purple","darkgrey","forestgreen"))+
  theme(axis.text.x = element_text(size=74), axis.text.y = element_text(size=0),
        axis.title.y = element_blank(),axis.title.x = element_text(size=75),
        legend.position = "none", legend.text=element_text(size=82),
        legend.title=element_blank(),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  geom_vline(xintercept=0,size=1, linetype="solid") + 
  scale_x_continuous(limits = c(-0.1,0.2)) + 
  annotate(geom="text", x=-0.06, y=3.4, size=20, label="Female Bias",color="Black") + 
  annotate(geom="text", x=0.15, y=3.4, size=20, label="Male Bias",color="Black")

#ggsave('Fig2C.png', width=16, height=16, path = savepath_main)

wilcox.test(fig2C$GoogleNews.300D.Gender.Norm)
t.test(fig2C$census.gender.parity, fig2C$Img.Gender.Parity, paired=T)
t.test(fig2C$census.gender.parity, fig2C$GoogleNews.300D.Gender.Norm, paired=T)
t.test(fig2C$Img.Gender.Parity, fig2C$GoogleNews.300D.Gender.Norm, paired=T)

#################################
##Figure 3 Experimental Results##
#################################
exp_dt<-read.csv(paste(data_path, "data_exp.csv", sep=""))
exp_dt$time_per_source<-exp_dt$time/exp_dt$sources
exp_dt_main<-subset(exp_dt, condition != "Text.Neutral")
exp_dt_main$condition<-as.factor(exp_dt_main$condition)
levels(exp_dt_main$condition)<-c("Control", "Image", "Text")

########
#Fig 3A#
########
exp_dt_simp<-subset(exp_dt_main, condition %in% c("Text", "Image"))
exp_dt_simp_agg<-exp_dt_simp %>% group_by(category, condition) %>% dplyr::summarise(btwn.upload.parity=mean(gender_num, na.rm=T), 
                                                                                    btwn.upload.str=abs(btwn.upload.parity))
exp_dt_simp_agg_full<-merge(exp_dt_simp, exp_dt_simp_agg, by=c("category", "condition"))

fig3ACD<-exp_dt_simp_agg_full %>% group_by(condition, category) %>% 
  dplyr::summarise(
    gender.rate = mean(gender.rate, na.rm=T),
    str_stereo = mean(str_stereo, na.rm=T), 
    stim.parity = mean(gender_num, na.rm=T), 
    btwn.upload.parity=mean(btwn.upload.parity, na.rm=T), 
    btwn.upload.str = mean(btwn.upload.str, na.rm=T),
    stim.str = abs(stim.parity)
  )

ggplot(fig3ACD, aes(x = stim.str, fill=condition, alpha=condition, linetype=condition)) +
  geom_density(size=1.4) + theme_bw() + scale_alpha_manual(values=c(0.5,0.5)) + 
  scale_linetype_manual(values=c("solid", "solid")) + 
  scale_fill_manual(values=c("purple","forestgreen")) + 
  ylab("Density") + xlab("Average Strength of Gender\nAssociation in Participants' Uploads") + 
  theme(legend.text=element_text(size=50),
        legend.position=c(0.8,0.85),
        legend.title = element_blank(),
        plot.title=element_blank(),
        axis.title.x=element_text(size = 36, hjust = 0.5),
        axis.title.y=element_text(size = 36, hjust = 0.5),
        axis.text.x=element_text(size = 36, hjust = 0.5),
        axis.text.y=element_text(size = 36, hjust = 0.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits = c(-0.28, 1.26), breaks=c(0,0.25,0.5,0.75,1), labels=c("0", "0.25", "0.5", "0.75", "1")) + 
  geom_vline(xintercept = mean(subset(fig3ACD, condition=="Image")$stim.str), color="purple", size=2) + 
  geom_vline(xintercept = mean(subset(fig3ACD, condition=="Text")$stim.str), color="forestgreen", size=2)

#ggsave('Fig3A.png', width=10, height=10, path = savepath_main)

########
#Fig 3B#
########
fig3B<-exp_dt_main %>% group_by(category,condition) %>% dplyr::summarise(str_stereo=mean(str_stereo,na.rm=T), gender.rate = mean(gender.rate, na.rm=T)) 

ggplot(fig3B, aes(x = str_stereo, fill=condition, alpha=condition, linetype=condition)) +
  geom_density(size=1.4) + theme_bw() + 
  scale_alpha_manual(values=c(0,0.5,0.5,0)) + 
  scale_linetype_manual(values=c("dotted", "solid", "solid")) + 
  scale_fill_manual(values=c("black", "purple", "forestgreen")) + 
  ylab("Density") + xlab("Avg. Strength of Gender Association\nin Participants' Explicit Ratings") + 
  theme(legend.text=element_text(size=50),
        legend.position=c(0.78,0.85),
        legend.title = element_blank(),
        plot.title=element_blank(),
        axis.title.x=element_text(size = 36, hjust = 0.5),
        axis.title.y=element_text(size = 36, hjust = 0.5),
        axis.text.x=element_text(size = 36, hjust = 0.5),
        axis.text.y=element_text(size = 36, hjust = 0.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits = c(-0.05, 0.96), breaks=c(0,0.25,0.5,0.75,1), 
                     labels=c("0", "0.25", "0.5", "0.75", "1")) + 
  geom_vline(xintercept = mean(subset(fig3B, condition=="Image")$str_stereo), color="purple", size=2) + 
  geom_vline(xintercept = mean(subset(fig3B, condition=="Text")$str_stereo), color="forestgreen", size=2) + 
  geom_vline(xintercept = mean(subset(fig3B, condition=="Control")$str_stereo), color="black", size=2, linetype="dotted")

#ggsave('Fig3B.png', width=10, height=10, path = savepath_main)
pairwise.wilcox.test(fig3B$str_stereo, fig3B$condition, p.adjust.method = "none", paired=T)

#run equivalence test
wilcox_TOST(subset(fig3B, condition=="Image")$str_stereo, subset(fig3B, condition=="Text")$str_stereo, paired=F, eqb = 0.11)

########
#Fig 3C#
########
ggplot(fig3ACD, aes(x=stim.parity, y=gender.rate, shape=condition, color=condition, group=1))+
  geom_point(size=8) + 
  geom_smooth(size=6, method='lm', formula= y~x, color="black") + 
  scale_color_manual(values=c("purple", "forestgreen")) + 
  xlab("Average Gender Association in\nParticipants' Uploads") + 
  ylab("Average Gender Association in\nParticipants' Explicit Responses") + theme_bw() + 
  theme(axis.text.x = element_text(size=36), 
        axis.text.y = element_text(size=36),
        axis.title.x = element_text(size=36),
        axis.title.y = element_text(size=36),
        legend.position = c(0.83,0.1), 
        legend.text=element_text(size=40),
        legend.title=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        legend.box.background = element_rect(colour = "black"),
        legend.background = element_blank()) + 
  geom_vline(xintercept = 0, linetype="dotted", size=2) + 
  geom_hline(yintercept = 0, linetype="dotted", size=2) + 
  scale_x_continuous(limits = c(-1.02, 1.02), breaks=c(-1,-0.5,0,0.5,1), 
                     labels=c("-1","-0.5","0","0.5","1"))

#ggsave('Fig3C.png', width=10, height=10, path = savepath_main)

cor.test(fig3ACD$gender.rate, fig3ACD$stim.parity)
cor.test(subset(fig3ACD, condition=="Image")$gender.rate, 
         subset(fig3ACD, condition=="Image")$stim.parity)
cor.test(subset(fig3ACD, condition!="Image")$gender.rate, 
         subset(fig3ACD, condition!="Image")$stim.parity)

########
#Fig 3D#
########
ggplot(fig3ACD, aes(x=stim.str, y=str_stereo, shape=condition, color=condition, group=1))+
  geom_point(size=8) + 
  geom_smooth(size=6, method='lm', formula= y~x, color="black") + 
  scale_color_manual(values=c("purple", "forestgreen")) + 
  xlab("Average Strength of Gender \n Association in Participants' Uploads") + 
  ylab("Avg. Strength of Gender Association \n in Participants' Explicit Responses") + theme_bw() + 
  theme(axis.text.x = element_text(size=36), 
        axis.text.y = element_text(size=36),
        axis.title.x = element_text(size=36),
        axis.title.y = element_text(size=36),
        legend.position = c(0.83,0.1), 
        legend.text=element_text(size=40),
        legend.title=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        legend.box.background = element_rect(colour = "black"),
        legend.background = element_blank()) + 
  scale_x_continuous(limits = c(-0.05, 0.96), breaks=c(0,0.25,0.5,0.75,1), 
                     labels=c("0", "0.25", "0.5", "0.75", "1")) + 
  scale_y_continuous(limits = c(0.1, 0.72), breaks=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7), 
                     labels=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7))

#ggsave('Fig3D.png', width=10, height=10, path = savepath_main)

cor.test(fig3ACD$stim.str, fig3ACD$str_stereo)
cor.test(subset(fig3ACD, condition=="Image")$str_stereo, 
         subset(fig3ACD, condition=="Image")$stim.str)
cor.test(subset(fig3ACD, condition!="Image")$stim.str, 
         subset(fig3ACD, condition!="Image")$str_stereo)

#Experimental Control Models
fig3ACD$condition<-as.factor(as.character(fig3ACD$condition))
fig3ACD <- within(fig3ACD, condition <- relevel(condition, ref = 2))

#Fig.3C controls
mod_3C_cntrl_simp<-lm(gender.rate ~ condition, data=fig3ACD)
summary(mod_3C_cntrl_simp)
mod_3C_cntrl_simp_vcov <- cluster.vcov(mod_3C_cntrl_simp, fig3ACD$category)
coeftest(mod_3C_cntrl_simp, mod_3C_cntrl_simp_vcov)

mod_3C_cntrl<-lm(gender.rate ~ stim.parity + condition, data=fig3ACD)
summary(mod_3C_cntrl)
mod_3C_cntrl_vcov <- cluster.vcov(mod_3C_cntrl, fig3ACD$category)
coeftest(mod_3C_cntrl, mod_3C_cntrl_vcov)

mod_3C_cntrl_int<-lm(gender.rate ~ stim.parity * condition, data=fig3ACD)
summary(mod_3C_cntrl_int)
mod_3C_cntrl_int_vcov <- cluster.vcov(mod_3C_cntrl_int, fig3ACD$category)
coeftest(mod_3C_cntrl_int, mod_3C_cntrl_int_vcov)

#Fig.3D controls
mod_3D_cntrl_simp<-lm(str_stereo ~ condition, data=fig3ACD)
summary(mod_3D_cntrl_simp)
mod_3D_cntrl_simp_vcov <- cluster.vcov(mod_3D_cntrl_simp, fig3ACD$category)
coeftest(mod_3D_cntrl_simp, mod_3D_cntrl_simp_vcov)

mod_3D_cntrl<-lm(str_stereo ~ stim.str + condition, data=fig3ACD)
summary(mod_3D_cntrl)
mod_3D_cntrl_vcov <- cluster.vcov(mod_3D_cntrl, fig3ACD$category)
coeftest(mod_3D_cntrl, mod_3D_cntrl_vcov)

mod_3D_cntrl_int<-lm(str_stereo ~ stim.str * condition, data=fig3ACD)
summary(mod_3D_cntrl_int)
mod_3D_cntrl_int_vcov <- cluster.vcov(mod_3D_cntrl_int, fig3ACD$category)
coeftest(mod_3D_cntrl_int, mod_3D_cntrl_int_vcov)

########
#Fig 3E#
########
fig3E<-exp_dt_main %>% group_by(ProlificID, condition) %>% dplyr::summarise(str_stereo=mean(str_stereo), dscore=mean(dscore, na.rm=T),str.dscore=abs(dscore)) 

ggplot(fig3E, aes(x = dscore, fill=condition, alpha=condition, linetype=condition)) +
  geom_density(size=1.4) + theme_bw() + 
  scale_alpha_manual(values=c(0,0.5,0.5,0)) + 
  scale_linetype_manual(values=c("dotted", "solid", "solid")) + 
  scale_fill_manual(values=c("black", "purple", "forestgreen")) + 
  ylab("Density") + xlab("Participants' Implicit Gender Bias \n (D Score)") + 
  theme(legend.text=element_text(size=50),
        legend.position=c(0.2,0.85),
        legend.title = element_blank(),
        plot.title=element_blank(),
        axis.title.x=element_text(size = 36, hjust = 0.5),
        axis.title.y=element_text(size = 36, hjust = 0.5),
        axis.text.x=element_text(size = 36, hjust = 0.5),
        axis.text.y=element_text(size = 36, hjust = 0.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits = c(-0.7, 1.55), breaks=c(-0.4, 0, 0.4, 0.8, 1.2)) + 
  geom_vline(xintercept = mean(subset(fig3C, condition=="Image")$dscore, na.rm=T), color="purple", size=2) + 
  geom_vline(xintercept = mean(subset(fig3C, condition=="Text")$dscore, na.rm=T), color="forestgreen", size=2) + 
  geom_vline(xintercept = mean(subset(fig3C, condition=="Control")$dscore, na.rm=T), color="black", size=2, linetype="dotted") + 
  scale_y_continuous(breaks=c(0,1))

#ggsave('fig3E.png', width=10, height=10, path = savepath_main)
pairwise.wilcox.test(fig3E$dscore, fig3E$condition, p.adjust.method = "none")
pairwise.t.test(fig3E$dscore, fig3E$condition, p.adjust.method = "none")

#run equivalence tests
wilcox_TOST(subset(fig3E, condition=="Control")$dscore, subset(fig3E, condition=="Text")$dscore, paired=F, eqb = 0.13)
wilcox_TOST(subset(fig3E, condition=="Image")$dscore, subset(fig3E, condition=="Text")$dscore, paired=F, eqb = 0.14)

wilcox.test(subset(exp_dt_main, condition=="Control")$dscore)
wilcox.test(subset(exp_dt_main, condition=="Image")$dscore)
wilcox.test(subset(exp_dt_main, condition=="Text")$dscore)

########
#Fig 3F#
########
exp_dt_main$str_stereo_BIN<-ntile(exp_dt_main$str_stereo, 4)

fig3F<-exp_dt_main %>% group_by(str_stereo_BIN) %>% 
  dplyr::summarise(str_stereo = mean(str_stereo,na.rm=T), 
                   cilow=t.test(dscore, conf.level=0.95)$conf.int[1], 
                   cihi=t.test(dscore, conf.level=0.95)$conf.int[2],
                   dscore = mean(dscore, na.rm=T))

ggplot(fig3F, aes(x=str_stereo_BIN, y=dscore))+
  geom_point(size=18, color="black") + geom_line(size=3) + 
  geom_errorbar(aes(ymin=cilow, ymax=cihi), linetype="solid",width=0.2, size=3)+
  xlab("Strength of Participants' Explicit \n Gender Associations (Quartiles)") + 
  ylab("Average Implicit Gender Bias \n (D Score)") + theme_bw() + 
  theme(plot.title = element_text(size=60, hjust=0.5), 
        axis.text.x = element_text(size=60),
        axis.text.y = element_text(size=60),
        axis.title.x = element_text(size=60),
        axis.title.y = element_text(size=60),
        legend.position = "none", legend.text=element_text(size=30),
        legend.title=element_text(size=30, face="bold"),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  scale_y_continuous(limits=c(0.34, 0.46), breaks=c(0.35, 0.4, 0.45))

#ggsave('fig3F.png', width=16, height=16, path = savepath_main)

jonckheere.test(exp_dt_main$dscore, exp_dt_main$str_stereo_BIN)
jonckheere.test(exp_dt_main$dscore, exp_dt_main$str_stereo)

#Replicating with linear regression
exp_dt_main$ProlificID<-as.factor(exp_dt_main$ProlificID)
fig3F_mod<-lm(dscore ~ str_stereo + condition + category, data = exp_dt_main)
summary(fig3F_mod)
fig3F_mod_vcov <- cluster.vcov(fig3F_mod, exp_dt_main$ProlificID)
coeftest(fig3F_mod, fig3F_mod_vcov)

############################
#Experimental Supplementary#
############################
savepath_supp<-"" #where you want the supplementary figures to be saved

#############
#Fig 3B Supp#
#############
fig3B_supp<-exp_dt %>% group_by(category,condition) %>% dplyr::summarise(str_stereo=mean(str_stereo)) 

ggplot(fig3B_supp, aes(x = str_stereo, fill=condition, alpha=condition, color=condition,linetype=condition)) +
  geom_density(size=1.7) + theme_bw() + 
  scale_alpha_manual(values=c(0,0.5,0.5,0)) + 
  scale_linetype_manual(values=c("dotted", "solid", "solid", "dotted")) + 
  scale_fill_manual(values=c("black", "purple", "forestgreen","forestgreen")) + 
  scale_color_manual(values=c("black", "black", "black","forestgreen")) + 
  ylab("Density") + xlab("Strength of Participants'\nGender Associations") + 
  theme(legend.text=element_text(size=40),
        legend.position=c(0.78,0.83),
        legend.title = element_blank(),
        plot.title=element_blank(),
        axis.title.x=element_text(size = 40, hjust = 0.5),
        axis.title.y=element_text(size = 40, hjust = 0.5),
        axis.text.x=element_text(size = 40, hjust = 0.5),
        axis.text.y=element_text(size = 40, hjust = 0.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits = c(-0.05, 0.96), breaks=c(0,0.25,0.5,0.75,1), 
                     labels=c("0", "0.25", "0.5", "0.75", "1")) 

#ggsave('Fig3B_supp.png', width=10, height=10, path = savepath_supp)

#############
#Fig 3E Supp#
#############
fig3E_supp<-exp_dt %>% group_by(ProlificID,condition) %>% dplyr::summarise(str_stereo=mean(str_stereo), dscore=mean(dscore, na.rm=T),str.dscore=abs(dscore)) 

ggplot(fig3E_supp, aes(x = str.dscore, fill=condition, alpha=condition, color=condition,linetype=condition)) +
  geom_density(size=1.7) + theme_bw() + 
  scale_alpha_manual(values=c(0,0.5,0.5,0)) + 
  scale_linetype_manual(values=c("dotted", "solid", "solid", "dotted")) + 
  scale_fill_manual(values=c("black", "purple", "forestgreen","forestgreen")) + 
  scale_color_manual(values=c("black", "black", "black","forestgreen")) + 
  ylab("Density") + xlab("Strength of Participants'\nImplicit Bias (dscore)") + 
  theme(legend.text=element_text(size=40),
        legend.position=c(0.78,0.8),
        legend.title = element_blank(),
        plot.title=element_blank(),
        axis.title.x=element_text(size = 40, hjust = 0.5),
        axis.title.y=element_text(size = 40, hjust = 0.5),
        axis.text.x=element_text(size = 40, hjust = 0.5),
        axis.text.y=element_text(size = 40, hjust = 0.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits = c(-0.3, 1.7)) 

#ggsave('fig3E_supp.png', width=10, height=10, path = savepath_supp)

pairwise.wilcox.test(fig3E_supp$dscore, fig3E_supp$condition, p.adjust.method = "none")
pairwise.wilcox.test(fig3E_supp$str.dscore, fig3E_supp$condition, p.adjust.method = "none")
pairwise.t.test(fig3E_supp$str.dscore, fig3E_supp$condition, p.adjust.method = "none")

###################################################
#Comparing strength of priming for text and images#
###################################################
exp_dt_gendered<-subset(exp_dt, gender_num %in% c(-1, 1))

ggplot(exp_dt_gendered, aes(x = str_stereo, fill=condition, alpha=condition, linetype=condition)) +
  geom_density(size=1.4) + theme_bw() + scale_alpha_manual(values=c(0.5,0.5)) + 
  scale_linetype_manual(values=c("solid", "solid")) + 
  scale_fill_manual(values=c("purple","forestgreen")) + 
  ylab("Density") + xlab("Strength of Participants' Gender Association") + 
  ggtitle("Participants' Responses to\n Gendered Descriptions") + 
  theme(legend.text=element_text(size=50),
        legend.position=c(0.8,0.85),
        legend.title = element_blank(),
        plot.title=element_text(size = 40, hjust = 0.5),
        axis.title.x=element_text(size = 40, hjust = 0.5),
        axis.title.y=element_text(size = 40, hjust = 0.5),
        axis.text.x=element_text(size = 40, hjust = 0.5),
        axis.text.y=element_text(size = 40, hjust = 0.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits = c(-0.28, 1.26), breaks=c(0,0.25,0.5,0.75,1), labels=c("0", "0.25", "0.5", "0.75", "1")) + 
  geom_vline(xintercept = mean(subset(exp_dt_gendered, condition=="Image")$str_stereo), color="purple", size=2) + 
  geom_vline(xintercept = mean(subset(exp_dt_gendered, condition=="Text")$str_stereo), color="forestgreen", size=2)

#ggsave('img_stronger_prime.png', width=14, height=14, path = savepath_supp)

exp_dt_gendered %>% group_by(condition) %>% dplyr::summarise(numdescriptions=length(str_stereo),str_stereo=mean(str_stereo))
t.test(subset(exp_dt_gendered, condition=="Image")$str_stereo,subset(exp_dt_gendered, condition=="Text")$str_stereo)
exp_dt_gendered$condition<-as.factor(exp_dt_gendered$condition)
exp_dt_gendered <- within(exp_dt_gendered, condition <- relevel(condition, ref = 2))
mod_prime_str<-lm(str_stereo ~ condition + category + as.factor(gender_num), data=exp_dt_gendered)
summary(mod_prime_str)

#######################################
#Observational corr. with experimental#
#######################################
comparison_data_main_Google$category<-comparison_data_main_Google$Social.Category
comparison_data_main_Google$category<-gsub(" ", "", comparison_data_main_Google$category)
exp_obs_m<-merge(fig3ACD, comparison_data_main_Google, by=c("category"))

cor.test(subset(exp_obs_m, condition=="Image")$Img.Gender.Parity, subset(exp_obs_m, condition=="Image")$btwn.upload.parity)
cor.test(subset(exp_obs_m, condition=="Image")$Img.Gender.Parity, subset(exp_obs_m, condition=="Image")$gender.rate)
cor.test(subset(exp_obs_m, condition=="Image")$gender.rate, subset(exp_obs_m, condition=="Image")$btwn.upload.parity)
cor.test(subset(exp_obs_m, condition=="Text")$GoogleNews.300D.Gender.Norm, subset(exp_obs_m, condition=="Text")$btwn.upload.parity)
cor.test(subset(exp_obs_m, condition=="Text")$GoogleNews.300D.Gender.Norm, subset(exp_obs_m, condition=="Text")$gender.rate)

ggplot(subset(exp_obs_m, condition=="Image"), 
       aes(x=Img.Gender.Parity, y=btwn.upload.parity, group=1))+
  geom_point(size=8) + theme_bw() + 
  geom_smooth(size=6, method='lm', formula= y~x, color="blue") + 
  xlab("Gender Associations in Google Images\n(Observational)") + 
  ylab("Gender Associations in Google Images\n(Participants' Experiment Uploads)") + 
  theme(axis.text.x = element_text(size=40), 
        axis.text.y = element_text(size=40),
        axis.title.x = element_text(size=40),
        axis.title.y = element_text(size=40),
        legend.position = c(0.83,0.1), 
        legend.text=element_text(size=40),
        legend.title=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        legend.box.background = element_rect(colour = "black"),
        legend.background = element_blank()) + 
  geom_vline(xintercept = 0, linetype="dotted", size=2) + 
  geom_hline(yintercept = 0, linetype="dotted", size=2) + 
  coord_cartesian(xlim=c(-0.9, 0.9), ylim=c(-1,1))

#ggsave('pA_obsv_exp_corr.png', width=14, height=14, path = savepath_supp)

ggplot(subset(exp_obs_m, condition=="Image"), 
       aes(x=Img.Gender.Parity, y=gender.rate, group=1))+
  geom_point(size=8) + theme_bw() + 
  geom_smooth(size=6, method='lm', formula= y~x, color="blue") + 
  xlab("Gender Associations in Google Images\n(Observational)") + 
  ylab("Gender Associations in Participants' Responses\n(Experimental)") + 
  theme(axis.text.x = element_text(size=40), 
        axis.text.y = element_text(size=40),
        axis.title.x = element_text(size=40),
        axis.title.y = element_text(size=40),
        legend.position = c(0.83,0.1), 
        legend.text=element_text(size=40),
        legend.title=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        legend.box.background = element_rect(colour = "black"),
        legend.background = element_blank()) + 
  geom_vline(xintercept = 0, linetype="dotted", size=2) + 
  geom_hline(yintercept = 0, linetype="dotted", size=2) + 
  coord_cartesian(xlim=c(-0.9, 0.9), ylim=c(-1, 1))

#ggsave('pB_obsv_exp_corr.png', width=14, height=14, path = savepath_supp)

ggplot(subset(exp_obs_m, condition=="Text"), 
       aes(x=GoogleNews.300D.Gender.Norm, y=btwn.upload.parity, group=1))+
  geom_point(size=8) + theme_bw() + 
  geom_smooth(size=6, method='lm', formula= y~x, color="blue") + 
  xlab("Gender Associations in Google News\n(Observational)") + 
  ylab("Gender Associations in Google News\n(Participants' Experiment Uploads)") + 
  theme(axis.text.x = element_text(size=40), 
        axis.text.y = element_text(size=40),
        axis.title.x = element_text(size=40),
        axis.title.y = element_text(size=40),
        legend.position = c(0.83,0.1), 
        legend.text=element_text(size=40),
        legend.title=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        legend.box.background = element_rect(colour = "black"),
        legend.background = element_blank()) + 
  geom_vline(xintercept = 0, linetype="dotted", size=2) + 
  geom_hline(yintercept = 0, linetype="dotted", size=2) + 
  coord_cartesian(xlim=c(-0.65, 0.65), ylim=c(-0.5,0.5)) + 
  scale_y_continuous(breaks=c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6))

#ggsave('pC_obsv_exp_corr.png', width=14, height=14, path = savepath_supp)

ggplot(subset(exp_obs_m, condition=="Text"), 
       aes(x=GoogleNews.300D.Gender.Norm, y=gender.rate, group=1))+
  geom_point(size=8) + theme_bw() + 
  geom_smooth(size=6, method='lm', formula= y~x, color="blue") + 
  xlab("Gender Associations in Google News\n(Observational)") + 
  ylab("Gender Associations in Participants' Responses\n(Experimental)") + 
  theme(axis.text.x = element_text(size=40), 
        axis.text.y = element_text(size=40),
        axis.title.x = element_text(size=40),
        axis.title.y = element_text(size=40),
        legend.position = c(0.83,0.1), 
        legend.text=element_text(size=40),
        legend.title=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        legend.box.background = element_rect(colour = "black"),
        legend.background = element_blank()) + 
  geom_vline(xintercept = 0, linetype="dotted", size=2) + 
  geom_hline(yintercept = 0, linetype="dotted", size=2) + 
  coord_cartesian(xlim=c(-0.65, 0.65), ylim=c(-0.6,0.6)) + 
  scale_y_continuous(breaks=c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6))

#ggsave('pD_obsv_exp_corr.png', width=14, height=14, path = savepath_supp)

##############################################################
#Main Experimental Results Controlling for Participant Gender#
##############################################################

#Table (Gender of upload controlling for gender of participant)
exp_dt_txt<-subset(exp_dt_main, condition %in% c("Text"))
exp_dt_txt_simp<-exp_dt_txt %>% dplyr::select(ProlificID, category, condition, gender_num, Subj.Sex)
colnames(exp_dt_txt_simp)[4]<-"gender_upload"

exp_dt_img<-subset(exp_dt_main, condition %in% c("Image"))
exp_dt_img_simp<-exp_dt_img %>% dplyr::select(ProlificID, category, condition, gender_num, Subj.Sex)
colnames(exp_dt_img_simp)[4]<-"gender_upload"

exp_dt_txt_v_img<-rbind(exp_dt_txt_simp, exp_dt_img_simp)
exp_dt_txt_v_img$ProlificID<-as.factor(exp_dt_txt_v_img$ProlificID)

mod_gender<-lm(gender_upload ~ condition + Subj.Sex + category, data = exp_dt_txt_v_img)
summary(mod_gender)
mod_gender_vcov <- cluster.vcov(mod_gender, exp_dt_txt_v_img$ProlificID)
coeftest(mod_gender, mod_gender_vcov)

#Table (Strength of Gender Associations in Subjects' Responses Controlling for Subject Gender/Gender Match w. Upload)
mod1_s11<-lm(str_stereo ~ condition + category, data = exp_dt_main)
summary(mod1_s11)
mod1_s11_vcov <- cluster.vcov(mod1_s11, exp_dt_main$ProlificID)
coeftest(mod1_s11, mod1_s11_vcov)

mod2_s11<-lm(str_stereo ~ condition + Subj.Sex + category, data = exp_dt_main)
summary(mod2_s11)
mod2_s11_vcov <- cluster.vcov(mod2_s11, exp_dt_main$ProlificID)
coeftest(mod2_s11, mod2_s11_vcov)

mod3_s11<-lm(str_stereo ~ condition + Subj.Sex + same_gender + category, data = exp_dt_main)
summary(mod3_s11)
mod3_s11_vcov <- cluster.vcov(mod3_s11, exp_dt_main$ProlificID)
coeftest(mod3_s11, mod3_s11_vcov)

########################################################################
##Priming Analysis Controlling for Gender Bias in Observational Sample##
########################################################################
comparison_data_main_Google_IMG<-comparison_data_main_Google[, c("Social.Category", "Img.Gender.Parity", "Img.Gender.Bias.Str")]
comparison_data_main_Google_IMG$condition<-"Image"
colnames(comparison_data_main_Google_IMG)<-c("Social.Category","Obsv.Parity", "Obsv.Stereo.Str", "condition")
comparison_data_main_Google_TXT<-comparison_data_main_Google[, c("Social.Category", "GoogleNews.300D.Gender.Norm")]
comparison_data_main_Google_TXT$Obsv.Stereo.Str<-abs(comparison_data_main_Google_TXT$GoogleNews.300D.Gender.Norm)
comparison_data_main_Google_TXT$condition<-"Text"
colnames(comparison_data_main_Google_TXT)<-c("Social.Category","Obsv.Parity", "Obsv.Stereo.Str", "condition")
obsv_full<-rbind(comparison_data_main_Google_IMG, comparison_data_main_Google_TXT)

exp_dt_main$Social.Category<-exp_dt_main$category
exp_dt_main_prime_ext<-merge(exp_dt_main, obsv_full, by=c("Social.Category", "condition"))
exp_dt_main_prime_ext$condition<-as.factor(exp_dt_main_prime_ext$condition)
exp_dt_main_prime_ext$condition<-relevel(exp_dt_main_prime_ext$condition, ref = "Text")

mod_prime<-lm(str_stereo ~ condition + Obsv.Stereo.Str + Social.Category, data = exp_dt_main_prime_ext)
summary(mod_prime)
mod_prime_vcov <- cluster.vcov(mod_prime, exp_dt_main_prime_ext$ProlificID)
coeftest(mod_prime, mod_prime_vcov)

##############################
#Controlling for time/sources#
##############################
exp_dt_img_vs_text<-subset(exp_dt, condition %in% c("Image", "Text"))
t.test(subset(exp_dt_img_vs_text, condition=="Text")$time, subset(exp_dt_img_vs_text, condition=="Image")$time)
t.test(subset(exp_dt_img_vs_text, condition=="Text")$time_per_source, subset(exp_dt_img_vs_text, condition=="Image")$time_per_source)
t.test(subset(exp_dt_img_vs_text, condition=="Text")$sources, subset(exp_dt_img_vs_text, condition=="Image")$sources)

exp_dt_img_vs_text %>% group_by(condition) %>% dplyr::summarise(mintime=min(time, na.rm=T),
                                                                maxtime=max(time, na.rm=T),
                                                                time=mean(time, na.rm=T), 
                                                                minsources=min(sources, na.rm=T),
                                                                maxsources=max(sources, na.rm=T),
                                                                sources=mean(sources, na.rm=T),
                                                                min_tpersource=min(time_per_source, na.rm=T),
                                                                max_tpersource=max(time_per_source, na.rm=T),
                                                                time_p_source = mean(time_per_source, na.rm=T))

ggplot(exp_dt_img_vs_text, aes(x = time, fill=condition, alpha=condition, linetype=condition)) +
  geom_density(size=1.4) + theme_bw() + scale_alpha_manual(values=c(0.5,0.5)) + 
  scale_linetype_manual(values=c("solid", "solid")) + 
  scale_fill_manual(values=c("purple","forestgreen")) + 
  ylab("Density") + xlab("Seconds per Occupation") + 
  theme(legend.text=element_text(size=50),
        legend.position=c(0.8,0.85),
        legend.title = element_blank(),
        plot.title=element_text(size = 40, hjust = 0.5),
        axis.title.x=element_text(size = 40, hjust = 0.5),
        axis.title.y=element_text(size = 40, hjust = 0.5),
        axis.text.x=element_text(size = 40, hjust = 0.5),
        axis.text.y=element_text(size = 40, hjust = 0.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits = c(-20, 800)) + 
  scale_y_continuous(breaks= c(0,0.01), labels = c("0.00","0.01"))

#ggsave('exp_time.png', width=13, height=13, path = savepath_supp)

ggplot(exp_dt_img_vs_text, aes(x = sources, fill=condition, alpha=condition, linetype=condition)) +
  geom_density(size=1.4) + theme_bw() + scale_alpha_manual(values=c(0.5,0.5)) + 
  scale_linetype_manual(values=c("solid", "solid")) + 
  scale_fill_manual(values=c("purple","forestgreen")) + 
  ylab("Density") + xlab("Sources per Occupation") + 
  theme(legend.text=element_text(size=50),
        legend.position=c(0.8,0.85),
        legend.title = element_blank(),
        plot.title=element_text(size = 40, hjust = 0.5),
        axis.title.x=element_text(size = 40, hjust = 0.5),
        axis.title.y=element_text(size = 40, hjust = 0.5),
        axis.text.x=element_text(size = 40, hjust = 0.5),
        axis.text.y=element_text(size = 40, hjust = 0.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits = c(-3, 50)) + 
  scale_y_continuous(breaks= c(0,0.1, 0.2, 0.3), labels = c("0.00","0.10", "0.20", "0.30"))

#ggsave('sources_per_occupation.png', width=13, height=13, path = savepath_supp)

ggplot(exp_dt_img_vs_text, aes(x = time_per_source, fill=condition, alpha=condition, linetype=condition)) +
  geom_density(size=1.4) + theme_bw() + scale_alpha_manual(values=c(0.5,0.5)) + 
  scale_linetype_manual(values=c("solid", "solid")) + 
  scale_fill_manual(values=c("purple","forestgreen")) + 
  ylab("Density") + xlab("Seconds per Source") + 
  theme(legend.text=element_text(size=50),
        legend.position=c(0.8,0.85),
        legend.title = element_blank(),
        plot.title=element_text(size = 40, hjust = 0.5),
        axis.title.x=element_text(size = 40, hjust = 0.5),
        axis.title.y=element_text(size = 40, hjust = 0.5),
        axis.text.x=element_text(size = 40, hjust = 0.5),
        axis.text.y=element_text(size = 40, hjust = 0.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits = c(-10, 250)) 

#ggsave('time_per_source.png', width=13, height=13, path = savepath_supp)

exp_dt_img_vs_text$condition<-as.factor(exp_dt_img_vs_text$condition)
exp_dt_img_vs_text <- within(exp_dt_img_vs_text, condition <- relevel(condition, ref = 2))

exp_cntrls<-lm(str_stereo ~ condition + time + sources + time_per_source + category, data=exp_dt_img_vs_text)
summary(exp_cntrls)
confint(exp_cntrls)
exp_cntrls_vcov <- cluster.vcov(exp_cntrls, exp_dt_img_vs_text$ProlificID)
coeftest(exp_cntrls, exp_cntrls_vcov)

###############
#IAT Over time#
###############
cor.test(exp_dt_main$dscore, exp_dt_main$dscore_day3)

exp_dt_main_long<-rbind(
  data.frame(ProlificID=exp_dt_main$ProlificID, condition=exp_dt_main$condition, category=exp_dt_main$category, dscore=exp_dt_main$dscore, day=1),
  data.frame(ProlificID=exp_dt_main$ProlificID, condition=exp_dt_main$condition, category=exp_dt_main$category, dscore=exp_dt_main$dscore_day3, day=3)
)

exp_dt_main_long<-unique(exp_dt_main_long %>% dplyr::select(ProlificID, condition, dscore, day))

IAT_day_by_cond<-lm(dscore ~ day + condition, data = exp_dt_main_long)
summary(IAT_day_by_cond)
confint(IAT_day_by_cond)

IAT_day_by_cond_vcov <- cluster.vcov(IAT_day_by_cond, exp_dt_main_long$ProlificID)
coeftest(IAT_day_by_cond, IAT_day_by_cond_vcov)

IAT_img<-subset(exp_dt_main, condition=="Image")

IAT_img_subj_bias<-IAT_img %>% group_by(ProlificID) %>% 
  dplyr::summarise(Expbiasrate=mean(gender.rate, na.rm=T), 
                   Str.Expbiasrate=mean(abs(gender.rate), na.rm=T),
                   IMGbias= mean(gender_num, na.rm=T),
                   Strbias=abs(IMGbias), 
                   dscore = mean(dscore),
                   dscore_day3 = mean(dscore_day3))

IAT_img_subj_bias_long <- gather(IAT_img_subj_bias, day, dscore, dscore:dscore_day3, factor_key=TRUE)
IAT_img_subj_bias_long$day<-as.factor(IAT_img_subj_bias_long$day)
levels(IAT_img_subj_bias_long$day)<-c(1,3)
IAT_img_subj_bias_long$day<-as.numeric(as.character(IAT_img_subj_bias_long$day))

IAT_IMG_day<-lm(dscore ~ day + Str.Expbiasrate, data = IAT_img_subj_bias_long)
summary(IAT_IMG_day)
confint(IAT_IMG_day)
IAT_IMG_by_cond_vcov <- cluster.vcov(IAT_IMG_day, IAT_img_subj_bias_long$ProlificID)
coeftest(IAT_IMG_day, IAT_IMG_by_cond_vcov)

#######################
#Supplementary figures# 
#######################

########################################################################################################################
#Fig. 1B: Split by male and female categories according to agreement in gender across text, images, and human judgments#
########################################################################################################################
fem_cat_df<-subset(comparison_data_main_Google,GoogleNews.300D.Gender.Norm<0 & Img.Gender.Parity<0 & Human.Gender.Judge<0)
male_cat_df<-subset(comparison_data_main_Google,GoogleNews.300D.Gender.Norm>0 & Img.Gender.Parity>0 & Human.Gender.Judge>0)

Google.Images.combo.Fem<-subset(comparison_data_main_Google, Social.Category %in% fem_cat_df$Social.Category)
Google.Images.combo.Fem$fig1b_cond<-"Female Cat.\n(by combo)"
Google.Images.combo.Fem$fig1b_measure<-"Images"
Google.Images.combo.Fem$fig1b_gender_assoc<-Google.Images.combo.Fem$Img.Gender.Parity
Google.Images.combo.Fem.simp<-Google.Images.combo.Fem[,c("Social.Category", "searchDEMO","fig1b_cond","fig1b_measure","fig1b_gender_assoc")]

Google.Images.combo.Male<-subset(comparison_data_main_Google, Social.Category %in% male_cat_df$Social.Category)
Google.Images.combo.Male$fig1b_cond<-"Male Cat.\n(by combo)"
Google.Images.combo.Male$fig1b_measure<-"Images"
Google.Images.combo.Male$fig1b_gender_assoc<-Google.Images.combo.Male$Img.Gender.Parity
Google.Images.combo.Male.simp<-Google.Images.combo.Male[,c("Social.Category", "searchDEMO","fig1b_cond","fig1b_measure","fig1b_gender_assoc")]

Google.Text.combo.Fem<-subset(comparison_data_main_Google, Social.Category %in% fem_cat_df$Social.Category)
Google.Text.combo.Fem$fig1b_cond<-"Female Cat.\n(by combo)"
Google.Text.combo.Fem$fig1b_measure<-"Text"
Google.Text.combo.Fem$fig1b_gender_assoc<-Google.Text.combo.Fem$GoogleNews.300D.Gender.Norm
Google.Text.combo.Fem.simp<-Google.Text.combo.Fem[,c("Social.Category", "searchDEMO","fig1b_cond","fig1b_measure","fig1b_gender_assoc")]

Google.Text.combo.Male<-subset(comparison_data_main_Google, Social.Category %in% male_cat_df$Social.Category)
Google.Text.combo.Male$fig1b_cond<-"Male Cat.\n(by combo)"
Google.Text.combo.Male$fig1b_measure<-"Text"
Google.Text.combo.Male$fig1b_gender_assoc<-Google.Text.combo.Male$GoogleNews.300D.Gender.Norm
Google.Text.combo.Male.simp<-Google.Text.combo.Male[,c("Social.Category", "searchDEMO","fig1b_cond","fig1b_measure","fig1b_gender_assoc")]

fig1B_combo<-rbind(Google.Text.combo.Fem.simp, Google.Text.combo.Male.simp, Google.Images.combo.Fem.simp, Google.Images.combo.Male.simp)
fig1B_combo$figb_combo_cond<-paste(fig1B_combo$fig1b_cond, fig1B_combo$fig1b_measure, sep="_")
fig1B_combo_main<-subset(fig1B_combo, searchDEMO=="None")

ggplot(fig1B_combo_main, aes(x = fig1b_gender_assoc, y = figb_combo_cond, fill =  fig1b_measure, group=figb_combo_cond))+
  geom_boxplot(size=2, outlier.size=0, outlier.colour = "white") +  xlab("Gender Association") + theme_bw() + 
  scale_fill_manual(values=c("white", "grey")) + 
  theme(axis.text.x = element_text(size=74), axis.text.y = element_blank(),
        axis.title.y = element_blank(),axis.title.x = element_text(size=75),
        legend.position = "none", legend.text=element_text(size=82),
        legend.title=element_blank(),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + geom_vline(xintercept=0,size=1, linetype="dotted")

#ggsave('Fig1B_bycombo.png', width=14, height=16, path = savepath_supp)

t.test(subset(fig1B_combo_main, fig1b_cond == "Female Cat.\n(by combo)" & fig1b_measure == "Text")$fig1b_gender_assoc, 
       subset(fig1B_combo_main, fig1b_cond == "Female Cat.\n(by combo)" & fig1b_measure == "Images")$fig1b_gender_assoc, paired=T)
t.test(subset(fig1B_combo_main, fig1b_cond == "Male Cat.\n(by combo)" & fig1b_measure == "Text")$fig1b_gender_assoc, 
       subset(fig1B_combo_main, fig1b_cond == "Male Cat.\n(by combo)" & fig1b_measure == "Images")$fig1b_gender_assoc, paired=T)

#######################################################################
#Breaking down Fig 2C by gender skew of occupation according to census#
#######################################################################
fig2C$census.gen.CATEGORY<-ifelse(fig2C$census.gender.parity<0, "Female", "Male")
fig2C_long_cat<-rbind(
  data.frame(Measure="Images", Social.Category = fig2C$Social.Category, Census.Gender= fig2C$census.gen.CATEGORY, Gender.Association=fig2C$Img.Gender.Parity),
  data.frame(Measure="Census", Social.Category = fig2C$Social.Category, Census.Gender= fig2C$census.gen.CATEGORY, Gender.Association=fig2C$census.gender.parity),
  data.frame(Measure="Text", Social.Category = fig2C$Social.Category, Census.Gender= fig2C$census.gen.CATEGORY, Gender.Association=fig2C$GoogleNews.300D.Gender.Norm))

fig2C_long_cat_agg<-fig2C_long_cat %>% group_by(Measure, Census.Gender) %>% 
  dplyr::summarise(cilow=t.test(Gender.Association)$conf.int[1],
                   cihi=t.test(Gender.Association)$conf.int[2],
                   Gender.Association=mean(Gender.Association, na.rm=T))

fig2C_long_cat_agg$Measure<-factor(fig2C_long_cat_agg$Measure, levels=rev(c("Text", "Census", "Images")))
fig2C_long_cat_agg$Measure_cond<-paste(fig2C_long_cat_agg$Measure, fig2C_long_cat_agg$Census.Gender, sep="_")
fig2C_long_cat_agg$Measure_cond<-factor(fig2C_long_cat_agg$Measure_cond, levels=rev(c("Census_Male","Text_Male","Images_Male","Census_Female","Text_Female","Images_Female")))

ggplot(fig2C_long_cat_agg, aes(x = Gender.Association, y = Measure_cond, color =  Measure, group=Measure_cond))+
  geom_point(size=20) +  xlab("Gender Association") + theme_bw() + 
  geom_errorbar(aes(xmin=cilow, xmax=cihi), linetype="solid",width=0.4, size=7)+
  scale_color_manual(values=c("purple","darkgrey","forestgreen"))+
  theme(axis.text.x = element_text(size=74), axis.text.y = element_text(size=15),
        axis.title.y = element_blank(),axis.title.x = element_text(size=75),
        legend.position = "none", legend.text=element_text(size=82),
        legend.title=element_blank(),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  geom_vline(xintercept=0,size=1, linetype="solid") + 
  coord_cartesian(xlim=c(-0.42, 0.42)) + 
  scale_x_continuous(breaks=c(-0.4, -0.2, 0, 0.2, 0.4))

#ggsave('fig2C_extended.png', width=20, height=20, path = "C:/Users/dougl/Desktop/")

t.test(subset(fig2C_long_cat, Measure=="Images" & Census.Gender == "Female")$Gender.Association)
t.test(subset(fig2C_long_cat, Measure=="Text" & Census.Gender == "Female")$Gender.Association)
t.test(subset(fig2C_long_cat, Measure=="Census" & Census.Gender == "Female")$Gender.Association)
t.test(subset(fig2C_long_cat, Measure=="Images" & Census.Gender == "Male")$Gender.Association)
t.test(subset(fig2C_long_cat, Measure=="Text" & Census.Gender == "Male")$Gender.Association)
t.test(subset(fig2C_long_cat, Measure=="Census" & Census.Gender == "Male")$Gender.Association)

#################################
#Results restricting to unigrams#
#################################
comparison_data_main_Google$phrase<-grepl(" ", comparison_data_main_Google$Social.Category)
comparison_data_main_Google_unigram<-subset(comparison_data_main_Google, !phrase)

unigram_fig<-rbind(data.frame(Measure="Text", Gender.Parity = comparison_data_main_Google_unigram$GoogleNews.300D.Gender.Norm, 
                              Social.Category=comparison_data_main_Google_unigram$Social.Category), 
                   data.frame(Measure="Images", Gender.Parity = comparison_data_main_Google_unigram$Img.Gender.Parity, 
                              Social.Category=comparison_data_main_Google_unigram$Social.Category))

ggplot(unigram_fig, aes(x = Gender.Parity, fill=Measure)) +
  geom_density(alpha=0.6, size=1.2) + theme_bw() + 
  scale_fill_manual(values=c("purple", "forestgreen")) + ylab("Source") + xlab("Gender Association") + 
  theme(legend.text=element_text(size=50),
        legend.position=c(0.2,0.9),
        legend.title = element_blank(),
        plot.title=element_blank(),axis.title.y=element_blank(),
        axis.title.x=element_text(size = 50, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.6),
        axis.text.y=element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +   
  scale_x_continuous(breaks=c(-1,-0.5, 0, 0.5, 1), labels =c(-1,-0.5, 0, 0.5, 1), limits = c(-1.4, 1.4)) + 
  geom_vline(xintercept = 0, linetype="dotted", size=2) + 
  geom_vline(xintercept = mean(subset(unigram_fig, Measure=="Text")$Gender.Parity), linetype="solid", size=2, color="forestgreen") + 
  geom_vline(xintercept = mean(subset(unigram_fig, Measure=="Images")$Gender.Parity), linetype="solid", size=2, color="purple") + 
  annotate(geom="text", x=-1, y=0.8, size=15, label="Female",color="Black") + 
  annotate(geom="text", x=1.1, y=0.8, size=15, label="Male",color="Black")

#ggsave('unigram_fig.png', width=10, height=10, path = savepath_supp)

#####################################################
#Results controlling for polesymy and word frequency#
#####################################################
comparison_data_main_Google_long<-rbind(
  data.frame(Measure="Image", 
             Social.Category=comparison_data_main_Google$Social.Category, 
             Gender.Bias=comparison_data_main_Google$Img.Gender.Parity,
             Strength.Bias=comparison_data_main_Google$Img.Gender.Bias.Str,
             Word.Frequency.Scaled = comparison_data_main_Google$Word.Frequency.Scaled,
             Polysemy = comparison_data_main_Google$Polysemy),
  data.frame(Measure="Text", 
             Social.Category=comparison_data_main_Google$Social.Category, 
             Gender.Bias=comparison_data_main_Google$GoogleNews.300D.Gender.Norm,
             Strength.Bias=abs(comparison_data_main_Google$GoogleNews.300D.Gender.Norm),
             Word.Frequency.Scaled = comparison_data_main_Google$Word.Frequency.Scaled,
             Polysemy = comparison_data_main_Google$Polysemy) )

mod_polyfreq_str<-lm(Strength.Bias ~ Measure + Word.Frequency.Scaled + Polysemy, data=comparison_data_main_Google_long)
summary(mod_polyfreq_str)
tab_model(mod_polyfreq_str)
mod_polyfreq_str_vcov <- cluster.vcov(mod_polyfreq_str, comparison_data_main_Google_long$Social.Category)
coeftest(mod_polyfreq_str, mod_polyfreq_str_vcov)

######################################################################
#results while restricting only to non-explicitly gendered categories#
######################################################################
comparison_data_main_Google_nongendered<-subset(comparison_data_main_Google, Gendered.Category==0)

nongendered_fig<-rbind(data.frame(Measure="Text", 
                              Gender.Parity = comparison_data_main_Google_nongendered$GoogleNews.300D.Gender.Norm, 
                              Social.Category=comparison_data_main_Google_nongendered$Social.Category), 
                   data.frame(Measure="Images", Gender.Parity = comparison_data_main_Google_nongendered$Img.Gender.Parity, 
                              Social.Category=comparison_data_main_Google_nongendered$Social.Category))

length(unique(nongendered_fig$Social.Category))

ggplot(nongendered_fig, aes(x = Gender.Parity, fill=Measure)) +
  geom_density(alpha=0.6, size=1.2) + theme_bw() + 
  scale_fill_manual(values=c("purple", "forestgreen")) + ylab("Source") + xlab("Gender Association") + 
  theme(legend.text=element_text(size=50),
        legend.position=c(0.2,0.9),
        legend.title = element_blank(),
        plot.title=element_blank(),axis.title.y=element_blank(),
        axis.title.x=element_text(size = 50, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.6),
        axis.text.y=element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +   
  scale_x_continuous(breaks=c(-1,-0.5, 0, 0.5, 1), labels =c(-1,-0.5, 0, 0.5, 1), limits = c(-1.4, 1.4)) + 
  geom_vline(xintercept = 0, linetype="dotted", size=2) + 
  geom_vline(xintercept = mean(subset(nongendered_fig, Measure=="Text")$Gender.Parity, na.rm=T), linetype="solid", size=2, color="forestgreen") + 
  geom_vline(xintercept = mean(subset(nongendered_fig, Measure=="Images")$Gender.Parity, na.rm=T), linetype="solid", size=2, color="purple") + 
  annotate(geom="text", x=-1, y=0.8, size=15, label="Female",color="Black") + 
  annotate(geom="text", x=1.1, y=0.8, size=15, label="Male",color="Black")

#ggsave('nongendered_fig.png', width=10, height=10, path = savepath_supp)

#####################
#Census centered fig#
#####################
comparison_data_main_Google_census<-subset(comparison_data_main_Google, !is.na(cps_occupation))
comparison_data_main_Google_census<-subset(comparison_data_main_Google_census, !is.na(census.women))
comparison_data_main_Google_census<-subset(comparison_data_main_Google_census, !is.na(Img.Gender.Parity))
comparison_data_main_Google_census<-subset(comparison_data_main_Google_census, !is.na(GoogleNews.300D.Gender.Norm))

comparison_data_main_Google_census$census.men<-1-comparison_data_main_Google_census$census.women
comparison_data_main_Google_census$census.gender.parity<-comparison_data_main_Google_census$census.men - comparison_data_main_Google_census$census.women

comparison_data_main_Google_census$img_census_cent<-comparison_data_main_Google_census$Img.Gender.Parity - comparison_data_main_Google_census$census.gender.parity
comparison_data_main_Google_census$text_census_cent<-comparison_data_main_Google_census$GoogleNews.300D.Gender.Norm - comparison_data_main_Google_census$census.gender.parity

comparison_data_main_Google_census_long<-rbind(
  data.frame(Measure="Images", Social.Category=comparison_data_main_Google_census$Social.Category, 
             Gender.Bias=comparison_data_main_Google_census$img_census_cent,
             Strength.Bias=abs(comparison_data_main_Google_census$img_census_cent)),
  data.frame(Measure="Text", Social.Category=comparison_data_main_Google_census$Social.Category, 
             Gender.Bias=comparison_data_main_Google_census$text_census_cent,
             Strength.Bias=abs(comparison_data_main_Google_census$text_census_cent) ))

ggplot(comparison_data_main_Google_census_long, aes(x = Gender.Bias, fill=Measure)) +
  geom_density(alpha=0.6, size=1.2) + theme_bw() + 
  scale_fill_manual(values=c("purple", "forestgreen")) + ylab("Source") + xlab("Gender Association") + 
  theme(legend.text=element_text(size=40),
        legend.position=c(0.2,0.9),
        legend.title = element_blank(),
        plot.title=element_blank(),axis.title.y=element_blank(),
        axis.title.x=element_text(size = 50, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.6),
        axis.text.y=element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +   
  scale_x_continuous(breaks=c(-1,-0.5, 0, 0.5, 1), labels =c(-1,-0.5, 0, 0.5, 1), limits = c(-1.55, 1.55)) + 
  geom_vline(xintercept = 0, linetype="dotted", size=2) + 
  geom_vline(xintercept = mean(subset(comparison_data_main_Google_census_long, Measure=="Text")$Gender.Bias), linetype="solid", size=2, color="forestgreen") + 
  geom_vline(xintercept = mean(subset(comparison_data_main_Google_census_long, Measure=="Images")$Gender.Bias), linetype="solid", size=2, color="purple") + 
  annotate(geom="text", x=-1, y=0.7, size=15, label="Female",color="Black") + 
  annotate(geom="text", x=1.1, y=0.7, size=15, label="Male",color="Black")

#ggsave('census_centered_fig.png', width=10, height=10, path = savepath_supp)

t.test(subset(comparison_data_main_Google_census_long, Measure=="Text")$Gender.Bias, 
       subset(comparison_data_main_Google_census_long, Measure=="Images")$Gender.Bias, paired=T) 
t.test(subset(comparison_data_main_Google_census_long, Measure=="Text")$Gender.Bias) 
t.test(subset(comparison_data_main_Google_census_long, Measure=="Images")$Gender.Bias) 

##########################################
#results while examining uncropped images# 
##########################################
uncropped<-read.csv(paste(data_path, "data_uncropped_raw.csv", sep=""))
uncropped_clean<-subset(uncropped, humface=="Yes" & catface=="Yes" & gender %in% c("Male", "Female"))
prop.test(sum(uncropped_clean$gender == "Male"), nrow(uncropped_clean), p=0.5)

uncropped_fig<-uncropped_clean %>% group_by(Social.Category) %>%
  dplyr::summarise(numimgs=length(unique(face_id)),Prop.Male=sum(gender=="Male")/length(gender), Prop.Female=sum(gender=="Female")/length(gender),Img.Gender.Parity.Uncropped=Prop.Male-Prop.Female)
comparison_data_main_simp<-comparison_data_main_Google[,c("Social.Category", "Img.Gender.Parity", "GoogleNews.300D.Gender.Norm")]
uncropped_comparison<-merge(uncropped_fig[,c("Social.Category", "Img.Gender.Parity.Uncropped")], comparison_data_main_simp, by=c("Social.Category"))

uncropped_comparison_long<-rbind(
  data.frame(Measure="Images Cropped", 
             Social.Category=uncropped_comparison$Social.Category, 
             Gender.Bias=uncropped_comparison$Img.Gender.Parity,
             Strength.Bias=abs(uncropped_comparison$Img.Gender.Parity)),
  data.frame(Measure="Text", 
             Social.Category=uncropped_comparison$Social.Category, 
             Gender.Bias=uncropped_comparison$GoogleNews.300D.Gender.Norm,
             Strength.Bias=abs(uncropped_comparison$GoogleNews.300D.Gender.Norm) ),
  data.frame(Measure="Images Uncropped", 
             Social.Category=uncropped_comparison$Social.Category, 
             Gender.Bias=uncropped_comparison$Img.Gender.Parity.Uncropped,
             Strength.Bias=abs(uncropped_comparison$Img.Gender.Parity.Uncropped) ))

ggplot(uncropped_comparison_long, aes(x = Gender.Bias, fill=Measure, color=Measure, group=Measure, linetype=Measure, alpha=Measure)) + 
  scale_alpha_manual(values=c(0,0.6,0.6)) + scale_fill_manual(values=c("purple", "purple", "forestgreen")) + 
  scale_color_manual(values=c("purple", "black", "black")) + scale_linetype_manual(values=c("dashed","solid","solid")) + 
  geom_density(lwd = 2) + theme_bw() + xlab("Gender Association") + ylab("Density") + 
  theme(legend.text=element_text(size=28),legend.position="top",legend.title = element_blank(),
        plot.title=element_text(size = 40, hjust = 0.5),axis.title.y=element_text(size = 40, hjust = 0.5),
        axis.title.x=element_text(size = 40, hjust = 0.5),axis.text.x=element_text(size = 40, hjust = 0.5, vjust=0.7),
        axis.text.y=element_text(size = 40, hjust = 0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits=c(-1.4,1.4), breaks=c(-1,-0.5,0,0.5,1)) + 
  geom_vline(xintercept = mean(subset(uncropped_comparison_long, Measure=="Images Cropped")$Gender.Bias, na.rm=T), 
             color="purple", size=2, alpha=1, linetype="solid") + 
  geom_vline(xintercept = mean(subset(uncropped_comparison_long, Measure=="Images Uncropped")$Gender.Bias, na.rm=T), 
             color="purple", size=2, alpha=1, linetype="dashed") + 
  geom_vline(xintercept = mean(subset(uncropped_comparison_long, Measure=="Text")$Gender.Bias, na.rm=T), 
             color="forestgreen", size=2, alpha=1, linetype="solid") + 
  geom_vline(xintercept = 0, color="black", size=4, alpha=1, linetype="solid") + 
  annotate(geom="text", x=-0.8, y=1, size=12, label="Female",color="Black") + 
  annotate(geom="text", x=0.8, y=1, size=12, label="Male",color="Black") + 
  annotate(geom="text", x=-0.8, y=1.5, size=12, label="N=291\nCategories",color="Black") 

#ggsave('uncropped_fig.png', width=10, height=10, path = savepath_supp)

cor.test(uncropped_comparison$Img.Gender.Parity.Uncropped, uncropped_comparison$Img.Gender.Parity)

###############################
#distribution of image sources# 
###############################
img_sources<-read.csv(paste(data_path, "img_source_stats.csv", sep=""))

ggplot(img_sources, aes(x=Freq, y=reorder(Source, Freq)))+
  geom_bar(stat="identity", size=2, color="black", fill="grey90") + 
  xlab("Portion of Images from Source") + ylab("Source") + theme_bw() + 
  theme(axis.text.x = element_text(size=35), axis.text.y = element_text(size=35),
        axis.title.x = element_text(size=35),axis.title.y = element_blank(),
        legend.position = "none", legend.text=element_text(size=35),
        legend.title=element_text(size=35, face="bold"),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) 

#ggsave('image_sources.png', width=10, height=10, path = savepath_supp)

#Comparison against different IP addresses 
GEO<-read.csv(paste(data_path, "GEO_replication.csv", sep=""))

GEO_agg<-GEO %>% group_by(query, IP) %>% 
  dplyr::summarise(numMale=sum(gender=="Male"),numAll=length(gender),
    Prop.Male=numMale/numAll,Prop.Female=sum(gender=="Female")/numAll,Gender.Bias=Prop.Male-Prop.Female)

##CORRELATION BETWEEN IPs##
IPs<-unique(as.character(GEO_agg$IP))

corr_IPs<-data.frame()
for(i in 1:length(IPs)){
  IP_i<-IPs[i]
  IP_i_df<-subset(GEO_agg, IP == IP_i)
  IP_i_df_categories<-unique(as.character(IP_i_df$query))
  
  for(j in 1:length(IPs)){
    IP_j<-IPs[j]
    IP_j_df<-subset(GEO_agg, IP == IP_j)
    IP_j_df_categories<-unique(as.character(IP_j_df$query))
    categories_int<-intersect(IP_j_df_categories, IP_i_df_categories)
    
    IP_j_df_m<-subset(IP_j_df, query %in% categories_int)
    IP_i_df_m<-subset(IP_i_df, query %in% categories_int)
    
    corr_out<-cor.test(IP_i_df_m$Gender.Bias,IP_j_df_m$Gender.Bias)
    pval<-round(corr_out$p.value,2)
    Correlation<-round(corr_out$estimate, 2)
    corr_IPs<-rbind(corr_IPs, data.frame(IP_i=IP_i, IP_j=IP_j, Correlation=Correlation, pval=pval))
  }
}

ggplot(corr_IPs, aes(x = IP_i, y = IP_j, fill = Correlation)) +
  geom_tile(color = "black") +theme_bw() + 
  geom_text(aes(label = Correlation), color = "white", size = 8) +
  coord_fixed() + 
  scale_fill_continuous(limits=c(0, 1), breaks=seq(0,1,by=0.2)) + 
  theme(axis.title = element_blank(), 
        axis.text = element_text(size=18),
        legend.text = element_text(size=18), 
        legend.title = element_text(size=20)) 

#ggsave('GEO_corr_IPs.png', width=10, height=10, path = savepath_supp)

##Image overlap BETWEEN IPs##
img_overlap_IPs<-data.frame()
for(i in 1:length(IPs)){
  IP_i<-IPs[i]
  IP_i_df<-subset(GEO, IP == IP_i)
  IP_i_df_imgs<-unique(IP_i_df$image_id)
  
  for(j in 1:length(IPs)){
    IP_j<-IPs[j]
    IP_j_df<-subset(GEO, IP == IP_j)
    IP_j_df_imgs<-unique(IP_j_df$image_id)
    
    img_overlap<-round(length(intersect(IP_i_df_imgs, IP_j_df_imgs))/length(unique(IP_i_df_imgs, IP_j_df_imgs)),2)
    img_overlap_IPs<-rbind(img_overlap_IPs, data.frame(IP_i=IP_i, IP_j=IP_j, img_overlap=img_overlap))
    
  }
}

ggplot(img_overlap_IPs, aes(x = IP_i, y = IP_j, fill = img_overlap)) +
  geom_tile(color = "black") +theme_bw() + 
  geom_text(aes(label = img_overlap), color = "white", size = 8) +
  coord_fixed() + 
  scale_fill_continuous(limits=c(0, 1), breaks=seq(0,1,by=0.2), name="Image Overlap") + 
  theme(axis.title = element_blank(), 
        axis.text = element_text(size=18),
        legend.text = element_text(size=18), 
        legend.title = element_text(size=20)) 

#ggsave('GEO_overlap_IPs.png', width=10, height=10, path = savepath_supp)

ggplot(GEO_agg, aes(x = Gender.Bias, color=IP, fill=IP, alpha=IP)) +
  geom_density(size=2.5) + theme_bw() + 
  scale_color_manual(values=c("grey20", "deeppink2", "turquoise2", "goldenrod", "dodgerblue")) + 
  scale_fill_manual(values=c("grey20","deeppink2","turquoise2","goldenrod","dodgerblue")) + 
  scale_alpha_manual(values=c(0,0,0,0,0,0,0)) + 
  ylab("Source") + xlab("Gender Association") + 
  theme(legend.text=element_text(size=40),
        legend.position="top",
        legend.title = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.6),
        axis.text.y=element_text(size = 60, hjust = 0.6), 
        plot.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +   
  scale_x_continuous(breaks=c(-1,-0.5, 0, 0.5, 1), labels =c(-1,-0.5, 0, 0.5, 1), limits = c(-1.4, 1.4)) + 
  annotate(geom="text", x=-1, y=0.8, size=15, label="Female",color="Black") + 
  annotate(geom="text", x=1.1, y=0.8, size=15, label="Male",color="Black") + 
  geom_vline(xintercept = 0, linetype="dotted", size=2) + 
  geom_vline(xintercept = mean(subset(GEO_agg, IP=="Amsterdam")$Gender.Bias, na.rm=T), linetype="solid", size=2, color="black") + 
  geom_vline(xintercept = mean(subset(GEO_agg, IP=="Bangalore")$Gender.Bias, na.rm=T), linetype="solid", size=2, color="deeppink2") + 
  geom_vline(xintercept = mean(subset(GEO_agg, IP=="Frankfurt")$Gender.Bias, na.rm=T), linetype="solid", size=2, color="turquoise2") + 
  geom_vline(xintercept = mean(subset(GEO_agg, IP=="Singapore")$Gender.Bias, na.rm=T), linetype="solid", size=2, color="goldenrod") + 
  geom_vline(xintercept = mean(subset(GEO_agg, IP=="Toronto")$Gender.Bias, na.rm=T), linetype="solid", size=2, color="dodgerblue")

#ggsave('GEO_gender_dist.png', width=16, height=16, path = savepath_supp)

##################################################
#Results comparing against other embedding models#
##################################################
embedding_comp_long<-rbind(
  data.frame(model="Google Images",
             Social.Category = comparison_data_main_Google$Social.Category, 
             Gender.bias = comparison_data_main_Google$Img.Gender.Parity,
             Gender.bias.str = abs(comparison_data_main_Google$Img.Gender.Parity)), 
  data.frame(model="w2vec (Google 2013)",
             Social.Category = comparison_data_main_Google$Social.Category, 
             Gender.bias = comparison_data_main_Google$GoogleNews.300D.Gender.Norm,
             Gender.bias.str = abs(comparison_data_main_Google$GoogleNews.300D.Gender.Norm)), 
  data.frame(model="Bert",
             Social.Category = comparison_data_main_Google$Social.Category, 
             Gender.bias = comparison_data_main_Google$Bert.Norm,
             Gender.bias.str = abs(comparison_data_main_Google$Bert.Norm)), 
  data.frame(model="ConceptNet",
             Social.Category = comparison_data_main_Google$Social.Category, 
             Gender.bias = comparison_data_main_Google$ConceptNet.Norm,
             Gender.bias.str = abs(comparison_data_main_Google$ConceptNet.Norm)), 
  data.frame(model="FastText",
             Social.Category = comparison_data_main_Google$Social.Category, 
             Gender.bias = comparison_data_main_Google$FastText.Norm,
             Gender.bias.str = abs(comparison_data_main_Google$FastText.Norm)), 
  data.frame(model="Twitter Glove",
             Social.Category = comparison_data_main_Google$Social.Category, 
             Gender.bias = comparison_data_main_Google$Glove.Twitter.Norm,
             Gender.bias.str = abs(comparison_data_main_Google$Glove.Twitter.Norm)), 
  data.frame(model="Wiki Glove",
             Social.Category = comparison_data_main_Google$Social.Category, 
             Gender.bias = comparison_data_main_Google$Glove.Wiki.Norm,
             Gender.bias.str = abs(comparison_data_main_Google$Glove.Wiki.Norm)),
  data.frame(model="GPT3",
             Social.Category = comparison_data_main_Google$Social.Category, 
             Gender.bias = comparison_data_main_Google$GPT3.gender.norm,
             Gender.bias.str = abs(comparison_data_main_Google$GPT3.gender.norm)),
  data.frame(model="w2vec (News 2021-23)",
             Social.Category = comparison_data_main_Google$Social.Category, 
             Gender.bias = comparison_data_main_Google$retrained.word2vec.300.Norm,
             Gender.bias.str = abs(comparison_data_main_Google$retrained.word2vec.300.Norm)))

####################
###Density Figure###
####################
embedding_comp_long$model<-
  factor(embedding_comp_long$model, 
         levels = c("GPT3", "Bert", "Wiki Glove", "Twitter Glove", "FastText", 
                    "ConceptNet", "Google Images", "w2vec (Google 2013)", "w2vec (News 2021-23)"))

embedding_comp_long$model<-factor(embedding_comp_long$model, 
                                  levels=c("Google Images","GPT3", "Bert", "Wiki Glove", "Twitter Glove", "FastText", "ConceptNet", "w2vec (News 2021-23)", "w2vec (Google 2013)"))

ggplot(embedding_comp_long, aes(x = Gender.bias, color=model, fill=model, alpha=model)) +
  geom_density(size=2.5) + theme_bw() + 
  scale_fill_manual(values=c("purple", "grey30","deeppink2","turquoise2","goldenrod","chocolate1", "gold", "blue", "forestgreen")) + 
  scale_color_manual(values=c("purple", "grey30","deeppink2","turquoise2","goldenrod","chocolate1", "gold", "blue", "forestgreen")) + 
  scale_alpha_manual(values=c(0.4,0,0,0,0,0,0,0, 0)) + 
  ylab("Density") + xlab("Gender Association") + 
  theme(legend.text=element_text(size=38),
        legend.position=c(0.78,0.83),
        #legend.position="none",
        legend.title = element_blank(),
        axis.text.y=element_text(size = 50),
        axis.title.x=element_text(size = 50, hjust = 0.5),
        axis.title.y=element_text(size = 50, hjust = 0.5),
        axis.text.x=element_text(size = 50, hjust = 0.6),
        plot.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +   
  scale_x_continuous(breaks=c(-1,-0.5, 0, 0.5, 1), labels =c(-1,-0.5, 0, 0.5, 1), limits = c(-1.4, 1.4)) + 
  scale_y_continuous(breaks=c(0, 1, 2)) + 
  annotate(geom="text", x=-1, y=0.8, size=18, label="Female",color="Black") + 
  annotate(geom="text", x=1.1, y=0.8, size=18, label="Male",color="Black") + 
  geom_vline(xintercept = 0, linetype="dotted", size=2)

#ggsave('all_models_density.png', width=16, height=16, path = savepath_supp)

embedding_comp_long$model<-factor(embedding_comp_long$model, 
                                  levels=c("Google Images","GPT3", "Bert", "Wiki Glove", "Twitter Glove", "FastText", "ConceptNet", "w2vec (News 2021-23)", "w2vec (Google 2013)"))

ggplot(embedding_comp_long, aes(x = Gender.bias.str, y = model, group=model, fill=model))+
  geom_boxplot(outlier.color="white", color="black",outlier.shape=16, outlier.size=2, notch=FALSE,  size=3)+
  xlab("Strength of Gender Association") + theme_bw() + 
  scale_fill_manual(values=c("purple", "grey30","deeppink2","turquoise2","goldenrod","chocolate1", "gold", "blue", "forestgreen")) + 
  theme(axis.text.x = element_text(size=50), 
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=50),
        plot.title = element_blank(),
        legend.position = "none", legend.text=element_text(size=60),
        legend.title=element_blank(),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  scale_x_continuous(breaks=c(0,0.25,0.5,0.75,1), labels=c("0","0.25","0.5","0.75","1"), 
                     limits=c(0,1.05)) + scale_y_discrete(limits=rev)

#ggsave('all_models_str.png', width=16, height=16, path = savepath_supp)

##############################################################################
#Correlation between Google News word2vec 2013 and retrained word2vec 2021-23#
##############################################################################
ggplot(comparison_data_main_Google, aes(x=GoogleNews.300D.Gender.Norm, y=retrained.word2vec.300.Norm, group=1))+
  geom_point(size=4, color="darkgrey") + 
  geom_smooth(size=3, method='lm', formula= y~x, color="red") + 
  xlab("Gender Association\n(w2vec Google News 2013)") + 
  ylab("Gender Association\n(w2vec Online News 2021-23)") +
  theme_bw() + 
  theme(axis.text.x = element_text(size=50), 
        axis.text.y = element_text(size=50),
        axis.title.x = element_text(size=50),
        axis.title.y = element_text(size=50),
        legend.position = c(0.83,0.1), 
        legend.text=element_text(size=40),
        legend.title=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        legend.box.background = element_rect(colour = "black"),
        legend.background = element_blank()) + 
  geom_vline(xintercept = 0, linetype="dotted", size=2) + 
  geom_hline(yintercept = 0, linetype="dotted", size=2)  

#ggsave('corr_GNews2013_OnlineNews2023.png', width=16, height=16, path = savepath_supp)

comparison_data_main_Google$Gnews2013_abs<-abs(comparison_data_main_Google$GoogleNews.300D.Gender.Norm)
comparison_data_main_Google$Onews2023_abs<-abs(comparison_data_main_Google$retrained.word2vec.300.Norm)
t.test(comparison_data_main_Google$Gnews2013_abs, comparison_data_main_Google$Onews2023_abs)

#Compare 2013 w2vec, 2021-23 w2vec, and Google Images in terms of absolute strength of gender associations
embedding_comp_long_sub<-subset(embedding_comp_long, model %in% c("Google Images", "w2vec (Google 2013)", "w2vec (News 2021-23)"))

ggplot(embedding_comp_long_sub, aes(x = Gender.bias.str, color=model, fill=model, alpha=model)) +
  geom_density(size=2.5) + theme_bw() + 
  scale_fill_manual(values=c("purple",  "blue", "forestgreen")) + 
  scale_color_manual(values=c("purple", "blue", "forestgreen")) + 
  scale_alpha_manual(values=c(0.4,0,0)) + 
  ylab("Density") + xlab("Strength of Gender Association") + 
  theme(legend.text=element_text(size=43),
        legend.position=c(0.7,0.9),
        #legend.position="top",
        legend.title = element_blank(),
        axis.text.y=element_text(size = 50),
        axis.title.x=element_text(size = 50, hjust = 0.5),
        axis.title.y=element_text(size = 50, hjust = 0.5),
        axis.text.x=element_text(size = 50, hjust = 0.6),
        plot.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +   
  scale_x_continuous(limits = c(-0.2, 1.2), breaks=c(0,0.25, 0.5, 0.75, 1)) 

#ggsave('strength_gender_bias_image_w2vec_retrain.png', width=16, height=16, path = savepath_supp)

##################################################
#Replication with Machine learning classification#
##################################################
ml_cropped<-read.csv(paste(data_path, "ml_cropped.csv", sep=""))
ml_cropped<-ml_cropped[complete.cases(ml_cropped),]
ml_cropped<-subset(ml_cropped, !is.na(gender))
ml_cropped$condition<-"Cropped"

ml_cropped_agg<-ml_cropped %>% group_by(searchDEMO, Social.Category, condition) %>% 
  dplyr::summarise(Prop.Male = sum(gender=="Male")/length(gender), 
                   Prop.Female=1-Prop.Male, 
                   Gender.Bias=Prop.Male-Prop.Female,
                   gender_conf=mean(gender_conf)) 

ml_cropped %>% group_by(gender) %>% dplyr::summarise(gender_conf=mean(gender_conf))

ml_uncropped<-read.csv(paste(data_path, "ml_uncropped.csv", sep=""))
ml_uncropped<-subset(ml_uncropped, !is.na(gender))
ml_uncropped$condition<-"Uncropped"

ml_uncropped_agg<-ml_uncropped %>% group_by(searchDEMO, Social.Category, condition) %>% 
  dplyr::summarise(Prop.Male = sum(gender=="Male")/length(gender), 
                   Prop.Female=1-Prop.Male, 
                   Gender.Bias=Prop.Male-Prop.Female,
                   gender_conf=mean(gender_conf)) 
ml_uncropped %>% group_by(gender) %>% dplyr::summarise(gender_conf=mean(gender_conf))

ml_dt_long<-rbind(ml_uncropped_agg, ml_cropped_agg)
ml_dt_long_main<-subset(ml_dt_long, searchDEMO=="None")

ggplot(ml_dt_long_main, aes(x = Gender.Bias, color=condition, group=condition, alpha=condition)) + 
  scale_alpha_manual(values=c(0,0.6,0.6)) + 
  scale_color_manual(values=c("purple", "grey70")) + 
  geom_density(lwd = 4) + theme_bw() + xlab("Gender Association") + ylab("Density") + 
  theme(legend.text=element_text(size=50),legend.position="top",legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),axis.title.y=element_text(size = 60, hjust = 0.5),
        axis.title.x=element_text(size = 60, hjust = 0.5),axis.text.x=element_text(size = 60, hjust = 0.5, vjust=0.7),
        axis.text.y=element_text(size = 60, hjust = 0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits=c(-1.4,1.4), breaks=c(-1,-0.5,0,0.5,1)) + 
  geom_vline(xintercept = mean(subset(ml_dt_long_main, condition=="Cropped")$Gender.Bias, na.rm=T), 
             color="purple", size=4, alpha=1, linetype="solid") + 
  geom_vline(xintercept = mean(subset(ml_dt_long_main, condition=="Uncropped")$Gender.Bias, na.rm=T), 
             color="grey70", size=4, alpha=1, linetype="solid") + 
  geom_vline(xintercept = 0, color="black", size=4, alpha=1, linetype="solid") + 
  annotate(geom="text", x=-1, y=1, size=20, label="Female",color="Black") + 
  annotate(geom="text", x=1.1, y=1, size=20, label="Male",color="Black") + 
  annotate(geom="text", x=-0.95, y=1.3, size=20, label="N=3,326  \n  Categories",color="Black") 

#ggsave('FIG_ML_A.png', width=16, height=16, path = savepath_supp)

ML_bar_fig<-ml_dt_long_main %>% group_by(searchDEMO,condition) %>% 
  dplyr::summarise(cilow=t.test(Prop.Male)$conf.int[1],
                   cihi=t.test(Prop.Male)$conf.int[2],
                   Prop.Male=mean(Prop.Male))

ggplot(ml_dt_long_main, aes(y=Prop.Male, x=reorder(condition, -Prop.Male), 
                            fill =  condition, group=condition, color=condition)) +
  geom_boxplot(size=2, outlier.size=0, outlier.colour = "white") + 
  scale_fill_manual(values=c("pink", "grey90")) +
  scale_color_manual(values=c("purple", "grey50")) + 
  ylab("P(Male Face)") + theme_bw() + 
  theme(axis.text.x = element_text(size=60), 
        axis.text.y = element_text(size=60),
        axis.title.y = element_text(size=60, vjust=0.5),
        axis.title.x = element_blank(),
        legend.position = "none", legend.text=element_text(size=82),
        legend.title=element_blank(),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  geom_hline(yintercept = 0.5) +  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) 

#ggsave('FIG_ML_B.png', width=16, height=16, path = savepath_supp)

#uncropped confidence 
ggplot(ml_uncropped, aes(x = gender_conf, fill=gender, color=gender, group=gender)) + 
  theme_bw() + stat_ecdf(geom = "step", pad = TRUE, size=4) + 
  scale_color_manual(values=c("goldenrod", "lightblue")) + 
  ggtitle("Uncropped Google Images") + ylab("CDF") + 
  xlab("Confidence (Gender Classification)") + 
  theme(legend.text=element_text(size=60),
        legend.position=c(0.15, 0.8),
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_text(size = 60, hjust = 0.5),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.5, vjust=0.7),
        axis.text.y=element_text(size = 60, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

#ggsave('ML_uncropped_gender_confidence_dist.png', width=17, height=17, path = savepath_supp)

#cropped confidence 
ggplot(ml_cropped, aes(x = gender_conf, fill=gender, color=gender, group=gender)) + 
  theme_bw() + stat_ecdf(geom = "step", pad = TRUE, size=4) + 
  scale_color_manual(values=c("goldenrod", "lightblue")) + 
  ggtitle("Cropped Google Images") + ylab("CDF") + 
  xlab("Confidence (Gender Classification)") + 
  theme(legend.text=element_text(size=60),
        legend.position=c(0.15, 0.8),
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_text(size = 60, hjust = 0.5),
        axis.title.x=element_text(size = 60, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.5, vjust=0.7),
        axis.text.y=element_text(size = 60, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

#ggsave('ML_cropped_gender_confidence_dist.png', width=17, height=17, path = savepath_supp)

#####################################
#Model controlling for ML confidence#
#####################################
ml_cropped<-ml_cropped %>% dplyr::select(-X)
ml_all<-rbind(ml_cropped, ml_uncropped)
ml_all$Gender.of.Face<-as.factor(ml_all$gender)
levels(ml_all$Gender.of.Face)<-c(0,1)
ml_all_main<-subset(ml_all, searchDEMO=="None")
quantiles <- quantile(ml_all_main$gender_conf, probs = seq(.1, .9, by = .1))
ml_all_main$confidence_bin <- cut2(ml_all_main$gender_conf, cuts = as.numeric(quantiles))
unique(ml_all_main$confidence_bin)

ml_all_main_agg<-ml_all_main %>% group_by(confidence_bin, condition) %>% 
  dplyr::summarise(
    pMALE=prop.test(sum(gender=="Male"), length(gender))$estimate, 
    cilow=prop.test(sum(gender=="Male"), length(gender))$conf.int[1], 
    cihi=prop.test(sum(gender=="Male"), length(gender))$conf.int[2])

ggplot(ml_all_main_agg, 
       aes(x = confidence_bin, y = pMALE, group=condition, color=condition, fill=condition)) + theme_bw() + 
  geom_point(size=8, position=position_dodge(0.8)) + 
  geom_errorbar(aes(ymin=cilow, ymax=cihi), width=0, position=position_dodge(0.8)) + 
  xlab("Confidence Deciles") + ylab("P(Male)") + 
  theme(legend.text=element_text(size=60),
        legend.position="top",
        legend.title = element_blank(),
        plot.title=element_text(size = 60, hjust = 0.5),
        axis.title.y=element_text(size = 60, hjust = 0.5),
        axis.title.x=element_text(size = 60, hjust = 0.5, vjust=0.3),
        axis.text.x=element_text(size = 30, hjust = 0.5, vjust=0.5, angle=25),
        axis.text.y=element_text(size = 60, hjust = 0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  coord_cartesian(ylim=c(0.5,1)) + 
  geom_hline(yintercept = 0.5)

#ggsave('ML_confidence_cutoff.png', width=17, height=17, path = savepath_supp)

####################################################################
#Results according to different definitions of the gender dimension#
####################################################################
comparison_data_main_Google$gender_dim1_bin<-ntile(comparison_data_main_Google$GoogleNews.300D.Gender.Norm, 50)
comparison_data_main_Google$gender_dim2_bin<-ntile(comparison_data_main_Google$GoogleNews.300D.Gender.v2.Norm, 50)
comparison_data_main_Google$gender_dim3_bin<-ntile(comparison_data_main_Google$GoogleNews.300D.Gender.v3.Norm, 50)
comparison_data_main_Google$gender_dim4_bin<-ntile(comparison_data_main_Google$GoogleNews.300D.Gender.v4.Norm, 50)

comp_V1_V2<-comparison_data_main_Google %>% group_by(gender_dim1_bin) %>% 
  dplyr::summarise(cilow = t.test(GoogleNews.300D.Gender.v2.Norm)$conf.int[1],
                   cihi = t.test(GoogleNews.300D.Gender.v2.Norm)$conf.int[2],
                   GoogleNews.300D.Gender.v2.Norm = mean(GoogleNews.300D.Gender.v2.Norm), 
                   GoogleNews.300D.Gender.Norm = mean(GoogleNews.300D.Gender.Norm))

ggplot(comp_V1_V2, aes(x=GoogleNews.300D.Gender.Norm, y=GoogleNews.300D.Gender.v2.Norm))+
  geom_point(size=3, color="black") + geom_line(size=1) +
  geom_errorbar(aes(ymin=cilow, ymax=cihi), linetype="solid",width=0, size=1)+
  xlab("Gender Association\n(Text, Main)") + ylab("Gender Association\n(Text, Version 2)") + theme_bw() + 
  theme(axis.text.x = element_text(size=75), axis.text.y = element_text(size=75),
        axis.title.x = element_text(size=70),axis.title.y = element_text(size=70),
        legend.position = "none", legend.text=element_text(size=30),
        legend.title=element_text(size=30, face="bold"),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  geom_hline(yintercept = 0, linetype="dotted", size=1) + 
  geom_vline(xintercept = 0, linetype="dotted", size=1) 

cor.test(comparison_data_main_Google$GoogleNews.300D.Gender.Norm, comparison_data_main_Google$GoogleNews.300D.Gender.v2.Norm)
#ggsave('corr_gender_dim_V1_V2.png', width=16, height=16, path = savepath_supp)

comp_V1_V3<-comparison_data_main_Google %>% group_by(gender_dim1_bin) %>% 
  dplyr::summarise(cilow = t.test(GoogleNews.300D.Gender.v3.Norm)$conf.int[1],
                   cihi = t.test(GoogleNews.300D.Gender.v3.Norm)$conf.int[2],
                   GoogleNews.300D.Gender.v3.Norm = mean(GoogleNews.300D.Gender.v3.Norm), 
                   GoogleNews.300D.Gender.Norm = mean(GoogleNews.300D.Gender.Norm))

ggplot(comp_V1_V3, aes(x=GoogleNews.300D.Gender.Norm, y=GoogleNews.300D.Gender.v3.Norm))+
  geom_point(size=3, color="black") + geom_line(size=1) +
  geom_errorbar(aes(ymin=cilow, ymax=cihi), linetype="solid",width=0, size=1)+
  xlab("Gender Association\n(Text, Main)") + ylab("Gender Association\n(Text, Version 3)") + theme_bw() + 
  theme(axis.text.x = element_text(size=75), axis.text.y = element_text(size=75),
        axis.title.x = element_text(size=70),axis.title.y = element_text(size=70),
        legend.position = "none", legend.text=element_text(size=30),
        legend.title=element_text(size=30, face="bold"),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  geom_hline(yintercept = 0, linetype="dotted", size=1) + 
  geom_vline(xintercept = 0, linetype="dotted", size=1) 

cor.test(comparison_data_main_Google$GoogleNews.300D.Gender.Norm, comparison_data_main_Google$GoogleNews.300D.Gender.v3.Norm)
#ggsave('corr_gender_dim_V1_V3.png', width=16, height=16, path = savepath_supp)

comp_V1_V4<-comparison_data_main_Google %>% group_by(gender_dim1_bin) %>% 
  dplyr::summarise(cilow = t.test(GoogleNews.300D.Gender.v4.Norm)$conf.int[1],
                   cihi = t.test(GoogleNews.300D.Gender.v4.Norm)$conf.int[2],
                   GoogleNews.300D.Gender.v4.Norm = mean(GoogleNews.300D.Gender.v4.Norm), 
                   GoogleNews.300D.Gender.Norm = mean(GoogleNews.300D.Gender.Norm))

ggplot(comp_V1_V4, aes(x=GoogleNews.300D.Gender.Norm, y=GoogleNews.300D.Gender.v4.Norm))+
  geom_point(size=3, color="black") + geom_line(size=1) +
  geom_errorbar(aes(ymin=cilow, ymax=cihi), linetype="solid",width=0, size=1)+
  xlab("Gender Association\n(Text, Main)") + ylab("Gender Association\n(Text, Version 4)") + theme_bw() + 
  theme(axis.text.x = element_text(size=75), axis.text.y = element_text(size=75),
        axis.title.x = element_text(size=70),axis.title.y = element_text(size=70),
        legend.position = "none", legend.text=element_text(size=30),
        legend.title=element_text(size=30, face="bold"),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + 
  geom_hline(yintercept = 0, linetype="dotted", size=1) + 
  geom_vline(xintercept = 0, linetype="dotted", size=1) 

cor.test(comparison_data_main_Google$GoogleNews.300D.Gender.Norm, comparison_data_main_Google$GoogleNews.300D.Gender.v4.Norm)
#ggsave('corr_gender_dim_V1_V4.png', width=16, height=16, path = savepath_supp)

#Replicate main for gender dimension v2#
dim_replication_fig2<-rbind(data.frame(Measure="Text.Main", Gender.Parity = comparison_data_main_Google$GoogleNews.300D.Gender.Norm, 
                            Social.Category=comparison_data_main_Google$Social.Category), 
                 data.frame(Measure="Text.v2", Gender.Parity = comparison_data_main_Google$GoogleNews.300D.Gender.v2.Norm, 
                            Social.Category=comparison_data_main_Google$Social.Category), 
                 data.frame(Measure="Text.v3", Gender.Parity = comparison_data_main_Google$GoogleNews.300D.Gender.v3.Norm, 
                            Social.Category=comparison_data_main_Google$Social.Category), 
                 data.frame(Measure="Text.v4", Gender.Parity = comparison_data_main_Google$GoogleNews.300D.Gender.v4.Norm, 
                            Social.Category=comparison_data_main_Google$Social.Category), 
                 data.frame(Measure="Images", Gender.Parity = comparison_data_main_Google$Img.Gender.Parity, 
                            Social.Category=comparison_data_main_Google$Social.Category))

dim_replication_fig2<-dim_replication_fig2[complete.cases(dim_replication_fig2),]
dim_replication_fig2<-subset(dim_replication_fig2, Social.Category %in% unique(subset(dim_replication_fig2, Measure=="Text.Main")$Social.Category))

ggplot(dim_replication_fig2, aes(x = Gender.Parity, color=Measure)) +
  geom_density(alpha=0.6, size=1.4) + theme_bw() + 
  geom_vline(xintercept = 0, linetype="dotted", size=2) + 
  scale_color_manual(values=c("purple", "forestgreen", "green", "lightgreen","darkgreen")) + 
  ylab("Density") + xlab("Gender Association") + 
  theme(legend.text=element_text(size=50),
        legend.position=c(0.2,0.8),
        legend.title = element_blank(),
        plot.title=element_blank(),
        axis.title.y=element_text(size = 50, hjust = 0.5),
        axis.title.x=element_text(size = 50, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.6),
        axis.text.y=element_text(size = 60),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +   
  scale_x_continuous(breaks=c(-1,-0.5, 0, 0.5, 1), labels =c(-1,-0.5, 0, 0.5, 1), limits = c(-1.4, 1.4)) + 
  geom_vline(xintercept = 0, linetype="dotted", size=2) + 
  annotate(geom="text", x=-1, y=0.9, size=20, label="Female",color="Black") + 
  annotate(geom="text", x=1.1, y=0.9, size=20, label="Male",color="Black") 

#ggsave('corr_gender_dim_density.png', width=16, height=16, path = savepath_supp)

###############################################
#Replication with Explicitly Gendered Searches#
###############################################
SI_gendered_searches<-data_demo %>% 
  group_by(Data.Source, Social.Category, searchDEMO, face_id, image_id) %>% 
  dplyr::summarise(Img.Gender.Mode=getmode(Img.Gender)) %>% group_by(searchDEMO, Social.Category) %>% 
  dplyr::summarise(propMALE=sum(Img.Gender.Mode=="Male")/length(Img.Gender.Mode),
                   propFEMALE=sum(Img.Gender.Mode=="Female")/length(Img.Gender.Mode))

SI_gendered_searches_panelA<-SI_gendered_searches %>% group_by(searchDEMO, Social.Category) %>% dplyr::summarise(parity=propMALE-propFEMALE)
SI_gendered_searches_panelA$searchtype<-as.factor(SI_gendered_searches_panelA$searchDEMO)
levels(SI_gendered_searches_panelA$searchtype)<-c("Female Search", "Male Search")

ggplot(SI_gendered_searches_panelA, aes(x = parity, fill=searchtype)) +
  geom_density(alpha=0.6, size=1.5) + theme_bw() + 
  scale_fill_manual(values=c("orange", "blue")) + ylab("Density") + xlab("Gender Association") + 
  theme(legend.text=element_text(size=50),
        legend.position="top",
        legend.title = element_blank(),
        plot.title=element_blank(),
        axis.title.x=element_text(size = 50, hjust = 0.5),
        axis.title.y=element_text(size = 50, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.6),
        axis.text.y=element_text(size = 60, hjust = 0.6),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +   
  scale_x_continuous(breaks=c(-1,-0.5, 0, 0.5, 1), labels =c(-1,-0.5, 0, 0.5, 1), limits = c(-1.4, 1.4)) + 
  geom_vline(xintercept = 0, linetype="dotted", size=2) + 
  annotate(geom="text", x=-1, y=1.7, size=15, label="Female",color="Black") + 
  annotate(geom="text", x=1.1, y=1.7, size=15, label="Male",color="Black") 

SI_gendered_searches_fem<-subset(SI_gendered_searches, searchDEMO=="Female")
SI_gendered_searches_male<-subset(SI_gendered_searches, searchDEMO=="Male")
SI_gendered_searches_fem$propgen<-SI_gendered_searches_fem$propFEMALE
SI_gendered_searches_male$propgen<-SI_gendered_searches_male$propMALE
SI_gendered_searches_fem_simp<-SI_gendered_searches_fem %>% dplyr::select(searchDEMO, Social.Category, propgen)
SI_gendered_searches_male_simp<-SI_gendered_searches_male %>% dplyr::select(searchDEMO, Social.Category, propgen)
SI_gendered_searches_org<-rbind(SI_gendered_searches_male_simp, SI_gendered_searches_fem_simp)

SI_gendered_searches_panelB<-SI_gendered_searches_org %>% group_by(Social.Category) %>% 
  dplyr::summarise(parity=propgen[searchDEMO=="Male"]-propgen[searchDEMO=="Female"])

ggplot(SI_gendered_searches_panelB, aes(x = parity)) +
  geom_density(alpha=0.6, size=1.5) + theme_bw() + 
  ylab("Density") + xlab("Gender Association") + 
  scale_fill_manual(values=c("White")) +
  scale_color_manual(values=c("Black")) +
  theme(legend.text=element_text(size=50),
        legend.position="top",
        legend.title = element_blank(),
        plot.title=element_blank(),
        axis.title.x=element_text(size = 50, hjust = 0.5),
        axis.title.y=element_text(size = 50, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.6),
        axis.text.y=element_text(size = 60, hjust = 0.6),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +   
  scale_x_continuous(breaks=c(-1,-0.5, 0, 0.5, 1), labels =c(-1,-0.5, 0, 0.5, 1), limits = c(-0.6, 0.6)) + 
  geom_vline(xintercept = 0, linetype="dotted", size=2) + 
  annotate(geom="text", x=-.4, y=1.7, size=15, label="Female",color="Black") + 
  annotate(geom="text", x=0.4, y=1.7, size=15, label="Male",color="Black") + 
  geom_vline(xintercept = mean(SI_gendered_searches_panelB$parity), linetype="solid", size=2, color="black") 

SI_gendered_searches_fem$propgen_rev<-SI_gendered_searches_fem$propMALE
SI_gendered_searches_male$propgen_rev<-SI_gendered_searches_male$propFEMALE
SI_gendered_searches_fem_rev<-SI_gendered_searches_fem %>% dplyr::select(searchDEMO, Social.Category, propgen_rev)
SI_gendered_searches_male_rev<-SI_gendered_searches_male %>% dplyr::select(searchDEMO, Social.Category, propgen_rev)
SI_gendered_searches_org_rev<-rbind(SI_gendered_searches_male_rev, SI_gendered_searches_fem_rev)

SI_gendered_searches_panelB_rev<-SI_gendered_searches_org_rev %>% group_by(Social.Category) %>% 
  dplyr::summarise(parity=propgen_rev[searchDEMO=="Male"]-propgen_rev[searchDEMO=="Female"])

ggplot(SI_gendered_searches_panelB_rev, aes(x = parity)) +
  geom_density(alpha=0.6, size=1.5) + theme_bw() + 
  ylab("Density") + xlab("P(Female|Male.Search)-P(Male|Female.Search)") + 
  scale_fill_manual(values=c("White")) +
  scale_color_manual(values=c("Black")) +
  theme(legend.text=element_text(size=50),
        legend.position="top",
        legend.title = element_blank(),
        plot.title=element_blank(),
        axis.title.x=element_text(size = 50, hjust = 0.5),
        axis.title.y=element_text(size = 50, hjust = 0.5),
        axis.text.x=element_text(size = 60, hjust = 0.6),
        axis.text.y=element_text(size = 60, hjust = 0.6),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +   
  scale_x_continuous(breaks=c(-1,-0.5, 0, 0.5, 1), labels =c(-1,-0.5, 0, 0.5, 1), limits = c(-0.6, 0.6)) + 
  geom_vline(xintercept = 0, linetype="dotted", size=2) + 
  geom_vline(xintercept = mean(SI_gendered_searches_panelB_rev$parity), linetype="solid", size=2, color="black") 

t.test(SI_gendered_searches_panelB_rev$parity)
#ggsave('gendered_search_results_rev.png', width=18, height=18, path = savepath_supp)

#####################################
#results using the wiki-IMDB dataset# 
#####################################

###############################
#IMDb Data (IMDb-Wiki Dataset)#
###############################
IMDBceleb<-read.csv(paste(data_path, "IMDBceleb.csv", sep=""))
IMDBceleb<-subset(IMDBceleb, gender %in% c(0,1))
#IMDBceleb<-subset(IMDBceleb, age > 0 & age <= 100)
IMDBceleb$gender_cat<-as.factor(IMDBceleb$gender)
levels(IMDBceleb$gender_cat)<-c("Female", "Male")

####################################
#Wikipedia Data (IMDb-Wiki Dataset)#
####################################
wikiceleb<-read.csv(paste(data_path, "wikiceleb.csv", sep=""))
wikiceleb<-subset(wikiceleb, gender %in% c(0,1))
#wikiceleb<-subset(wikiceleb, age > 0 & age <= 100)
wikiceleb$gender_cat<-as.factor(wikiceleb$gender)
levels(wikiceleb$gender_cat)<-c("Female", "Male")

wiki_imdb_prop_fig<-rbind(
  data.frame(Source="IMDb", numMale=sum(IMDBceleb$gender_cat=="Male"),
             numFaces=length(IMDBceleb$gender_cat)) %>%
    mutate(Prop.Male=numMale/numFaces,cilow=prop.test(numMale,numFaces)$conf.int[1],
           cihi=prop.test(numMale,numFaces)$conf.int[2]),   
  data.frame(Source="Wiki", numMale=sum(wikiceleb$gender_cat=="Male"),
             numFaces=length(wikiceleb$gender_cat)) %>%
    mutate(Prop.Male=numMale/numFaces,cilow=prop.test(numMale,numFaces)$conf.int[1],
           cihi=prop.test(numMale,numFaces)$conf.int[2]))

ggplot(wiki_imdb_prop_fig, aes(y=Prop.Male, x=Source, fill = Source))+
  geom_bar(stat="identity", position=position_dodge(0.9), color="black", size=3) + 
  scale_fill_manual(values=c("blue", "red")) + 
  geom_errorbar(aes(ymin=cilow, ymax=cihi), linetype="solid",width=0.03, size=6)+
  xlab("Measure") + ylab("P(Male Face)") + 
  theme_bw() + theme(axis.text.x = element_text(size=50),
                     axis.text.y = element_text(size=50),
                     axis.title.y = element_text(size=50),
                     axis.title.x = element_blank(),
                     strip.text.x = element_text(size=50),
                     legend.position = "none", 
                     legend.text=element_text(size=18),
                     legend.title=element_text(size=18, face="bold"),
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_blank()) + 
  coord_cartesian(ylim=c(0.45,0.85)) + 
  geom_hline(yintercept = 0.5, linetype="dashed", size=2)

#ggsave('wiki_imdb_prop_fig.png', width=16, height=16, path = savepath_supp)

IMdb.Celeb.Bias<-subset(wiki_imdb_prop_fig, Source=="IMDb")$Prop.Male - (1 - subset(wiki_imdb_prop_fig, Source=="IMDb")$Prop.Male)
Wiki.Celeb.Bias<-subset(wiki_imdb_prop_fig, Source=="Wiki")$Prop.Male - (1 - subset(wiki_imdb_prop_fig, Source=="Wiki")$Prop.Male)
Census.Celeb.Bias<- 0.51-0.49 #male and female representation from the cps_majorgroup, "Arts, Design, entertainment, sports, and media occupations", 2019 US Census
Wiki.FastText.Celeb.Bias<-subset(data_binary_mode_agg_macro_final, Social.Category=="celebrity" & searchDEMO=="None")$FastText
Wiki.Glove.Celeb.Bias<-subset(data_binary_mode_agg_macro_final, Social.Category=="celebrity" & searchDEMO=="None")$Wiki.300D.Gender

wiki_imdb_point_fig<-data.frame(
  Source=c("Wiki.Image", "IMDb.Image", "Census","Wiki.Text.Glove","Wiki.Text.FastText"), 
  Gender.Bias=c(Wiki.Celeb.Bias, IMdb.Celeb.Bias, Census.Celeb.Bias,Wiki.Glove.Celeb.Bias,Wiki.FastText.Celeb.Bias)) %>%
  mutate(Gender.Str=abs(Gender.Bias))

wiki_imdb_point_fig$Source<-as.factor(wiki_imdb_point_fig$Source)
levels(wiki_imdb_point_fig$Source)<-c("Census","IMDb\nImage","Wiki\nImage","Wiki\nFastText","Wiki\nGlove" )

ggplot(wiki_imdb_point_fig, aes(x=Gender.Bias, y=reorder(Source, Gender.Bias), fill = Source, color = Source, shape = Source))+
  geom_point(size=18) +  scale_color_manual(values=c("orange", "blue", "red","lightgreen", "black")) +
  scale_fill_manual(values=c("orange", "blue", "red","lightgreen", "black")) +
  scale_shape_manual(values=c(25, 19, 18, 17, 15)) + 
  ylab("Measure") + xlab("Gender Associations for 'Celebrity'") + 
  theme_bw() + theme(axis.text.x = element_text(size=50),
                     axis.text.y = element_text(size=50),
                     axis.title.y = element_blank(),
                     axis.title.x = element_text(size=50),
                     strip.text.x = element_text(size=50),
                     legend.position = "none", 
                     legend.text=element_text(size=18),
                     legend.title=element_text(size=18, face="bold"),
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_blank()) + 
  geom_vline(xintercept = 0, linetype="dashed") + 
  scale_x_continuous(limits = c(-1,1)) + 
  annotate(geom="text", x=-0.7, y=1, size=17, label="Female",color="Black") + 
  annotate(geom="text", x=0.7, y=1, size=17, label="Male",color="Black")

#ggsave('wiki_imdb_point_fig.png', width=16, height=16, path = savepath_supp)

############################
#Replication with wikipedia#
############################
wiki_dt<-subset(data_binary_mode_agg_macro_final, Data.Source=="Wikipedia")

wiki_dt_clean<-subset(wiki_dt, num_faces>=10)

wiki_rep<-rbind(data.frame(Measure="Text.50D", Gender.Parity = wiki_dt$Wiki.50D.Gender.Norm, 
                            Social.Category=wiki_dt$Social.Category), 
                data.frame(Measure="Text.100D", Gender.Parity = wiki_dt$Wiki.100D.Gender.Norm, 
                           Social.Category=wiki_dt$Social.Category), 
                data.frame(Measure="Text.200D", Gender.Parity = wiki_dt$Wiki.200D.Gender.Norm, 
                           Social.Category=wiki_dt$Social.Category), 
                data.frame(Measure="Text.300D", Gender.Parity = wiki_dt$Wiki.300D.Gender.Norm, 
                           Social.Category=wiki_dt$Social.Category), 
                 data.frame(Measure="Images", Gender.Parity = wiki_dt$Img.Gender.Parity, 
                            Social.Category=wiki_dt$Social.Category))

wiki_rep_main<-wiki_rep[complete.cases(wiki_rep),]
wiki_rep_main<-subset(wiki_rep_main, Social.Category %in% unique(subset(wiki_rep, Measure=="Text.300D")$Social.Category))

wiki_rep$Measure<-factor(wiki_rep$Measure, levels=c("Text.50D", "Text.100D", "Text.200D", "Text.300D", "Images"))

ggplot(wiki_rep, aes(x = abs(Gender.Parity), color=Measure, fill=Measure, alpha=Measure)) +
  geom_density(size=2) + theme_bw() + 
  scale_color_manual(values=c("blue", "red","forestgreen", "black","purple")) +
  scale_fill_manual(values=c("blue", "red","forestgreen", "black", "purple")) +
  scale_alpha_manual(values=c(0, 0, 0, 0, 0.6)) +
  ylab("Density") + xlab("Strength of Gender Association") + 
  theme(legend.text=element_text(size=40),
        legend.position="top",
        legend.title = element_blank(),
        plot.title=element_blank(),
        axis.title.y=element_text(size = 40, hjust = 0.5),
        axis.title.x=element_text(size = 40, hjust = 0.5),
        axis.text.x=element_text(size = 40, hjust = 0.6),
        axis.text.y=element_text(size = 40, hjust = 0.6),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(limits=c(-0.3,1.3), breaks=c(0,0.25,0.5,0.75,1))

#ggsave('wiki_rep_density.png', width=16, height=16, path = savepath_supp)

wiki_rep_pmale<-wiki_rep %>% group_by(Measure) %>% 
  dplyr::summarise(
    Prop.Male=sum(Gender.Parity>0, na.rm=T)/length(Gender.Parity),
    cilow=prop.test(sum(Gender.Parity>0, na.rm=T),length(Gender.Parity))$conf.int[1],
    cihi=prop.test(sum(Gender.Parity>0, na.rm=T),length(Gender.Parity))$conf.int[2])

wiki_rep_pmale$Measure<-factor(wiki_rep_pmale$Measure, levels=c("Text.50D", "Text.100D", "Text.200D", "Text.300D", "Images"))

ggplot(wiki_rep_pmale, aes(y=Prop.Male, x=Measure, fill=Measure))+
  geom_bar(stat="identity", position=position_dodge(0.9), color="black", size=3) + 
  geom_errorbar(aes(ymin=cilow, ymax=cihi), linetype="solid",width=0.3, size=2, color="black")+
  scale_fill_manual(values=c("blue", "red","forestgreen", "darkgray", "purple")) +
  xlab("Measure") + ylab("P(Male Category)") + 
  theme_bw() + theme(axis.text.x = element_text(size=40),
                     axis.text.y = element_text(size=40),
                     axis.title.y = element_text(size=40),
                     axis.title.x = element_blank(),
                     strip.text.x = element_text(size=40),
                     legend.position = "none", 
                     legend.text=element_text(size=18),
                     legend.title=element_text(size=18, face="bold"),
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_blank()) + 
  coord_cartesian(ylim=c(0.45,0.83)) + 
  geom_hline(yintercept = 0.5, linetype="dashed", size=2)

#ggsave('wiki_rep_pMale.png', width=16, height=16, path = savepath_supp)

###########################################
##Controlling for frequency of the search##
###########################################
cor.test(comparison_data_main_Google$Img.Prop.Male, comparison_data_main_Google$Google.Img.US.Search.Freq)
cor.test(comparison_data_main_Google$Img.Prop.Male, comparison_data_main_Google$Google.Img.US.Search.Freq, method="spearman")

comparison_data_main_Google$searchfreqbin<-ntile(comparison_data_main_Google$Google.Img.US.Search.Freq, 10)

Google_search_freq_plot<-comparison_data_main_Google %>% group_by(searchfreqbin) %>% 
  dplyr::summarise(Proportion.of.Male.Faces = mean(Img.Prop.Male))
cor.test(Google_search_freq_plot$searchfreqbin, Google_search_freq_plot$Proportion.of.Male.Faces)
Google_search_freq_plot<-Google_search_freq_plot[complete.cases(Google_search_freq_plot),]

ggplot(data=Google_search_freq_plot, aes(x=as.factor(searchfreqbin), y = Proportion.of.Male.Faces)) + theme_bw() + 
  geom_point(size=10,  shape=16, color="black")  + 
  ylab("Proportion of Male Faces\nPer Category") + xlab("Search Frequency (Deciles)") + 
  theme(
    plot.title = element_text(size=40, hjust = 0.5),
    axis.title.y=element_text(size=40),
    axis.title.x=element_text(size=40),
    axis.text.x=element_text(size = 40, angle = 0),
    axis.text.y=element_text(size = 40),
    legend.position = "none", 
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "black")
  ) + geom_hline(yintercept = 0.5, linetype="dotted", size=1.5) + 
  scale_y_continuous(limits=c(0.45, 0.65))

#ggsave('pMale_Google_search_freq.png', width=10, height=10, path = savepath_supp)

freq.mod<-lm(Img.Prop.Male ~ Google.Img.US.Search.Freq, data=comparison_data_main_Google)
summary(freq.mod)
tab_model(freq.mod, show.se = TRUE)

############################################################
#Robustness to number of images associated with each search#
############################################################
data_binary_mode_agg_noDEMO<-subset(data_binary_mode_agg, searchDEMO=="None" & Data.Source=="Google")

nsamp_fig<-data.frame()

for(sim in seq(1,50,1)){
  print(sim)
  for(n in seq(1,50,1)){
    sub_df<-data_binary_mode_agg_noDEMO %>% group_by(Social.Category) %>% sample_n(n, replace=TRUE)
    sub_df_agg<-sub_df %>% group_by(Social.Category) %>% 
      dplyr::summarise(prop_male=sum(Img.Gender.Mode=="Male")/length(Img.Gender.Mode))
    sub_df_agg$male_dom<-sub_df_agg$prop_male>0.5
    
    btest<-binom.test(sum(sub_df_agg$male_dom), length(sub_df_agg$male_dom), p=0.5)
    btest_pval<-btest$p.value
    btest_cilow<-as.numeric(btest$conf.int)[1]
    btest_cihi<-as.numeric(btest$conf.int)[2]
    btest_avg<-as.numeric(btest$estimate)
    
    nsamp_fig<-rbind(nsamp_fig,
                 data.frame(n=n, sim=sim, prop_male=btest_avg, cilow=btest_cilow, 
                            cihi=btest_cihi, pval=btest_pval))
  }
}

nsamp_fig_plot<-nsamp_fig %>% group_by(n) %>% 
  dplyr::summarise(prop_male=mean(prop_male), cilow=mean(cilow), ci_hi=mean(cihi))

ggplot(nsamp_fig_plot) + theme_bw() +
  geom_smooth(size=4, aes(x=n, y=prop_male), color="red") +
  xlab("Number of Images per Search Term") + ylab("Proportion of Male Faces") + 
  theme(axis.text=element_text(size=40),
        plot.title = element_text(size=40,hjust = 0.5),
        axis.title.x=element_text(size=40,hjust = 0.5),
        axis.title.y=element_text(size=40,hjust = 0.5),
        legend.position = "none",
        legend.text=element_text(size=30),
        legend.title=element_text(size=30), 
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black", fill="white", size=1.4),
        axis.text.x=element_text(size = 40, vjust=0.8), 
        axis.text.y=element_text(size = 40, vjust=0.8), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  scale_y_continuous(labels=scales::percent_format(trim=TRUE,accuracy=1)) + 
  geom_hline(yintercept = 0.5, linetype="dotted", size=2)

#ggsave('num_imgs_per_search.png', width=11, height=11, path = savepath_supp)

##############################################
##Robustness to number of categories examined#
##############################################
data_binary_mode_agg_noDEMO<-subset(data_binary_mode_agg, searchDEMO=="None" & Data.Source=="Google")

ncat_fig<-data.frame()
all_categories<-unique(data_binary_mode_agg_noDEMO$Social.Category)
for(sim in seq(10,20,1)){
  print(sim)
  for(n in seq(100,3400,100)){
    sub_categories<-sample(all_categories, n)
    sub_df<-subset(data_binary_mode_agg_noDEMO, Social.Category %in% sub_categories)
    sub_df_agg<-sub_df %>% group_by(Social.Category) %>% 
      dplyr::summarise(prop_male=sum(Img.Gender.Mode=="Male")/length(Img.Gender.Mode))
    sub_df_agg$prop_male_centered<-sub_df_agg$prop_male-0.5
    avg_prop_male<-mean(sub_df_agg$prop_male)
    ptest<-t.test(sub_df_agg$prop_male_centered)
    pval<-ptest$p.value
    cilow=as.numeric(ptest$conf.int)[1]
    cihi=as.numeric(ptest$conf.int)[2]
    ncat_fig<-rbind(ncat_fig,data.frame(n=n, sim=sim, prop_male=avg_prop_male, pval=pval,cilow=cilow,cihi=cihi))
  }
}

ggplot(ncat_fig) + theme_bw() + geom_point(size=5, aes(x=n, y=prop_male), stroke = 1.25) +
  geom_smooth(size=4, aes(x=n, y=prop_male), color="red") +
  xlab("Number of Categories Examined") + ylab("Proportion of Male Faces") + 
  theme(axis.text=element_text(size=40),plot.title = element_text(size=40,hjust = 0.5),
        axis.title.x=element_text(size=40,hjust = 0.5),axis.title.y=element_text(size=40,hjust = 0.5),
        legend.position = "none",legend.text=element_text(size=30),legend.title=element_text(size=30), 
        legend.background = element_blank(),legend.box.background = element_rect(colour = "black", fill="white", size=1.4),
        axis.text.x=element_text(size = 40, vjust=0.8), axis.text.y=element_text(size = 40, vjust=0.8), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  scale_y_continuous(labels=scales::percent_format(trim=TRUE,accuracy=1)) + 
  geom_hline(yintercept = 0.5, linetype="dotted", size=2)

#ggsave('num_categories_in_analysis.png', width=11, height=11, path = savepath_supp)

############################
#Num. Faces Cutoff Analysis#
############################
fig_face_sub<-data.frame()
fig_face_sub_fem<-data.frame()
fig_face_sub_male<-data.frame()

for(cutoff in seq(2,100,2)){
  print(cutoff)
  comparison_data_main_Google_faceSUB<-subset(comparison_data_main_Google, 
                                              num_faces >= cutoff & Data.Source=="Google")
  
  fig2A_raw_sub<-rbind(data.frame(Measure="Text", Gender.Parity = comparison_data_main_Google_faceSUB$GoogleNews.300D.Gender.Norm, 
                                  Social.Category=comparison_data_main_Google_faceSUB$Social.Category), 
                       data.frame(Measure="Images", Gender.Parity = comparison_data_main_Google_faceSUB$Img.Gender.Parity, 
                                  Social.Category=comparison_data_main_Google_faceSUB$Social.Category))
  
  fig2A_sub<-fig2A_raw_sub[complete.cases(fig2A_raw_sub),]
  fig2A_sub<-subset(fig2A_sub, Social.Category %in% unique(subset(fig2A_sub, Measure=="Text")$Social.Category))
  
  ncat_all<-length(unique(fig2A_sub$Social.Category))
  abs_out<-t.test(abs(subset(fig2A_sub, Measure == "Images")$Gender.Parity), 
                  abs(subset(fig2A_sub, Measure != "Images")$Gender.Parity), paired=T)
  
  dt_male<-subset(fig2A_sub, Gender.Parity > 0)
  ncat_male<-length(unique(dt_male$Social.Category))
  male_out<-t.test(abs(subset(dt_male, Measure == "Images")$Gender.Parity), 
                   abs(subset(dt_male, Measure != "Images")$Gender.Parity))
  
  dt_fem<-subset(fig2A_sub, Gender.Parity < 0)
  ncat_fem<-length(unique(dt_fem$Social.Category))
  fem_out<-t.test(abs(subset(dt_fem, Measure == "Images")$Gender.Parity), 
                  abs(subset(dt_fem, Measure != "Images")$Gender.Parity))
  
  fig_face_sub<-rbind(fig_face_sub, 
                      data.frame(cutoff=cutoff, 
                                 ncat=ncat_all, 
                                 t=abs_out$statistic, 
                                 cilow=abs_out$conf.int[1],
                                 cihi=abs_out$conf.int[2],
                                 mean_diff=abs_out$estimate))
  
  fig_face_sub_fem<-rbind(fig_face_sub_fem, 
                          data.frame(cutoff=cutoff, 
                                     ncat = ncat_fem, 
                                     t=fem_out$statistic, 
                                     cilow=fem_out$conf.int[1],
                                     cihi=fem_out$conf.int[2],
                                     mean_diff=mean(fem_out$conf.int)))
  
  fig_face_sub_male<-rbind(fig_face_sub_male, 
                           data.frame(cutoff=cutoff, 
                                      ncat = ncat_male, 
                                      t=male_out$statistic, 
                                      cilow=male_out$conf.int[1],
                                      cihi=male_out$conf.int[2],
                                      mean_diff=mean(male_out$conf.int)))
}

ggplot(fig_face_sub_male, aes(x=cutoff, y=mean_diff))+
  geom_point(size=12, alpha=0.4) + geom_line(size=0.4) +
  geom_errorbar(aes(ymin=cilow, ymax=cihi), linetype="solid",width=0.4, size=1)+
  ggtitle("Male-skewed Categories") + 
  xlab("Minimum Number of Faces") + 
  ylab("Strength of Gender Association\n(Images - Text)") +
  theme_bw() + theme(axis.text.x = element_text(size=30),
                     axis.text.y = element_text(size=30),
                     axis.title.x = element_text(size=30),
                     axis.title.y = element_text(size=30),
                     legend.position = "none",
                     legend.title=element_blank(),
                     plot.title = element_text(size=30,hjust=0.5),
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black")) + 
  geom_hline(yintercept = 0, linetype="solid", size=2) + 
  coord_cartesian(ylim=c(-0.05, 0.23))

#ggsave('male_face_cutoff_sweep.png', width=11, height=11, path = savepath_supp)

ggplot(fig_face_sub_fem, aes(x=cutoff, y=mean_diff))+
  geom_point(size=12, alpha=0.4) + geom_line(size=1) +
  geom_errorbar(aes(ymin=cilow, ymax=cihi), linetype="solid",width=0.4, size=1)+
  ggtitle("Female-skewed Categories") + 
  xlab("Minimum Number of Faces") + 
  ylab("Strength of Gender Association\n(Images - Text)") +
  theme_bw() + theme(axis.text.x = element_text(size=30),
                     axis.text.y = element_text(size=30),
                     axis.title.x = element_text(size=30),
                     axis.title.y = element_text(size=30),
                     plot.title = element_text(size=30,hjust=0.5),
                     legend.position = "none",
                     legend.title=element_blank(),
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black")) + 
  geom_hline(yintercept = 0, linetype="solid", size=2) + 
  coord_cartesian(ylim=c(-0.05, 0.23))

#ggsave('female_face_cutoff_sweep.png', width=11, height=11, path = savepath_supp)

#########################################
#Classification Accuracy Validation Task#
#########################################
validation_dt<-read.csv(paste(data_path, "Nature_Dvalidation_task.csv", sep=""))
validation_dt_age<-subset(validation_dt, feature=="age")
validation_dt_age$age<-as.factor(validation_dt_age$response)
validation_dt_age$age_num<-validation_dt_age$age
levels(validation_dt_age$age_num)<-c(1,2,3,4,5,6,7)
validation_dt_age$age_num<-as.numeric(as.character(validation_dt_age$age_num))

#rate accuracy gender
validation_dt_gender<-subset(validation_dt, feature=="gender")
colnames(validation_dt_gender)[2]<-"Guess.Gender"
validation_dt_gender$gender_match<-validation_dt_gender$Guess.Gender==validation_dt_gender$real_gender
table(validation_dt_gender$gender_match)/sum(table(validation_dt_gender$gender_match))

##Control for Familiarity##
validation_dt_familiar<-subset(validation_dt, feature=="fam")
colnames(validation_dt_familiar)[2]<-"familiar"
table(validation_dt_familiar$familiar)/sum(table(validation_dt_familiar$familiar))

validation_dt_familiar_agg$gender<-sapply(1:nrow(validation_dt_familiar_agg), function(x) strsplit(validation_dt_familiar_agg[x,]$img_id, "_")[[1]][2])
validation_dt_familiar_agg %>% group_by(gender) %>% dplyr::summarise(propFAM=mean(propFAM))
wilcox.test(subset(validation_dt_familiar_agg, gender=="f")$propFAM, subset(validation_dt_familiar_agg, gender=="m")$propFAM)

validation_dt_full<-merge(validation_dt_age, validation_dt_gender, by=c("SubjID", "img_id", "real_age", "real_age_num", "real_gender"))
validation_dt_full<-merge(validation_dt_full, validation_dt_familiar, by=c("SubjID", "img_id", "real_age", "real_age_num","real_gender"))
validation_dt_full_clean<-subset(validation_dt_full, familiar!="Yes")
table(validation_dt_full_clean$gender_match)/sum(table(validation_dt_full_clean$gender_match))

#######################################
####Additional Supplementary TABLES####
#######################################
data_main$Gender.of.Face<-as.factor(data_main$Img.Gender)
levels(data_main$Gender.of.Face)<-c(0,1)
data_main$Gender.of.Face<-as.numeric(as.character(data_main$Gender.of.Face))

#control for coder demographics
data_main_Google<-subset(data_main, Data.Source=="Google")
mod_cntrl_deom <- glmmTMB(Gender.of.Face ~ coder_age + coder_inc + coder_edu + coder_ideo + coder_gender + coder_race + ( 1 | Social.Category), data=data_main_Google, family=binomial)
summary(mod_cntrl_deom)
tab_model(mod_cntrl_deom, show.ci = FALSE, show.se=TRUE)

#evaluate bias toward same gender classification
f_coders<-subset(data_binary, coder_gender=="Female" & Data.Source == "Google")
table(f_coders$Img.Gender)/sum(table(f_coders$Img.Gender))
m_coders<-subset(data_binary, coder_gender=="Male" & Data.Source == "Google")
table(m_coders$Img.Gender)/sum(table(m_coders$Img.Gender))

#control for rate of intercoder agreement 
data_binary_str_agree<-data_binary %>% 
  group_by(Data.Source, Social.Category, searchDEMO, face_id, image_id, Img.Gender) %>% 
  dplyr::summarise(numcoders=length(unique(WorkerId))) %>% 
  group_by(Data.Source, Social.Category, searchDEMO, face_id, image_id) %>% 
  dplyr::mutate(propcoders=numcoders/sum(numcoders))
data_binary_str_agree$Img.Gender.Mode<-data_binary_str_agree$Img.Gender

data_binary_mode_agg_simp<-data_binary_mode_agg[,c("Data.Source", "Social.Category", "searchDEMO", "face_id", "image_id", "Img.Gender.Mode")]
data_binary_mode_agg_simp_m<-merge(data_binary_mode_agg_simp, data_binary_str_agree, by=c("Data.Source", "Social.Category", "searchDEMO", "face_id", "image_id", "Img.Gender.Mode"))
data_binary_mode_agg_simp_m$Img.Gender.Mode.Num<-as.factor(data_binary_mode_agg_simp_m$Img.Gender.Mode)
levels(data_binary_mode_agg_simp_m$Img.Gender.Mode.Num)<-c(0,1)
data_binary_mode_agg_simp_m$Img.Gender.Mode.Num<-as.numeric(as.character(data_binary_mode_agg_simp_m$Img.Gender.Mode.Num))
google_intercoder_rate<-subset(data_binary_mode_agg_simp_m, Data.Source == "Google")

mod_coder_agreeclust<- miceadds::glm.cluster(data=google_intercoder_rate, 
                                        formula= Img.Gender.Mode.Num ~ propcoders,  
                                        cluster="Social.Category", family="binomial")
summary(mod_coder_agreeclust)
exp(cbind(coef(mod_coder_agreeclust), confint(mod_coder_agreeclust)))

mod_coder_agree<- glm(data=google_intercoder_rate, formula= Img.Gender.Mode.Num ~ propcoders, family="binomial")
NagelkerkeR2(mod_coder_agree)

google_intercoder_rate %>% group_by(Img.Gender.Mode) %>% dplyr::summarise(propcoders=mean(propcoders), numcoders=mean(numcoders))
t.test(subset(google_intercoder_rate, Img.Gender.Mode=="Female")$propcoders, subset(google_intercoder_rate, Img.Gender.Mode=="Male")$propcoders)

####################################################
#Chance-corrected measures of inter-rater agreement#
####################################################
dt_rater<-data_main %>% dplyr::select(WorkerId, Social.Category, face_id, Img.Gender)
dt_rater$coder_ID<-paste(dt_rater$WorkerId, dt_rater$face_id, sep="_")
dt_rater_sub<-dt_rater %>% dplyr::select()
dt_rater_clean<-dt_rater %>% group_by(coder_ID, face_id) %>% 
  dplyr::summarise(WorkerId = unique(WorkerId), Img.Gender = sample(Img.Gender, 1))
dt_rater<-dt_rater[,!names(dt_rater) %in% c("coder_ID")]

dt_rater_matrix<-pivot_wider(dt_rater, names_from = WorkerId, values_from = Img.Gender)
dt_rater_matrix<-as.data.frame(dt_rater_matrix)
dt_rater_matrix[dt_rater_matrix == "NULL"] = NA
dt_rater_gewt<-gwet.ac1.raw(dt_rater_matrix)
dt_rater_gewt$est

#Control for number of image per search# 
data_binary_mode_agg_macro_final_NODEMO<-subset(data_binary_mode_agg_macro_final, searchDEMO=="None" & Data.Source=="Google")
mod_num_faces <- lm(Img.Prop.Male ~ num_faces, data=data_binary_mode_agg_macro_final_NODEMO)
summary(mod_num_faces)
confint(mod_num_faces)
nrow(data_binary_mode_agg_macro_final_NODEMO)

#Control for multiple faces, image repeats, and avatars
data_binary_mode_agg_avatar<-data_binary %>% 
  group_by(Data.Source, Social.Category, searchDEMO, face_id, image_id) %>% 
  dplyr::summarise(Img.Gender.Mode=getmode(Img.Gender),
                   Img.Avatar.Mode=getmode(avatar))

data_mode_avatar_Google<-subset(data_binary_mode_agg_avatar, Data.Source=="Google" & searchDEMO == "None")

data_mode_avatar_Google<-data_mode_avatar_Google %>% dplyr::group_by(Social.Category, image_id) %>% 
  dplyr::mutate(multiplefaces=ifelse(length(unique(face_id))>1,1,0),
                num_faces = length(unique(face_id))) %>% group_by(image_id) %>% 
  dplyr::mutate(num_imgrepeat=length(unique(Social.Category)),
                imgrepeat = num_imgrepeat>1)

table(data_mode_avatar_Google$imgrepeat)/sum(table(data_mode_avatar_Google$imgrepeat))
table(data_mode_avatar_Google$Img.Avatar.Mode)/sum(table(data_mode_avatar_Google$Img.Avatar.Mode))

data_mode_avatar_Google$Gender.of.Face<-as.factor(data_mode_avatar_Google$Img.Gender.Mode)
levels(data_mode_avatar_Google$Gender.of.Face)<-c(0,1)
data_mode_avatar_Google$Gender.of.Face<-as.numeric(as.character(data_mode_avatar_Google$Gender.of.Face))

mod_gen_controls <- miceadds::glm.cluster(data=data_mode_avatar_Google, 
                                          formula= Gender.of.Face ~ num_faces + num_imgrepeat + Img.Avatar.Mode, 
                                          cluster="Social.Category", family="binomial")

summary(mod_gen_controls)
exp(cbind(coef(mod_gen_controls), confint(mod_gen_controls)))  
NagelkerkeR2(mod_gen_controls$glm_res)



