#Install Packages
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("ggthemes")
# install.packages("gridExtra")
# install.packages("scales")
# install.packages("reshape2")
# install.packages("ggh4x")

#Load Packages
library(tidyverse)
library(ggplot2)
library(readxl)
library(ggthemes)
library(gridExtra)
library(grid)
library(scales)
library(reshape2)
library(lubridate)
library(ggh4x)
library(here)


#Import File

biomarkers <- read_excel(here("Biomarkers.xlsx"))

#Normalize CRP Dates

crp_labs_1<-biomarkers %>% 
  filter(lab_type=="CRP") %>% 
  mutate(time_from_admit_date= (interval(start = admit_date, end = lab_date) /
                                  duration(num = 1, units = "days")))


#CRP Graph

crp_labs_1%>% 
  filter(subject_id != 3) |> 
  mutate(subject_id = case_when(
    subject_id ==7 ~ 3,
    TRUE ~ subject_id
  )) |> 
  filter(lab_type=="CRP") %>% 
  filter(time_from_admit_date<11) %>% 
  ggplot(aes(x = time_from_admit_date, y = 10*lab_value, group = subject_id)) + geom_line(aes(color = as.factor(subject_id))) + 
  geom_point(aes(color = as.factor(subject_id))) +
  xlab("Days from Admission") + ylab("C-Reactive Protein (mg/L)")+ 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_discrete(breaks=c("1", "2", "3", "4", "5", "6", "7"), labels=c("Patient 1", "Patient 2", "Patient 3", "Patient 4", "Patient 5", "Patient 6", "Patient 7")) +
  theme(legend.title=element_blank(), legend.position=c(0.65, 0.5)) +
  annotate("text", x = 1.13, y= 230, label = "*", size=7.0) +
  annotate("text", x = 1.13, y=56, label = "*", size=7.0) +
# annotate("text", x = 3.13, y=232, label = "*", size=7.0) +
# annotate("text", x = 7.14, y=62.8, label = "+", size=6.0)+
  annotate("text", x = 1.13, y=38, label = "*", size=7.0)+ 
annotate("text", x = 1.13, y=-0.6, label = "*", size=7.0) +
  annotate("text", x = 1.13, y=13.6, label = "*", size=7.0) +
  annotate("text", x = 2.13, y=132, label = "*", size=7.0)+
annotate("text", x = 4.14, y=32, label = "+", size=6.0) +
  annotate("text", x = 4.14, y=230, label = "+ Stopped Upa", size=5.0)  +
  annotate("text", x = 4.14, y=250, label = "* Started Upa", size=6.0)

#Normalize FCP Dates

FCP_labs_1<-biomarkers %>% 
  filter(lab_type=="FCP") %>% 
  mutate(time_from_admit_date= (interval(start = admit_date, end = lab_date) /
                                  duration(num = 1, units = "days")))


#FCPGraph

FCP_labs_1%>% 
  #filter(lab_type=="CRP") %>% 
  #filter(time_from_admit_date<11) %>% 
  ggplot(aes(x = time_from_admit_date, y = lab_value, group = subject_id)) + geom_line(aes(color = as.factor(subject_id))) + 
  geom_point(aes(color = as.factor(subject_id))) +
  xlab("Days from Admission") + ylab("Fecal Calprotectin (mg/Kg)")+ 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_color_discrete(breaks=c("1", "2", "3", "4", "5", "6", "7"), labels=c("Patient 1", "Patient 2", "Patient 3", "Patient 4", "Patient 5", "Patient 6", "Patient 7"))


  theme(legend.title=element_blank(), legend.position=c(0.81, 0.25)) +
  annotate("text", x = 1.13, y= 23, label = "*", size=7.0) +
  annotate("text", x = 1.13, y=5.6, label = "*", size=7.0) +
  annotate("text", x = 3.13, y=23.2, label = "*", size=7.0) +
  annotate("text", x = 7.14, y=6.28, label = "+", size=6.0)+
  annotate("text", x = 1.13, y=3.8, label = "*", size=7.0)+ 
  annotate("text", x = 1.13, y=-0.06, label = "*", size=7.0) +
  annotate("text", x = 7.13, y=-0.06, label = "*", size=7.0) +
  annotate("text", x = 2.13, y=13.2, label = "*", size=7.0)+
  annotate("text", x = 4.14, y=3.2, label = "+", size=6.0)
