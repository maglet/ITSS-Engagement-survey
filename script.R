library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)
survey<-read_xlsx("data/S&S Staff Engagement Survey_August 17, 2022_12.12.xlsx", 
                  sheet = "raw-data", 
                  skip = 2,
                  col_names = c("Start Date",
                                "End Date",
                                "Response Type",
                                "IP Address",
                                "Progress",
                                "Duration (in seconds)",
                                "Finished",
                                "Recorded Date",
                                "response_id",
                                "Recipient Last Name",
                                "Recipient First Name",
                                "Recipient Email",
                                "External Data Reference",
                                "Location Latitude",
                                "Location Longitude",
                                "Distribution Channel",
                                "User Language",
                                "motivation",
                                "motivation_other",
                                "format",
                                "format_other",
                                "event_type",
                                "event_type_other",
                                "frequency",
                                "frequency_other",
                                "topics",
                                "present",
                                "present_name",
                                "present_email",
                                "other"     ))%>%
  select(response_id,
    "motivation",
         "motivation_other",
         "format",
         "format_other",
         "event_type",
         "event_type_other",
         "frequency",
         "frequency_other",
         "topics",
         "present",
         "present_name",
         "present_email",
         "other")

                  
topics<-read_xlsx("data/S&S Staff Engagement Survey_August 17, 2022_12.12.xlsx", 
                sheet = "topics")%>%
  filter(!is.na(Category))

topics%>%
  ggplot(aes(x = fct_infreq(Category)))+
  geom_bar(fill = "#4E2A84")+
  labs(x="")+ 
  theme(text = element_text(size = 15),
        axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 12))

frequencies<-separate_rows(survey, frequency, sep = ",")%>%
  select(response_id, frequency)

frequencies%>%
  filter(!is.na(frequency), frequency!="Other")%>%
  ggplot(aes(x = frequency))+
  geom_bar(fill = "#4E2A84")+
  labs(x="")+ 
  theme(text = element_text(size = 15),
        axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 12))

event_types<-separate_rows(survey, event_type, sep = ",")%>%
  select(response_id, event_type)

event_types%>%
  filter(!is.na(event_type))%>%
  ggplot(aes(x = fct_infreq(event_type)))+
  geom_bar(fill = "#4E2A84")+
  labs(x="")+ 
  theme(text = element_text(size = 15),
        axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 12))


formats<-separate_rows(survey, format, sep = ",")%>%
  select(response_id, format)

formats%>%
  filter(!is.na(format))%>%
  ggplot(aes(x = fct_infreq(format)))+
  geom_bar(fill = "#4E2A84")+
  labs(x="")+ 
  theme(text = element_text(size = 15),
    axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 12))

motivations<-separate_rows(survey, motivation, sep = ",")%>%
  select(response_id, motivation)

motivations%>%
  filter(!is.na(motivation))%>%
  mutate(motivation = fct_recode(motivation, Socializing = "I know other people who are going"), 
         motivation = fct_recode(motivation, Topic = "An interesting topic"), 
         motivation = fct_recode(motivation, Presenter = "A good presenter"))%>%
  ggplot(aes(x = fct_infreq(motivation)))+
  geom_bar(fill = "#4E2A84")+
  labs(x="")+ 
  theme(text = element_text(size = 15),
        axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 12))


#####33
lunch_and_learn<-event_types%>%
  filter(event_type=="Lunch and Learn")

lunch_and_learn_format<- formats %>%
  filter(response_id %in% lunch_and_learn$response_id)

lunch_and_learn_format%>%
  count(format)%>%
  arrange(desc(n))