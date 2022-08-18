library(readxl)
library(dplyr)
library(ggplot2)

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
                                "Response ID",
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
  select("motivation",
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

survey%>%
  count(format, event_type)
                  
topics<-read_xlsx("data/S&S Staff Engagement Survey_August 17, 2022_12.12.xlsx", 
                sheet = "topics")%>%
  filter(!is.na(Category))

topics%>%
  count(Category)%>%
  arrange(desc(n))
