######### ASSIGNMENT PART II ##########

library("readxl")
library("pacman")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, party, rio, tidyverse)

#CSQA Audit Dataset

CSQA_Audit <- import("Data/CSQA_Audit_Data(1).xlsx", col_types = c("text", "text", "text", 
                                                                   "text", "text", "text", "text", "text", 
                                                                   "text", "text", "text", "text", "numeric", 
                                                                   "date", "date", "date", "date", "date", 
                                                                   "date", "date", "date", "date", "date", 
                                                                   "numeric")) %>% as_tibble
    

CSQA_Date_Intake <- as.Date(CSQA_Audit$`Date of Intake`)
CSQA_Date_Qsent <- as.Date(CSQA_Audit$`Date Q sent`)
CSQA_Date_Qrecieved <- as.Date(CSQA_Audit$`Date Q Rcvd`)
CSQA_Date_OnSiteScheduled <- as.Date(CSQA_Audit$`Date On Site Scheduled`)
CSQA_Date_AuditStartDate <- as.Date(CSQA_Audit$`Audit Start Date`)
CSQA_Date_AuditEndDate <- as.Date(CSQA_Audit$`Audit End Date`)
CSQA_Date_FinalReportDue <- as.Date(CSQA_Audit$`Date Final Report Due`)
CSQA_Date_CompletionDate <- as.Date(CSQA_Audit$`Date of Completion`)

library(lubridate)
(CSQA_Days_Intake_QSent <- ymd(CSQA_Date_Qsent) - ymd(CSQA_Date_Intake))
(CSQA_Days_QSent_QReceived <- ymd(CSQA_Date_Qrecieved) - ymd(CSQA_Date_Qsent))
(CSQA_Days_OnSiteScheduled_AuditStartDate <- ymd(CSQA_Date_AuditStartDate) - ymd(CSQA_Date_OnSiteScheduled))
(CSQA_Days_StartDate_EndDate <- ymd(CSQA_Date_AuditEndDate) - ymd(CSQA_Date_AuditStartDate))
(CSQA_Days_AuditEnd_FinalReportDue <- ymd(CSQA_Date_FinalReportDue) - ymd(CSQA_Date_AuditEndDate))
(CSQA_Days_FinalReportDue_CompletionDate <- ymd(CSQA_Date_CompletionDate) - ymd(CSQA_Date_FinalReportDue))

class(as(CSQA_Days_FinalReportDue_CompletionDate,"numeric"))

(mean(CSQA_Days_FinalReportDue_CompletionDate,na.rm = TRUE))
(median(CSQA_Days_FinalReportDue_CompletionDate,na.rm = TRUE))


#GXP Audit Dataset

GXP_Audit <- import("Data/GXP_Audit_Data(1).xlsx", col_types = c("text", "text", "text", 
                                                                 "text", "text", "text", "text", "text", 
                                                                 "text", "text", "text", "text", "text", 
                                                                 "date", "date", "date", "date", "date", 
                                                                 "date", "date", "date", "text", "numeric")) %>% as_tibble

GXP_Date_Intake <- as.Date(GXP_Audit$Date.of.Intake)
GXP_Date_Qsent <- as.Date(GXP_Audit$Date.Q.sent)
GXP_Date_Qrecieved <- as.Date(GXP_Audit$Date.Q.Rcvd)
GXP_Date_OnSiteScheduled <- as.Date(GXP_Audit$Date.On.Site.Scheduled)
GXP_Date_AuditStartDate <- as.Date(GXP_Audit$Audit.Start.Date)
GXP_Date_AuditEndDate <- as.Date(GXP_Audit$Audit.End.Date)
GXP_Date_FinalReportDue <- as.Date(GXP_Audit$Date.Final..Report.Due)
GXP_Date_CompletionDate <- as.Date(GXP_Audit$Date.of..Completion)

(GXP_Days_Intake_QSent <- ymd(GXP_Date_Qsent) - ymd(GXP_Date_Intake))
(GXP_Days_QSent_QReceived <- ymd(GXP_Date_Qrecieved) - ymd(GXP_Date_Qsent))
(GXP_Days_OnSiteScheduled_AuditStartDate <- ymd(GXP_Date_AuditStartDate) - ymd(GXP_Date_OnSiteScheduled))
(GXP_Days_StartDate_EndDate <- ymd(GXP_Date_AuditEndDate) - ymd(GXP_Date_AuditStartDate))
(GXP_Days_AuditEnd_FinalReportDue <- ymd(GXP_Date_FinalReportDue) - ymd(GXP_Date_AuditEndDate))
(GXP_Days_FinalReportDue_CompletionDate <- ymd(GXP_Date_CompletionDate) - ymd(GXP_Date_FinalReportDue))

