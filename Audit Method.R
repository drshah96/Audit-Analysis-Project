# Audit Method 

library("readxl")
library("pacman")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, party, rio, tidyverse)

#CSQA Audit Dataset

CSQA_auditmethod_raw <- import("Data/CSQA_Audit_Data(1).xlsx") %>%
    as_tibble %>%
    select("Audit Method")

table(CSQA_auditmethod_raw)

CSQA_auditmethod <- table(CSQA_auditmethod_raw$`Audit Method`)
CSQA_auditmethod <- as.data.frame(CSQA_auditmethod)

names(CSQA_auditmethod) <- c("Status","Frequency")

CSQA_auditmethod$Percent <- CSQA_auditmethod$Frequency/sum(CSQA_auditmethod$Frequency) * 100
CSQA_auditmethod

library(ggplot2)
library("dplyr")

CSQA_AuditMethod_pie <- data.frame(status = CSQA_auditmethod$Status, frequency = CSQA_auditmethod$Frequency, percent = round(CSQA_auditmethod$Percent,2))
CSQA_AuditMethod_pie

# Frequency Pie Chart
CSQA_AuditMethod_pie_frequency <- CSQA_AuditMethod_pie %>%
    arrange(desc(status)) %>%
    mutate(ypos = cumsum(frequency) - 0.5*frequency)
CSQA_AuditMethod_pie_frequency

ggplot(CSQA_AuditMethod_pie_frequency, aes(x = "status", y = frequency, fill = status)) +
    geom_bar(width = 2, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(y = ypos, label = frequency), color = "black") +
    scale_fill_brewer(palette = "GnBu") + theme_minimal()

#Percentage Bar Chart
CSQA_AuditMethod_pie_percent <- CSQA_AuditMethod_pie %>%
    arrange(desc(status)) %>%
    mutate(ypos_percent = cumsum(percent) - 0.5*percent)
CSQA_AuditMethod_pie_percent

ggplot(CSQA_AuditMethod_pie_percent, aes(x = "status", y = percent, fill = status)) +
    geom_bar(width = 2, stat = "identity", color = "white") +
    #coord_polar("y", start = 0) +
    geom_text(aes(y = ypos_percent, label = percent), color = "black") +
    scale_fill_brewer(palette="OrRd") + theme_minimal()


# GXP Audit Dataset

GXP_auditmethod_raw <- import("Data/GXP_Audit_Data(1).xlsx") %>%
    as_tibble %>%
    select("Audit.Method")

table(GXP_auditmethod_raw)

GXP_auditmethod <- table(GXP_auditmethod_raw$Audit.Method)
GXP_auditmethod <- as.data.frame(GXP_auditmethod)

names(GXP_auditmethod) <- c("Status","Frequency")

GXP_auditmethod$Percent <- GXP_auditmethod$Frequency/sum(GXP_auditmethod$Frequency) * 100
GXP_auditmethod

GXP_AuditMethod_pie <- data.frame(status = GXP_auditmethod$Status, frequency = GXP_auditmethod$Frequency, percent = round(GXP_auditmethod$Percent,2))
GXP_AuditMethod_pie

# Frequency Pie Chart
GXP_AuditMethod_pie_frequency <- GXP_AuditMethod_pie %>%
    arrange(desc(status)) %>%
    mutate(ypos = cumsum(frequency) - 0.5*frequency)
GXP_AuditMethod_pie_frequency

ggplot(GXP_AuditMethod_pie_frequency, aes(x = "status", y = frequency, fill = status)) +
    geom_bar(width = 2, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(y = ypos, label = frequency), color = "black") +
    scale_fill_brewer(palette = "GnBu") + theme_minimal()

#Percentage Bar Chart
GXP_AuditMethod_pie_percent <- GXP_AuditMethod_pie %>%
    arrange(desc(status)) %>%
    mutate(ypos_percent = cumsum(percent) - 0.5*percent)
GXP_AuditMethod_pie_percent

ggplot(GXP_AuditMethod_pie_percent, aes(x = "status", y = percent, fill = status)) +
    geom_bar(width = 2, stat = "identity", color = "white") +
    #coord_polar("y", start = 0) +
    geom_text(aes(y = ypos_percent, label = percent), color = "black") +
    scale_fill_brewer(palette="OrRd") + theme_minimal()
