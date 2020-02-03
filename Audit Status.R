# Audit Status

library("readxl")
library("pacman")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, party, rio, tidyverse)

# CSQA Audits Dataset
CSQA_auditstatus_raw <- import("Data/CSQA_Audit_Data(1).xlsx") %>%
    as_tibble %>%
    select("Audit Status")

table(CSQA_auditstatus_raw)

CSQA_auditstatus <- table(CSQA_auditstatus_raw$`Audit Status`)
CSQA_auditstatus <- as.data.frame(CSQA_auditstatus)

names(CSQA_auditstatus) <- c("Status","Frequency")

CSQA_auditstatus$Percent <- CSQA_auditstatus$Frequency/sum(CSQA_auditstatus$Frequency) * 100
CSQA_auditstatus

library(ggplot2)
library("dplyr")

CSQA_AuditStatus_pie <- data.frame(status = CSQA_auditstatus$Status, frequency = CSQA_auditstatus$Frequency, percent = round(CSQA_auditstatus$Percent,2))
CSQA_AuditStatus_pie

# Frequency Pie Chart
CSQA_AuditStatus_pie_frequency <- CSQA_AuditStatus_pie %>%
    arrange(desc(status)) %>%
    mutate(ypos = cumsum(frequency) - 0.5*frequency)
CSQA_AuditStatus_pie

ggplot(CSQA_AuditStatus_pie_frequency, aes(x = "status", y = frequency, fill = status)) +
    ggtitle("CSQA Audit Status Frequency Pie Chart") +
    geom_bar(width = 2, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(y = ypos, label = frequency), color = "black") +
    scale_fill_brewer(palette = "GnBu") + theme_minimal()

#Percentage Bar Chart
CSQA_AuditStatus_pie_percent <- CSQA_AuditStatus_pie %>%
    arrange(desc(status)) %>%
    mutate(ypos_percent = cumsum(percent) - 0.5*percent)
CSQA_AuditStatus_pie

ggplot(CSQA_AuditStatus_pie_percent, aes(x = "status", y = percent, fill = status)) +
    geom_bar(width = 2, stat = "identity", color = "white") +
    #coord_polar("y", start = 0) +
    geom_text(aes(y = ypos_percent, label = percent), color = "black") +
    scale_fill_brewer(palette="OrRd") + theme_minimal()


# GXP Audits Dataset

GXP_auditstatus_raw <- import("Data/GXP_Audit_Data(1).xlsx") %>%
    as_tibble %>%
    select("Audit.Status")

table(GXP_auditstatus_raw)

GXP_auditstatus <- table(GXP_auditstatus_raw$Audit.Status)
GXP_auditstatus <- as.data.frame(GXP_auditstatus)

names(GXP_auditstatus) <- c("Status","Frequency")

GXP_auditstatus$Percent <- GXP_auditstatus$Frequency/sum(GXP_auditstatus$Frequency) * 100
GXP_auditstatus

GXP_pie <- data.frame(status = GXP_auditstatus$Status, frequency = GXP_auditstatus$Frequency, percent = round(GXP_auditstatus$Percent,2))
GXP_pie 

# Frequency Pie Chart
GXP_AuditStatus_pie_frequency <- GXP_pie %>%
    arrange(desc(status)) %>%
    mutate(ypos = cumsum(frequency) - 0.5*frequency)
GXP_AuditStatus_pie_frequency

ggplot(GXP_AuditStatus_pie_frequency, aes(x = "status", y = frequency, fill = status)) +
    geom_bar(width = 2, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(y = ypos, label = frequency), color = "black") +
    scale_fill_brewer(palette="GnBu") + theme_minimal()

#Percentage Bar Chart
GXP_AuditStatus_pie_percent <- GXP_pie %>%
    arrange(desc(status)) %>%
    mutate(ypos_percent = cumsum(percent) - 0.5*percent)
GXP_AuditStatus_pie_percent

ggplot(GXP_AuditStatus_pie_percent, aes(x = "status", y = percent, fill = status)) +
    geom_bar(width = 2, stat = "identity", color = "white") +
    #coord_polar("y", start = 0) +
    geom_text(aes(y = ypos_percent, label = percent), color = "black") +
    scale_fill_brewer(palette="OrRd") + theme_minimal()
