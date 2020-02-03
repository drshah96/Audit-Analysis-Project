# Audit Type 

library("readxl")
library("pacman")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, party, rio, tidyverse)

# CSQA Audit Dataset

CSQA_audittype_raw <- import("Data/CSQA_Audit_Data(1).xlsx") %>%
    as_tibble %>%
    select("Audit Type")

table(CSQA_audittype_raw)

CSQA_audittype <- table(CSQA_audittype_raw$`Audit Type`)
CSQA_audittype <- as.data.frame(CSQA_audittype)

names(CSQA_audittype) <- c("Status","Frequency")

CSQA_audittype$Percent <- CSQA_audittype$Frequency/sum(CSQA_audittype$Frequency) * 100
CSQA_audittype

CSQA_AuditType_pie <- data.frame(status = CSQA_audittype$Status, frequency = CSQA_audittype$Frequency, percent = round(CSQA_audittype$Percent,2))
CSQA_AuditType_pie

# Frequency Pie Chart
CSQA_AuditType_pie_frequency <- CSQA_AuditType_pie %>%
    arrange(desc(status)) %>%
    mutate(ypos = cumsum(frequency) - 0.5*frequency)
CSQA_AuditType_pie_frequency

ggplot(CSQA_AuditType_pie_frequency, aes(x = "status", y = frequency, fill = status)) +
    geom_bar(width = 2, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(y = ypos, label = frequency), color = "black") +
    scale_fill_brewer(palette = "GnBu") + theme_minimal()

#Percentage Bar Chart
CSQA_AuditType_pie_percent <- CSQA_AuditType_pie %>%
    arrange(desc(status)) %>%
    mutate(ypos_percent = cumsum(percent) - 0.5*percent)
CSQA_AuditType_pie_percent

ggplot(CSQA_AuditType_pie_percent, aes(x = "status", y = percent, fill = status)) +
    geom_bar(width = 2, stat = "identity", color = "white") +
    #coord_polar("y", start = 0) +
    geom_text(aes(y = ypos_percent, label = percent), color = "black") +
    scale_fill_brewer(palette="OrRd") + theme_minimal()

# GXP Audit Dataset

GXP_audittype_raw <- import("Data/GXP_Audit_Data(1).xlsx") %>%
    as_tibble %>%
    select("Audit.Type")

table(GXP_audittype_raw)

GXP_audittype <- table(GXP_audittype_raw$Audit.Type)
GXP_audittype <- as.data.frame(GXP_audittype)

names(GXP_audittype) <- c("Status","Frequency")

GXP_audittype$Percent <- GXP_audittype$Frequency/sum(GXP_audittype$Frequency) * 100
GXP_audittype

GXP_AuditType_pie <- data.frame(status = GXP_audittype$Status, frequency = GXP_audittype$Frequency, percent = round(GXP_audittype$Percent,2))
GXP_AuditType_pie

# Frequency Pie Chart
GXP_AuditType_pie_frequency <- GXP_AuditType_pie %>%
    arrange(desc(status)) %>%
    mutate(ypos = cumsum(frequency) - 0.5*frequency)
GXP_AuditType_pie_frequency

ggplot(GXP_AuditType_pie_frequency, aes(x = "status", y = frequency, fill = status)) +
    geom_bar(width = 2, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(y = ypos, label = frequency), color = "black") +
    scale_fill_brewer(palette = "GnBu") + theme_minimal()

#Percentage Bar Chart
GXP_AuditType_pie_percent <- GXP_AuditType_pie %>%
    arrange(desc(status)) %>%
    mutate(ypos_percent = cumsum(percent) - 0.5*percent)
GXP_AuditType_pie_percent

ggplot(GXP_AuditType_pie_percent, aes(x = "status", y = percent, fill = status)) +
    geom_bar(width = 2, stat = "identity", color = "white") +
    #coord_polar("y", start = 0) +
    geom_text(aes(y = ypos_percent, label = percent), color = "black") +
    scale_fill_brewer(palette="OrRd") + theme_minimal()
    