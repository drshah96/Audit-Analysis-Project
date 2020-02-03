# In USA or OUSA

library("readxl")
library("pacman")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, party, rio, tidyverse)

# CSQA Audit Dataset

CSQA_USAstatus_raw <- import("Data/CSQA_Audit_Data(1).xlsx") %>%
    as_tibble %>%
    select("In USA or OUS")

table(CSQA_USAstatus_raw)

CSQA_USAstatus <- table(CSQA_USAstatus_raw$`In USA or OUS`)
CSQA_USAstatus <- as.data.frame(CSQA_USAstatus)

names(CSQA_USAstatus) <- c("Status","Frequency")

CSQA_USAstatus$Percent <- CSQA_USAstatus$Frequency/sum(CSQA_USAstatus$Frequency) * 100
CSQA_USAstatus

CSQA_USAstatus_pie <- data.frame(status = CSQA_USAstatus$Status, frequency = CSQA_USAstatus$Frequency, percent = round(CSQA_USAstatus$Percent,2))
CSQA_USAstatus_pie

# Frequency Pie Chart
CSQA_USAstatus_pie_frequency <- CSQA_USAstatus_pie %>%
    arrange(desc(status)) %>%
    mutate(ypos = cumsum(frequency) - 0.5*frequency)
CSQA_USAstatus_pie_frequency

ggplot(CSQA_USAstatus_pie_frequency, aes(x = "status", y = frequency, fill = status)) +
    geom_bar(width = 2, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(y = ypos, label = frequency), color = "black") +
    scale_fill_brewer(palette = "GnBu") + theme_minimal()

#Percentage Bar Chart
CSQA_USAstatus_pie_percent <- CSQA_USAstatus_pie %>%
    arrange(desc(status)) %>%
    mutate(ypos_percent = cumsum(percent) - 0.5*percent)
CSQA_USAstatus_pie_percent

ggplot(CSQA_USAstatus_pie_percent, aes(x = "status", y = percent, fill = status)) +
    geom_bar(width = 2, stat = "identity", color = "white") +
    #coord_polar("y", start = 0) +
    geom_text(aes(y = ypos_percent, label = percent), color = "black") +
    scale_fill_brewer(palette="OrRd") + theme_minimal()

# GXP Audit Dataset

GXP_USAstatus_raw <- import("Data/GXP_Audit_Data(1).xlsx") %>%
    as_tibble %>%
    select("In.USA.or.OUS")

table(GXP_USAstatus_raw)

GXP_USAstatus <- table(GXP_USAstatus_raw$In.USA.or.OUS)
GXP_USAstatus <- as.data.frame(GXP_USAstatus)

names(GXP_USAstatus) <- c("Status","Frequency")

GXP_USAstatus$Percent <- GXP_USAstatus$Frequency/sum(GXP_USAstatus$Frequency) * 100
GXP_USAstatus

GXP_USAstatus_pie <- data.frame(status = GXP_USAstatus$Status, frequency = GXP_USAstatus$Frequency, percent = round(GXP_USAstatus$Percent,2))
GXP_USAstatus_pie

# Frequency Pie Chart
GXP_USAstatus_pie_frequency <- GXP_USAstatus_pie %>%
    arrange(desc(status)) %>%
    mutate(ypos = cumsum(frequency) - 0.5*frequency)
GXP_USAstatus_pie_frequency

ggplot(GXP_USAstatus_pie_frequency, aes(x = "status", y = frequency, fill = status)) +
    geom_bar(width = 2, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(y = ypos, label = frequency), color = "black") +
    scale_fill_brewer(palette = "GnBu") + theme_minimal()

#Percentage Bar Chart
GXP_USAstatus_pie_percent <- GXP_USAstatus_pie %>%
    arrange(desc(status)) %>%
    mutate(ypos_percent = cumsum(percent) - 0.5*percent)
GXP_USAstatus_pie_percent

ggplot(GXP_USAstatus_pie_percent, aes(x = "status", y = percent, fill = status)) +
    geom_bar(width = 2, stat = "identity", color = "white") +
    #coord_polar("y", start = 0) +
    geom_text(aes(y = ypos_percent, label = percent), color = "black") +
    scale_fill_brewer(palette="OrRd") + theme_minimal()
