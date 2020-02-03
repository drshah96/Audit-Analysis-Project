#GxP Area

library("readxl")
library("pacman")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, party, rio, tidyverse)

# CSQA Audit Dataset

CSQA_GxParea_raw <- import("Data/CSQA_Audit_Data(1).xlsx") %>%
    as_tibble %>%
    select("GxP Area")

table(CSQA_GxParea_raw)

CSQA_GxParea <- table(CSQA_GxParea_raw$`GxP Area`)
CSQA_GxParea <- as.data.frame(CSQA_GxParea)

names(CSQA_GxParea) <- c("Status","Frequency")

CSQA_GxParea$Percent <- CSQA_GxParea$Frequency/sum(CSQA_GxParea$Frequency) * 100
CSQA_GxParea

CSQA_GxParea_pie <- data.frame(status = CSQA_GxParea$Status, frequency = CSQA_GxParea$Frequency, percent = round(CSQA_GxParea$Percent,2))
CSQA_GxParea_pie

# Frequency Pie Chart
CSQA_GxParea_pie_frequency <- CSQA_GxParea_pie %>%
    arrange(desc(status)) %>%
    mutate(ypos = cumsum(frequency) - 0.5*frequency)
CSQA_GxParea_pie_frequency

ggplot(CSQA_GxParea_pie_frequency, aes(x = "status", y = frequency, fill = status)) +
    geom_bar(width = 2, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(y = ypos, label = frequency), color = "black") +
    scale_fill_brewer(palette = "GnBu") + theme_minimal()

#Percentage Bar Chart
CSQA_GxParea_pie_percent <- CSQA_GxParea_pie %>%
    arrange(desc(status)) %>%
    mutate(ypos_percent = cumsum(percent) - 0.5*percent)
CSQA_GxParea_pie_percent

ggplot(CSQA_GxParea_pie_percent, aes(x = "status", y = percent, fill = status)) +
    geom_bar(width = 2, stat = "identity", color = "white") +
    #coord_polar("y", start = 0) +
    geom_text(aes(y = ypos_percent, label = percent), color = "black") +
    scale_fill_brewer(palette="OrRd") + theme_minimal()

# GXP Audit Dataset

GXP_GxParea_raw <- import("Data/GXP_Audit_Data(1).xlsx") %>%
    as_tibble %>%
    select("GxP.Area")

table(GXP_GxParea_raw)

GXP_GxParea <- table(GXP_GxParea_raw$GxP.Area)
GXP_GxParea <- as.data.frame(GXP_GxParea)

names(GXP_GxParea) <- c("Status","Frequency")

GXP_GxParea$Percent <- GXP_GxParea$Frequency/sum(GXP_GxParea$Frequency) * 100
GXP_GxParea

GXP_GxParea_pie <- data.frame(status = GXP_GxParea$Status, frequency = GXP_GxParea$Frequency, percent = round(GXP_GxParea$Percent,2))
GXP_GxParea_pie

# Frequency Pie Chart
GXP_GxParea_pie_frequency <- GXP_GxParea_pie %>%
    arrange(desc(status)) %>%
    mutate(ypos = cumsum(frequency) - 0.5*frequency)
GXP_GxParea_pie_frequency

ggplot(GXP_GxParea_pie_frequency, aes(x = "status", y = frequency, fill = status)) +
    geom_bar(width = 2, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(y = ypos, label = frequency), color = "black") +
    scale_fill_brewer(palette = "GnBu") + theme_minimal()

#Percentage Bar Chart
GXP_GxParea_pie_percent <- GXP_GxParea_pie %>%
    arrange(desc(status)) %>%
    mutate(ypos_percent = cumsum(percent) - 0.5*percent)
GXP_GxParea_pie_percent

ggplot(GXP_GxParea_pie_percent, aes(x = "status", y = percent, fill = status)) +
    geom_bar(width = 2, stat = "identity", color = "white") +
    #coord_polar("y", start = 0) +
    geom_text(aes(y = ypos_percent, label = percent), color = "black") +
    scale_fill_brewer(palette="OrRd") + theme_minimal()
