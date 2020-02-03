# Proposed Method

library("readxl")
library("pacman")
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, party, rio, tidyverse)

# CSQA Audit Dataset

CSQA_proposemethod_raw <- import("Data/CSQA_Audit_Data(1).xlsx") %>%
    as_tibble %>%
    select("Proposed Quarter")

table(CSQA_proposemethod_raw)

CSQA_proposemethod <- table(CSQA_proposemethod_raw$`Proposed Quarter`)
CSQA_proposemethod <- as.data.frame(CSQA_proposemethod)

names(CSQA_proposemethod) <- c("Status","Frequency")

CSQA_proposemethod$Percent <- CSQA_proposemethod$Frequency/sum(CSQA_proposemethod$Frequency) * 100
CSQA_proposemethod

CSQA_proposemethod_pie <- data.frame(status = CSQA_proposemethod$Status, frequency = CSQA_proposemethod$Frequency, percent = round(CSQA_proposemethod$Percent,2))
CSQA_proposemethod_pie

# Frequency Pie Chart
CSQA_proposemethod_pie_frequency <- CSQA_proposemethod_pie %>%
    arrange(desc(status)) %>%
    mutate(ypos = cumsum(frequency) - 0.5*frequency)
CSQA_proposemethod_pie_frequency

ggplot(CSQA_proposemethod_pie_frequency, aes(x = "status", y = frequency, fill = status)) +
    geom_bar(width = 2, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(y = ypos, label = frequency), color = "black") +
    scale_fill_brewer(palette = "GnBu") + theme_minimal()

#Percentage Bar Chart
CSQA_proposemethod_pie_percent <- CSQA_proposemethod_pie %>%
    arrange(desc(status)) %>%
    mutate(ypos_percent = cumsum(percent) - 0.5*percent)
CSQA_proposemethod_pie_percent

ggplot(CSQA_proposemethod_pie_percent, aes(x = "status", y = percent, fill = status)) +
    geom_bar(width = 2, stat = "identity", color = "white") +
    #coord_polar("y", start = 0) +
    geom_text(aes(y = ypos_percent, label = percent), color = "black") +
    scale_fill_brewer(palette="OrRd") + theme_minimal()

# GXP Audit Dataset

GXP_proposemethod_raw <- import("Data/GXP_Audit_Data(1).xlsx") %>%
    as_tibble %>%
    select("Proposed.Quarter")

table(GXP_proposemethod_raw)

GXP_proposemethod <- table(GXP_proposemethod_raw$Proposed.Quarter)
GXP_proposemethod <- as.data.frame(GXP_proposemethod)

names(GXP_proposemethod) <- c("Status","Frequency")

GXP_proposemethod$Percent <- GXP_proposemethod$Frequency/sum(GXP_proposemethod$Frequency) * 100
GXP_proposemethod

GXP_proposemethod_pie <- data.frame(status = GXP_proposemethod$Status, frequency = GXP_proposemethod$Frequency, percent = round(GXP_proposemethod$Percent,2))
GXP_proposemethod_pie

# Frequency Pie Chart
GXP_proposemethod_pie_frequency <- GXP_proposemethod_pie %>%
    arrange(desc(status)) %>%
    mutate(ypos = cumsum(frequency) - 0.5*frequency)
GXP_proposemethod_pie_frequency

ggplot(GXP_proposemethod_pie_frequency, aes(x = "status", y = frequency, fill = status)) +
    geom_bar(width = 2, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    geom_text(aes(y = ypos, label = frequency), color = "black") +
    scale_fill_brewer(palette = "GnBu") + theme_minimal()

#Percentage Bar Chart
GXP_proposemethod_pie_percent <- GXP_proposemethod_pie %>%
    arrange(desc(status)) %>%
    mutate(ypos_percent = cumsum(percent) - 0.5*percent)
GXP_proposemethod_pie_percent

ggplot(GXP_proposemethod_pie_percent, aes(x = "status", y = percent, fill = status)) +
    geom_bar(width = 2, stat = "identity", color = "white") +
    #coord_polar("y", start = 0) +
    geom_text(aes(y = ypos_percent, label = percent), color = "black") +
    scale_fill_brewer(palette="OrRd") + theme_minimal()
