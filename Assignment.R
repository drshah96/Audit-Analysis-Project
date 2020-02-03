######### ASSIGNMENT - AUDIT ANALYSIS #########
######### DHRUVIN RAKESH SHAH - NUID: 001087062 #########

######### PART - I #########

##### Variable 1 - Audit Status #####
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
    ggtitle("CSQA Audit Status Percentage Bar Chart") +
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

GXP_auditstatus_pie <- data.frame(status = GXP_auditstatus$Status, frequency = GXP_auditstatus$Frequency, percent = round(GXP_auditstatus$Percent,2))
GXP_auditstatus_pie

# Frequency Pie Chart
GXP_AuditStatus_pie_frequency <- GXP_auditstatus_pie %>%
    arrange(desc(status)) %>%
    mutate(ypos = cumsum(frequency) - 0.5*frequency)
GXP_AuditStatus_pie_frequency

ggplot(GXP_AuditStatus_pie_frequency, aes(x = "status", y = frequency, fill = status)) +
    ggtitle("GXP Audit Status Frequency Pie Chart") +
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
    ggtitle("GXP Audit Status Percentage Bar Chart") +
    geom_bar(width = 2, stat = "identity", color = "white") +
    #coord_polar("y", start = 0) +
    geom_text(aes(y = ypos_percent, label = percent), color = "black") +
    scale_fill_brewer(palette="OrRd") + theme_minimal()



##### Variable 2 - In USA or OUSA #####
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
    ggtitle("CSQA In USA or OUSA Status Frequency Pie Chart") +
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
    ggtitle("CSQA In USA or OUSA Status Percent Bar Chart") +
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
    ggtitle("GXP In USA or OUSA Status Frequency Pie Chart") +
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
    ggtitle("GXP In USA or OUSA Status Percent Bar Chart") +
    geom_bar(width = 2, stat = "identity", color = "white") +
    #coord_polar("y", start = 0) +
    geom_text(aes(y = ypos_percent, label = percent), color = "black") +
    scale_fill_brewer(palette="OrRd") + theme_minimal()



##### Variable 3 - GxP Area #####
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
    ggtitle("CSQA GxP Area Frequency Pie Chart") +
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
    ggtitle("CSQA GxP Area Percent Bar Chart") +
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
    ggtitle("GXP GxP Area Frequency Pie Chart") +
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
    ggtitle("GXP GxP Area Percent Bar Chart") +
    geom_bar(width = 2, stat = "identity", color = "white") +
    #coord_polar("y", start = 0) +
    geom_text(aes(y = ypos_percent, label = percent), color = "black") +
    scale_fill_brewer(palette="OrRd") + theme_minimal()



##### Variable 4 - Audit Type #####
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
    ggtitle("CSQA Audit Type Frequency Pie Chart") +
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
    ggtitle("CSQA Audit Type Percentage Bar Chart") +
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
    ggtitle("GXP Audit Type frequency Pie Chart") +
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
    ggtitle("GXP Audit Type Percentage Bar Chart") +
    geom_bar(width = 2, stat = "identity", color = "white") +
    #coord_polar("y", start = 0) +
    geom_text(aes(y = ypos_percent, label = percent), color = "black") +
    scale_fill_brewer(palette="OrRd") + theme_minimal()



##### Variable 5 - Audit Method #####
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
    ggtitle("CSQA Audit Method Frequency Pie Chart") +
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
    ggtitle("CSQA Audit Method Percentage Bar Chart") +
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
    ggtitle("GXP Audit Method Frequency Pie Chart") +
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
    ggtitle("GXP Audit Method Percentage Bar Chart") +
    geom_bar(width = 2, stat = "identity", color = "white") +
    #coord_polar("y", start = 0) +
    geom_text(aes(y = ypos_percent, label = percent), color = "black") +
    scale_fill_brewer(palette="OrRd") + theme_minimal()



##### Variable 6 - Propose Quarter #####
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
    ggtitle("CSQA Propose Quarter Frequency Pie Chart") +
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
    ggtitle("CSQA Propose Quarter Percentage Bar Chart") +
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
    ggtitle("GXP Propose Quarter Frequency Pie Chart") +
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
    ggtitle("GXP Propose Quarter Percent Bar Chart") +
    geom_bar(width = 2, stat = "identity", color = "white") +
    #coord_polar("y", start = 0) +
    geom_text(aes(y = ypos_percent, label = percent), color = "black") +
    scale_fill_brewer(palette="OrRd") + theme_minimal()





######### PART - II #########
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

(mean(CSQA_Days_Intake_QSent,na.rm = TRUE))
(mean(GXP_Days_Intake_QSent,na.rm = TRUE))
(median(CSQA_Days_Intake_QSent,na.rm = TRUE))
(median(GXP_Days_Intake_QSent,na.rm = TRUE))

(mean(CSQA_Days_QSent_QReceived,na.rm = TRUE))
(mean(GXP_Days_QSent_QReceived,na.rm = TRUE))
(median(CSQA_Days_QSent_QReceived,na.rm = TRUE))
(median(GXP_Days_QSent_QReceived,na.rm = TRUE))

(mean(CSQA_Days_OnSiteScheduled_AuditStartDate,na.rm = TRUE))
(mean(GXP_Days_OnSiteScheduled_AuditStartDate,na.rm = TRUE))
(median(CSQA_Days_OnSiteScheduled_AuditStartDate,na.rm = TRUE))
(median(GXP_Days_OnSiteScheduled_AuditStartDate,na.rm = TRUE))

(mean(CSQA_Days_StartDate_EndDate,na.rm = TRUE))
(mean(GXP_Days_StartDate_EndDate,na.rm = TRUE))
(median(CSQA_Days_StartDate_EndDate,na.rm = TRUE))
(median(GXP_Days_StartDate_EndDate,na.rm = TRUE))

(mean(CSQA_Days_AuditEnd_FinalReportDue,na.rm = TRUE))
(mean(GXP_Days_AuditEnd_FinalReportDue,na.rm = TRUE))
(median(CSQA_Days_AuditEnd_FinalReportDue,na.rm = TRUE))
(median(GXP_Days_AuditEnd_FinalReportDue,na.rm = TRUE))

(mean(CSQA_Days_FinalReportDue_CompletionDate,na.rm = TRUE))
(mean(GXP_Days_FinalReportDue_CompletionDate,na.rm = TRUE))
(median(CSQA_Days_FinalReportDue_CompletionDate,na.rm = TRUE))
(median(GXP_Days_FinalReportDue_CompletionDate,na.rm = TRUE))
