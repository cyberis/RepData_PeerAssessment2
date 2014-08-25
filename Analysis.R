# Analysis.R - Throw away script with all of the code to carry out the analysis
# Reproducible Research Section 5 - Dr. Roger Peng
# August 4 - September 1, 2014
# Created by Christopher Bortz

# Step 0 - Setup our environment
library(R.utils)
library(data.table)
library(lubridate)
library(plyr)
library(ggplot2)
library(reports)

# Step 1 - Get the Data if we don't already have it
if(!file.exists("./data")) {
    dir.create("./data")
}
if(!file.exists("./data/StormData.csv.bz2") & !file.exists("./data/StormData.csv")) {
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
                  "./data/StormData.csv.bz2",
                  method = "curl")
    if(!file.exists("./data/StormData.csv")) {
        bunzip2("./data/StormData.csv.bz2")
    }
}
# This is National Weather Service Instruction 10-1605, Dated August 17, 2007
# Source URL: http://www.ncdc.noaa.gov/stormevents/pd01016005curr.pdf
if(!file.exists("./data/NWS_Doc.pdf")) {
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf",
                  "./data/NWS_Doc.pdf",
                  method = "curl")
}
if(!file.exists("./data/NWS_FAQ.pdf")) {
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf",
                  "./data/NWS_FAQ.pdf",
                  method = "curl")
}

# We want to use fread to create a data.table but first we
# need to read the file in using read.csv and then subset and write out the subset
if(!file.exists("./data/StormDamage.csv")) {
    data <- read.csv("./data/StormData.csv")
    data <- data[, c("BGN_DATE",
                     "EVTYPE",
                     "FATALITIES",
                     "INJURIES",
                     "PROPDMG",
                     "PROPDMGEXP",
                     "CROPDMG",
                     "CROPDMGEXP")]
    write.csv(data,
              "./data/StormDamage.csv",
              row.names = FALSE)
    rm(data)
}

# Step 2 - Load in the data
data <- fread("./data/StormDamage.csv")

# Lets make all of the column names lowercase
setnames(data, colnames(data), tolower(colnames(data)))

# Step 3 - Clean up the data
data$bgn_date <- mdy_hms(data$bgn_date)

# Let's just add some additional columns to include year,
# a health impact ranking, and a total damage amount
data[, year := year(bgn_date)]

# In order to develop a health impact score I weighted
# the fatalities 20 times more impactful than an injury.
# A report from the National Safety Council describes the cost impact
# of auto accidents. 
# Source: http://www.nsc.org/news_resources/injury_and_death_statistics/Pages/EstimatingtheCostsofUnintentionalInjuries.aspx
# The average financial impact of a fatality was $1,410,000 vs $78,900 for a disabling injury.
# That's roughly a 20:1 financial impact so I went with that for my scoring.
data[, health := fatalities * 20 + injuries]

# This will normalize damage estimate and exponent
scaleData <- function(mantissa, exponent) {
    if(suppressWarnings(!is.na(as.numeric(exponent))))
    {
        expn = as.numeric(exponent)
    } else {
        expn <- switch(exponent,
                       H = , h = 2,
                       K = , k = 3,
                       M = , m = 6,
                       B = , b = 9,
                       0)
    }
    return(mantissa * 10^expn)
}
scaleDataVector = Vectorize(scaleData)

# Now create a damage score that combines property and crop damage
data[, damage := scaleDataVector(propdmg, propdmgexp) + scaleDataVector(cropdmg, cropdmgexp)]

# The event type data is a mess. Let's create a new field that we can fix up as we have time
suppressWarnings(data[, eventType := CA(evtype)]) # All events are now in Title case.

# Lets Remove Summary Events
data <- data[!grepl("^Summary", data$eventType, ignore.case = TRUE), ]

# Step 4 - Lets plot the highest health impact events
healthImpact <- aggregate(health ~ eventType, data, sum)
healthImpact <- arrange(healthImpact, desc(health))
healthImpact$eventType <- factor(healthImpact$eventType, healthImpact$eventType) # Make plot by impact
healthImpact <- head(healthImpact, 30)

ggplot(data = healthImpact,
       aes(x = eventType, y = health )) + 
    geom_bar(stat = "identity", fill = "red") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    scale_y_log10(expand = waiver()) +
    ylab("Health Impact (Fatalities to Injured 20:1)") + 
    xlab("Event Type") + 
    ggtitle("Health Impact from Top 30 Weather Events (1950-2011)")

# Step 5 - Lets plot the highest damage impact events
damageImpact <- aggregate(damage ~ eventType, data, sum)
damageImpact <- arrange(damageImpact, desc(damage))
damageImpact$eventType <- factor(damageImpact$eventType, damageImpact$eventType) # Make plot by impact
damageImpact <- head(damageImpact, 30)

ggplot(data = damageImpact,
       aes(x = eventType, y = damage )) + 
    geom_bar(stat = "identity", fill = "red") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    scale_y_log10(expand = waiver()) +
    ylab("Property Damage Impact (US Dollars)") + 
    xlab("Event Type") + 
    ggtitle("Property Damage from Top 30 Weather Events (1950-2011)")

# Done