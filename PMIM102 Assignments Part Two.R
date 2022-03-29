# PMIM102 Assignment Part 2
# By Harrison Omonhinmin 2140714
# This code is to give provide an open-ended analysis - January 2022

#------------------------------------------------------------------------------

# Install and load tidyverse for data manipulation
library(tidyverse)

# Install and load testthat
install.packages("testthat")
library(testthat)

# Install RPostgreSQL package to gain access to PostGreSQL
install.packages("RPostgreSQL");

# If installed and available, load
require("RPostgreSQL");

# Select driver for database connection to SQL
drv = dbDriver("PostgreSQL");

# Connect to the database to run SQL codes
con <- dbConnect(drv, dbname = "gp_practice_data", 
                 host = "localhost", port = 5432,
                 user = "postgres", password = rstudioapi::askForPassword())

# Check all tables in the database
dbListTables(con)

# Examine all tables and assign them
address <- dbGetQuery(con, "
           select column_name, 
	         ordinal_position,
           data_type,
           character_maximum_length,
           numeric_precision
           from INFORMATION_SCHEMA.COLUMNS
           where table_schema = 'public'
           and table_name = 'address';")

bnf <- dbGetQuery(con, "
           select column_name, 
	         ordinal_position,
           data_type,
           character_maximum_length,
           numeric_precision
           from INFORMATION_SCHEMA.COLUMNS
           where table_schema = 'public'
           and table_name = 'bnf';")

chemsubstance <- dbGetQuery(con, "
           select column_name, 
	         ordinal_position,
           data_type,
           character_maximum_length,
           numeric_precision
           from INFORMATION_SCHEMA.COLUMNS
           where table_schema = 'public'
           and table_name = 'chemsubstance';")

gp_data_up_to_2015 <- dbGetQuery(con, "
           select column_name, 
	         ordinal_position,
           data_type,
           character_maximum_length,
           numeric_precision
           from INFORMATION_SCHEMA.COLUMNS
           where table_schema = 'public'
           and table_name = 'gp_data_up_to_2015';")

qof_achievement <- dbGetQuery(con, "
           select column_name, 
	         ordinal_position,
           data_type,
           character_maximum_length,
           numeric_precision
           from INFORMATION_SCHEMA.COLUMNS
           where table_schema = 'public'
           and table_name = 'qof_achievement';")

qof_indicator <- dbGetQuery(con, "
           select column_name, 
	         ordinal_position,
           data_type,
           character_maximum_length,
           numeric_precision
           from INFORMATION_SCHEMA.COLUMNS
           where table_schema = 'public'
           and table_name = 'qof_indicator';")


## Part two
# Determine most prevalent medical condition category
MedCategory <- dbGetQuery(con, "select DISTINCT chapterdesc, 
          COUNT(chapterdesc) AS med_cat
          FROM bnf
          GROUP BY chapterdesc")

MedCategory

# Visualise Medical Condition Category
MedAnalysis <- ggplot(MedCategory, aes(x = med_cat, y = chapterdesc, 
            color = as.factor(chapterdesc))) +
            geom_point(size = 3)

# Add a title and labels to the visual
MedAnalysis <- (MedAnalysis + 
            ggtitle("
            Prevalence of Medical Condition Category
            "))
MedAnalysis <- (MedAnalysis + 
            labs(x = "Prevalence Count", y = "Medical
            Condition"))

MedAnalysis

# Most prevalent stoma appliances treatment delivered
StomaTreatment <- dbGetQuery(con, "
                select chapterdesc, chemicaldesc, 
                count(chemicaldesc) as treatment_count
                from bnf where chapterdesc like '%Stoma Appliances%'
                group by chapterdesc, chemicaldesc")

StomaTreatment

# Visualise Stoma Appliances Treatment
StomaTreatmentGraph <- ggplot(StomaTreatment, aes(x = treatment_count, 
                    y = chemicaldesc, 
                    color = as.factor(chemicaldesc))) +
                    geom_point(size = 3)

# Add a title and labels to the visualisation
StomaTreatmentGraph <- (StomaTreatmentGraph + 
                    ggtitle("
                    Prevalence of Stoma Appliances Treatment
                    "))
StomaTreatmentGraph <- (StomaTreatmentGraph + 
                    labs(x = "Prevalence Count", y = "Stoma Appliances
                    Treatment"))

StomaTreatmentGraph

# Examine Stoma Appliances treatment data in April 2013
TreatmentFreq <- dbGetQuery(con, "select count(items) 
              as items_count, period, bnfcode, bnfname
              from gp_data_up_to_2015 
              where bnfcode like '236%' and period='201304'
              group by period, bnfcode, bnfname")

TreatmentFreq

# Total prescribed Stoma Appliances treatment in April 2013
sum(TreatmentFreq$items_count)


# Stoma treatment by postcode
StomaTreatmentTable <- dbGetQuery(con, "select hb, bnfcode, bnfname
                    from gp_data_up_to_2015
                    where bnfcode like '236%'")

StomaTreatmentTable


# Stoma treatment distribution within each postcode
StomaTreatmentDist <- dbGetQuery(con, "select distinct count(hb) 
                          as hb_count, hb
                          from gp_data_up_to_2015
                          where bnfcode like '236%'
                          group by hb")

StomaTreatmentDist

# Visual distribution of Stoma Appliances Treatment
TreatmentDist <- ggplot(StomaTreatmentDist, aes(x = hb_count, 
              y = hb, color = as.factor(hb))) +
              geom_point(size = 3)

# Add a title and labels to the visualisation
TreatmentDist <- (TreatmentDist + 
             ggtitle("
          Prevalence of Stoma Appliances Treatment by different postcode
          "))
TreatmentDist <- (TreatmentDist + 
             labs(x = "Prevalence Count", y = "Postcode"))

TreatmentDist

# Stoma Appliances trend by period
StomaTrend <- dbGetQuery(con, "select count(period) as cases, period
            from gp_data_up_to_2015
            where bnfcode like '236%'
            group by period")

StomaTrend <- data.frame(StomaTrend)
StomaTrend

# Visualization of the trend of Stoma Appliance by period
StomaTrendPlot <- ggplot(StomaTrend, aes(period, cases, fill = period)) +
  geom_line(size = 1)

# Add a title and labels to the visualisation
StomaTrendPlot <- (StomaTrendPlot + 
             ggtitle("Trend of Stoma Appliances"))
StomaTrendPlot <- (StomaTrendPlot + 
             labs(x = "Period", y = "Growth"))


StomaTrendPlot
   

# Close the connection and unload RPostGreSQL drivers.
dbDisconnect(con)
dbUnloadDriver(drv) 

cat('\nEnd of analysis. Thank you for using Harrison\'s code. 
    PMIM102 January 2022\n')