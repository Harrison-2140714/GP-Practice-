# PMIM102 Assignment Part 1
# By Harrison Omonhinmin 2140714
# This code is to give the solution of the assessment - January 2022

#------------------------------------------------------------------------------

# Install and load tidyverse for data manipulation
library(tidyverse)

# Install and load testthat
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


#------------------------------------------------------------------------------


# Use dbgetquery to query codes from SQL

# Solution 1
# Allow user to make input and select practiceid
PracticeId <- dbGetQuery(con, "SELECT postcode, practiceid, 
           street, area, posttown, county 
           FROM address WHERE practiceid='W92015'")

PracticeId

# Response message after inputing practiceid
CorrectId <- FALSE

UserPracticeId <- readline(prompt = "Please enter your practice id (Wxxxxx):
                           ")
if (grepl('^W[0-9]{5}$', UserPracticeId)){
  CorrectId <- TRUE
} else {cat('\nThe entered Practice ID (', UserPracticeId,')
            does not exist, kindly try again.\n', sep='')
}
if (!CorrectId){
  stop('\nHalting: No data found because the Practice ID entered
       does not exist.')
}
  
  
#------------------------------------------------------------------------------ 

# Solution 1(i)  
# Check if selected practice has medication data available
MedAtPractice <- dbGetQuery(con, "select practiceid, bnfcode, bnfname
              from gp_data_up_to_2015
              where practiceid='W92015'
              group by practiceid, bnfcode, bnfname")

MedAtPractice

# Solution 1(ii)
# Check if selected practice has QOF data available
QofAtPractice <- dbGetQuery(con, "select field4, orgcode 
           FROM qof_achievement WHERE orgcode='W92015'")

QofAtPractice

# Solution 1(iii) (a)
# Number of patients at selected practice
PatientsAtPractice <- dbGetQuery(con, "select SUM(field4) 
                  AS patients_at_practice 
                  FROM qof_achievement WHERE orgcode='W92015'")

PatientsAtPractice
  
# Solution 1(iii) (b)       
# Average spend per month
AvgSpend <- dbGetQuery(con, "select sum(actcost) / sum(items) as average_spend
from gp_data_up_to_2015")

AvgSpend


# Solution 1(iii) (c)
# Create a visualisation showing the spend on medication per patient, compared 
# to other practices within the postcode area (i.e. first part of postcode).

# Medication Spend per patient in all practices within same postcode (hb)
MedSpend <- dbGetQuery(con, "select sum(actcost) / 
                sum(items) as average_spend, hb, practiceid
                from gp_data_up_to_2015
                where hb='7A2'
                group by practiceid, hb")

MedSpend
MedSpend <- data.frame(MedSpend)
# Visuals
MedSpendPlot <- ggplot(MedSpend, aes(x = average_spend,
                  y = practiceid, fill = practiceid)) +
                  geom_col() +
                  scale_fill_manual("Practice Id", values = c("W92015" = "purple"))

# Add a title and labels 
MedSpendPlot <- (MedSpendPlot + 
                ggtitle("Comparison of Average Spend on Medication in practice W92015 to 
                other practices in same postcode"))

MedSpendPlot <- (MedSpendPlot + labs(x = "Average Spend",
                y = "Practice ID"))

MedSpendPlot


# Solution(iv) (d)
# Report the rate of diabetes at the practice, and visualize  
# how this compares to other practices in Wales

# Query and examine Diabetes data
dbGetQuery(con, "select indicator, area 
           FROM qof_indicator WHERE area='Diabetes Mellitus'")

# Diabetes information at selected practice in Wales
DmTablePractice <- dbGetQuery(con, "select SUM(numerator) AS sum_numerator,
                    SUM(field4) AS sum_field
                    from qof_achievement
                    where indicator like 'DM%' 
                    AND orgcode like 'W92015'")

DmTablePractice

# Rate of diabetes at selected practice in Wales
DmRatePractice <- (DmTablePractice$sum_numerator/
                  DmTablePractice$sum_field) * 100 / 1

print(DmRatePractice)

# Diabetes information at other practices in Wales
# Number of Diabetic patients at other practices
DmPatientOtherPractice <- dbGetQuery(con, "select SUM(field4) 
                      AS field_sum
                      from qof_achievement
                      where indicator like 'DM%' 
                      AND orgcode not like 'W92015'")

DmPatientOtherPractice

# Number of Diabetes cases at other practices
DmCasesOtherPractice <- dbGetQuery(con, "select SUM(numerator) 
                      AS numerator_sum
                      from qof_achievement
                      where indicator like 'DM%' 
                      AND orgcode not like 'W92015'")

DmCasesOtherPractice

# Bind Diabetes report (patients with dm and dm cases) together
DmTableOtherPractice <- cbind(DmPatientOtherPractice, DmCasesOtherPractice)

DmTableOtherPractice

# Rate of diabetes at other practices in Wales
DmRateOtherPractices <- (DmTableOtherPractice$numerator_sum/
                           DmTableOtherPractice$field_sum) * 100 / 1

DmRateOtherPractices

# Visualisation of Diabetes at practice to other practices in Wales

DmTable <- rbind(DmRateOtherPractices, DmRatePractice)

# Rename column for proper visualization and turn to data frame
colnames(DmTable)[1] <- 'Dm_Rates'


# Create new column list containing Diabetes by practice level
Dm_Types <- c('DmRateOtherPractices', 'DmRatePractices')

# Turn DmTable to a data frame
DmTable <- data.frame(DmTable)

DmTable$Dm_Types <- Dm_Types

DmTable <- mutate(DmTable, Dm_Types)

DmTable

# Visualize the rate of diabetes in selected practice comparing diabetes in
# other practices in Wales
DmPlot <- ggplot(DmTable, aes(Dm_Types, Dm_Rates, fill = Dm_Types)) +
          geom_col(size = 1)

# Add title and labels
DmPlot <- (DmPlot + 
          ggtitle("
          Rate of Diabetes in practice compared to
          \nother practices in Wales"))
DmPlot <- (DmPlot + 
          labs(x = "Diabetes in Practice", y = "Diabetes Rate"))


DmPlot

#-----------------------------------------------------------------------------

# Solution 2 
# Finally, perform an all-Wales analysis comparing the rate of diabetes 
# and the rate of insulin prescribing at a practice level.
# All wales diabetic patients and type analysis
WalesDm <- dbGetQuery(con, "select SUM(numerator) AS wales_dm, 
          SUM(field4) AS wales_patients
          from qof_achievement
          where indicator like 'DM%'")


WalesDm

WalesDmRate <- (WalesDm$wales_dm / WalesDm$wales_patients) * 100 / 1

WalesDmRate


# ----------------------------------------------------------------------------
# Solution 2(i)

# Insulin usage in selected practice
Insulin <- dbGetQuery(con, "select count(bnfname) as ins_count, 
            sum(items) as ins_sum
            from gp_data_up_to_2015 where bnfname 
            LIKE 'Ins %' and practiceid='W92015'
            group by practiceid")

Insulin

# Rate of insulin at selected practice
InsulinRate <- (Insulin$ins_count / Insulin$ins_sum) * 100 / 1

InsulinRate

# Diabetes and Insulin relationship
DmInsulin <- sqrt(WalesDmRate*InsulinRate)

DmInsulin


# Solution 2(ii)
# Finally, perform an all-Wales analysis comparing the rate of diabetes 
# and the rate of Metformin prescribing at a practice level.
# Metformin usage at selected practice
Metformin <- dbGetQuery(con, "select sum(items) as met_sum, 
          count(bnfname) as met_count
          from gp_data_up_to_2015 
          where bnfname LIKE 'Metf%' 
          and practiceid='W92015'
          group by practiceid")

Metformin

# Rate of metformin at selected practice
MetforminRate <- (Metformin$met_count / Metformin$met_sum) * 100 / 1

MetforminRate

# Diabetes and Metformin Relationship
DmMetformin <- sqrt(WalesDmRate*MetforminRate)

DmMetformin


#------------------------------------------------------------------------------

# Close the connection and unload RPostGreSQL drivers.
dbDisconnect(con)
dbUnloadDriver(drv)


cat('\nEnd of analysis. Thank you for using Harrison\'s code. 
    PMIM102 January 2022\n')

