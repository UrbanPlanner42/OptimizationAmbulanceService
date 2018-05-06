library(lubridate) 
library(ggplot2)
library(dplyr, warn.conflicts = TRUE)
library(shiny)
library(shinydashboard)


#Load Data
incident<-read.csv("~/Amazon Drive/New_Orleans/Shiny/Data/all_Incidents.csv",header = TRUE,stringsAsFactors = FALSE)

# #Earl B Function 
erlb <- function(nsrv, nerl, log=FALSE) {
  # Returns Erlang-B blockage for nsrv servers and nerl erlangs of traffic.
  # If log=TRUE, then logarithm of the blockage is returned.  Works for all
  # nsrv>=0 and all nerl>=0.  Both nsrv and nerl can be non-integers.
  
  lnerlb<-
    dgamma(nerl, shape=nsrv+1, scale=1, log=TRUE) -
    pgamma(nerl, shape=nsrv+1, scale=1, log.p=TRUE, lower.tail=FALSE) ;
  if (log) lnerlb else exp(lnerlb)
}

#Function to USE
elf <- function(n, a) { 
  b <- 1 ; i<- 0 
  while((i <- i+1)<=n) b <- b*a/(i+b*a) 
  b 
} 



#X is the Data Frame for 911 Data
#Y is the month
#C numbers of ambulances
#Z Begin of the Shift
#E End of the Shift 

#City Wide Function 

nua_schedule <- function(x, c, y, d, z, e) {
  #Converting to Time Format Time in R
  x$IncidentDate_Time <- ymd_hms(x$IncidentDate) #Get R compatible date
  x$Year <- year(x$IncidentDate_Time)
  x$Month <- month(x$IncidentDate_Time) #Day of the month
  x$DoW <- weekdays(x$IncidentDate_Time) #Get day of week
  x$IncidentDate_Date <- date(x$IncidentDate_Time) #Strip date from full format
  x$HoD <- hour(x$IncidentDate_Time)
  #Cleaning Dispatch Time and Closed Time
  x$BeginCase_Time <- ymd_hms(x$DispatchedTime)
  x$CloseCase_Time <- ymd_hms(x$ClosedTime)
  x$How_Long_Case <- (x$CloseCase_Time - x$BeginCase_Time)/60 #Get closed cases in Minute 
  
  #Summarize by Year
  gp1 <- group_by(x, Year, Month, DoW, HoD, How_Long_Case)
  gp1 <- gp1 %>%
    select(Year, Month, DoW, HoD, How_Long_Case) %>%
    group_by(Year, Month, DoW, HoD) %>%
    summarise_all(funs(Count_Cases = n(),
                       Average_CaseTime = mean(How_Long_Case, na.rm = TRUE)))
  
  
  #Summarize by Hour, Days, and Month
  gp2 <- group_by(gp1, Month, DoW, HoD, Count_Cases, Average_CaseTime)
  
  gp2 <- gp2 %>%
    group_by(Month, DoW, HoD) %>%
    summarise_each_(vars = c("Count_Cases", "Average_CaseTime"), 
                    funs = funs(Median="median"))
  
  gp2$ValueA_Erlag <- (gp2$Average_CaseTime_Median * gp2$Count_Cases_Median)/60 #Get the A in Erlag B formula
  gp2$Numbers_Ambulance <- c
  
  gp2 <- gp2[gp2$Month == y, ] #Month
  gp2 <- gp2[gp2$DoW == d, ] #Day
  gp2 <- gp2[gp2$HoD >= z & gp2$HoD <= e, ]  #Time of the shift
  
  #ErlagB <- mapply(elf, gp2$Numbers_Ambulance, gp2$ValueA_Erlag) #Run the function for ErlagB 
  ErlagB <- mapply(erlb, gp2$Numbers_Ambulance, gp2$ValueA_Erlag) #Run the function for ErlagB 
  gp2$Probability_NUA <- as.data.frame(ErlagB)
  gp2$ValueA_Erlag <- NULL #Eliminate Value A from the R Shiny Visualization
  
  return(as.data.frame(gp2))
}


month<-seq(from=1,to=12)
weekdays<-weekdays(Sys.Date()+0:6)
