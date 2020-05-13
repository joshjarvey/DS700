#Importing correct libraries:
library(readxl)
library(mice)

#Read in the data files:
  #for abbeville, specifying column types as numeric automatically changes text values to NA (which are missing values).
abbeville = read_excel("C:/Users/joshj/Documents/DS700-Intro to Data Science/Final Assignment/dataset.xlsx", sheet = "Abbeville, LA", col_types = c("numeric", "numeric", "numeric"))
  #renaming the column header Incoming Examinations to remove the space between the words. 
names(abbeville) = make.names(names(abbeville))  

  #for other locations, specifying column types helps read in properly
violet = read_excel("C:/Users/joshj/Documents/DS700-Intro to Data Science/Final Assignment/dataset.xlsx", sheet = "May-2007 Violet, LA", col_types = c("text", "text", "date", "text"))
neworleans = read_excel("C:/Users/joshj/Documents/DS700-Intro to Data Science/Final Assignment/dataset.xlsx", sheet = "May-2007 New Orleans, LA", col_types = c("text", "text", "date", "text"))
lafayette = read_excel("C:/Users/joshj/Documents/DS700-Intro to Data Science/Final Assignment/dataset.xlsx", sheet = "May-2007 Lafayette, LA", col_types = c("text", "text", "date", "text"))
batonrouge = read_excel("C:/Users/joshj/Documents/DS700-Intro to Data Science/Final Assignment/dataset.xlsx", sheet = "May-2007 Baton Rouge, LA", col_types = c("text", "text", "date", "text"))
dec13 = read_excel("C:/Users/joshj/Documents/DS700-Intro to Data Science/Final Assignment/dataset.xlsx", sheet = "December 2013 Data")
heartcode = read_excel("C:/Users/joshj/Documents/DS700-Intro to Data Science/Final Assignment/dataset.xlsx", sheet = "Heart-related Condition Codes")

  #create a new dataframe for comparing examination reasons at other health clinics. Including "cur" pulmonale as well, since this was a mistype. 
heartexam = data.frame(code = c("Angina","Aortic Valve Stenosis","Arrhythmia","CAD","Cardiac","Cardiovascular","Chest Pain",
                                "coronary Artery Disease (CAD)","Cur Pulmonale","Cor Pulmonale","Endocarditis","Heart","Heart Palpitations",
                                "Ischemic Heart Disease","Myocardial Infraction","Myocardial Ischemia","Myocarditis",
                                "Premature Ventricular Contraction","Stress Test","Ventricular Septal Defect (VSD)","VSD"),
                                stringsAsFactors = FALSE)



#Missing data issue:
  #histogram to see the distribution of the exams
hist(abbeville$Incoming.Examinations)
  #changing the remaining incoming exam values to NA where that field matches the string of nines issue
abbeville$Incoming.Examinations[which((abbeville$Incoming.Examinations %in% c("99999999","999999999")))] = NA
  #review histogram again
hist(abbeville$Incoming.Examinations)



#Date text-based formatting issue with various sheets:
  #adjusting text-based dates that were dropped during the read_excel() process.
  #all dates that were recoded as NA were in May of 2007 
  #none of the dates were May 2, 2007 (which is a significate date - renovation), therefore all NA's are set to 5-31-2007.
  #this will allow them to count within the month of May, but not affect any other conditional issues.
violet$Date[which(is.na(violet$Date))] = as.Date("2007-05-31")
lafayette$Date[which(is.na(lafayette$Date))] = as.Date("2007-05-31")
batonrouge$Date[which(is.na(batonrouge$Date))] = as.Date("2007-05-31")



#Renovation on 5/2/07 Problem:
  #this block of code finds the count of exams that were rerouted from Abbeville (due to renovation), on 5-2-2007, that were heart related.
  #it then takes the existing number for examinations on this date in Abbeville and adds to it the count from that location
  #note, need to use "as.numeric" because the data in abbeville dataset is char at this point. 
  #also note, need to convert location date "as.character" to compare to date value.
  #as a gut check, these are the values: violet=5. new orleans=0, lafayette=1, baton rouge=1. original=107 + 5 = 114 total.
abbeville$Incoming.Examinations[which((abbeville$Month == 5 & abbeville$Year == 2007))] = as.numeric(abbeville$Incoming.Examinations[which((abbeville$Month == 5 & abbeville$Year == 2007))]) + length(which(as.character(violet$Date) == "2007-05-02" & violet$`Original Hospital Location` == "Abbeville" & violet$Examination %in% heartexam$code))
abbeville$Incoming.Examinations[which((abbeville$Month == 5 & abbeville$Year == 2007))] = as.numeric(abbeville$Incoming.Examinations[which((abbeville$Month == 5 & abbeville$Year == 2007))]) + length(which(as.character(neworleans$Date) == "2007-05-02" & neworleans$`Original Hospital Location` == "Abbeville" & neworleans$Examination %in% heartexam$code))
abbeville$Incoming.Examinations[which((abbeville$Month == 5 & abbeville$Year == 2007))] = as.numeric(abbeville$Incoming.Examinations[which((abbeville$Month == 5 & abbeville$Year == 2007))]) + length(which(as.character(lafayette$Date) == "2007-05-02" & lafayette$`Original Hospital Location` == "Abbeville" & lafayette$Examination %in% heartexam$code))
abbeville$Incoming.Examinations[which((abbeville$Month == 5 & abbeville$Year == 2007))] = as.numeric(abbeville$Incoming.Examinations[which((abbeville$Month == 5 & abbeville$Year == 2007))]) + length(which(as.character(batonrouge$Date) == "2007-05-02" & batonrouge$`Original Hospital Location` == "Abbeville" & batonrouge$Examination %in% heartexam$code))



#Uncounted reroutes for May-July 2013:
  #for the month of May: extract the number of heart related exams that were rerouted from abbeville. 
  #add this count to the existing abbeville count.
  #repeat for each location.
  #as reference: violet=0, new orleans=297, lafayette=174, baton rouge=146. original=4730 + 617 = 5347 total.
abbeville$Incoming.Examinations[which((abbeville$Month == 5 & abbeville$Year == 2013))] = as.numeric(abbeville$Incoming.Examinations[which((abbeville$Month == 5 & abbeville$Year == 2013))]) + nrow(subset(violet, as.Date(Date) >=as.Date("2013-05-01") & as.Date(Date) <=as.Date("2013-05-31") & violet$Examination %in% heartexam$code & violet$`Original Hospital Location` == "Abbeville"))
abbeville$Incoming.Examinations[which((abbeville$Month == 5 & abbeville$Year == 2013))] = as.numeric(abbeville$Incoming.Examinations[which((abbeville$Month == 5 & abbeville$Year == 2013))]) + nrow(subset(neworleans, as.Date(Date) >=as.Date("2013-05-01") & as.Date(Date) <=as.Date("2013-05-31") & neworleans$Examination %in% heartexam$code & neworleans$`Original Hospital Location` == "Abbeville"))
abbeville$Incoming.Examinations[which((abbeville$Month == 5 & abbeville$Year == 2013))] = as.numeric(abbeville$Incoming.Examinations[which((abbeville$Month == 5 & abbeville$Year == 2013))]) + nrow(subset(lafayette, as.Date(Date) >=as.Date("2013-05-01") & as.Date(Date) <=as.Date("2013-05-31") & lafayette$Examination %in% heartexam$code & lafayette$`Original Hospital Location` == "Abbeville"))
abbeville$Incoming.Examinations[which((abbeville$Month == 5 & abbeville$Year == 2013))] = as.numeric(abbeville$Incoming.Examinations[which((abbeville$Month == 5 & abbeville$Year == 2013))]) + nrow(subset(batonrouge, as.Date(Date) >=as.Date("2013-05-01") & as.Date(Date) <=as.Date("2013-05-31") & batonrouge$Examination %in% heartexam$code & batonrouge$`Original Hospital Location` == "Abbeville"))

  #for the month of June: extract the number of heart related exams that were rerouted from abbeville. 
  #add this count to the existing abbeville count.
  #repeat for each location.
  #as reference: violet=0, new orleans=0, lafayette=19, baton rouge=4 original=4706 + 23 = 4729 total.
abbeville$Incoming.Examinations[which((abbeville$Month == 6 & abbeville$Year == 2013))] = as.numeric(abbeville$Incoming.Examinations[which((abbeville$Month == 6 & abbeville$Year == 2013))]) + nrow(subset(violet, as.Date(Date) >=as.Date("2013-06-01") & as.Date(Date) <=as.Date("2013-06-30") & violet$Examination %in% heartexam$code & violet$`Original Hospital Location` == "Abbeville"))
abbeville$Incoming.Examinations[which((abbeville$Month == 6 & abbeville$Year == 2013))] = as.numeric(abbeville$Incoming.Examinations[which((abbeville$Month == 6 & abbeville$Year == 2013))]) + nrow(subset(neworleans, as.Date(Date) >=as.Date("2013-06-01") & as.Date(Date) <=as.Date("2013-06-30") & neworleans$Examination %in% heartexam$code & neworleans$`Original Hospital Location` == "Abbeville"))
abbeville$Incoming.Examinations[which((abbeville$Month == 6 & abbeville$Year == 2013))] = as.numeric(abbeville$Incoming.Examinations[which((abbeville$Month == 6 & abbeville$Year == 2013))]) + nrow(subset(lafayette, as.Date(Date) >=as.Date("2013-06-01") & as.Date(Date) <=as.Date("2013-06-30") & lafayette$Examination %in% heartexam$code & lafayette$`Original Hospital Location` == "Abbeville"))
abbeville$Incoming.Examinations[which((abbeville$Month == 6 & abbeville$Year == 2013))] = as.numeric(abbeville$Incoming.Examinations[which((abbeville$Month == 6 & abbeville$Year == 2013))]) + nrow(subset(batonrouge, as.Date(Date) >=as.Date("2013-06-01") & as.Date(Date) <=as.Date("2013-06-30") & batonrouge$Examination %in% heartexam$code & batonrouge$`Original Hospital Location` == "Abbeville"))
  
  #for the month of July: extract the number of heart related exams that were rerouted from abbeville. 
  #add this count to the existing abbeville count.
  #repeat for each location.
  #as reference: violet=0, new orleans=0, lafayette=4, baton rouge=10 original=5000 + 14 = 5014 total.
abbeville$Incoming.Examinations[which((abbeville$Month == 7 & abbeville$Year == 2013))] = as.numeric(abbeville$Incoming.Examinations[which((abbeville$Month == 7 & abbeville$Year == 2013))]) + nrow(subset(violet, as.Date(Date) >=as.Date("2013-07-01") & as.Date(Date) <=as.Date("2013-07-31") & violet$Examination %in% heartexam$code & violet$`Original Hospital Location` == "Abbeville"))
abbeville$Incoming.Examinations[which((abbeville$Month == 7 & abbeville$Year == 2013))] = as.numeric(abbeville$Incoming.Examinations[which((abbeville$Month == 7 & abbeville$Year == 2013))]) + nrow(subset(neworleans, as.Date(Date) >=as.Date("2013-07-01") & as.Date(Date) <=as.Date("2013-07-31") & neworleans$Examination %in% heartexam$code & neworleans$`Original Hospital Location` == "Abbeville"))
abbeville$Incoming.Examinations[which((abbeville$Month == 7 & abbeville$Year == 2013))] = as.numeric(abbeville$Incoming.Examinations[which((abbeville$Month == 7 & abbeville$Year == 2013))]) + nrow(subset(lafayette, as.Date(Date) >=as.Date("2013-07-01") & as.Date(Date) <=as.Date("2013-07-31") & lafayette$Examination %in% heartexam$code & lafayette$`Original Hospital Location` == "Abbeville"))
abbeville$Incoming.Examinations[which((abbeville$Month == 7 & abbeville$Year == 2013))] = as.numeric(abbeville$Incoming.Examinations[which((abbeville$Month == 7 & abbeville$Year == 2013))]) + nrow(subset(batonrouge, as.Date(Date) >=as.Date("2013-07-01") & as.Date(Date) <=as.Date("2013-07-31") & batonrouge$Examination %in% heartexam$code & batonrouge$`Original Hospital Location` == "Abbeville"))

#Missing data between Dec 2009 - Feb 2010.
  #setting the missing values between between Dec 2009 - Feb 2010.
  #tt's explained there were 5129 total exams between this period.
  #dividing this number by 3, with the 2 extra head counts going into 2010 numbers.
abbeville$Incoming.Examinations[which((abbeville$Month == 12 & abbeville$Year == 2009))] = 1709
abbeville$Incoming.Examinations[which((abbeville$Month == 1 & abbeville$Year == 2010))] = 1710
abbeville$Incoming.Examinations[which((abbeville$Month == 2 & abbeville$Year == 2010))] = 1710

#Missing exams for Dec 2013:
  #the purpose of this function is to count the number of missing exams in Dec of 2013.
  #first it loops through all the dec13 unique SYSID strings and breaks them into components of prefix/suffix for comparision.
  #second if the prefix and suffix match the proper coding, then it checks the middle string against the heartcode table for a match.
  #if all conditions are met, then the counter is incremented by 1. Once the loop finishs, the counter is returned
dec13_counter <- function(dec13, heartcode){
    #initialize counter
  counter = 0
    #loop through each SYSID and break it into components to check against.
  for (i in 1:nrow(dec13)){
      #check prefix for abbeville code.
    if (substring(dec13$`Routing SYSID`[i],1,4) == "L839"){
        #check suffix for abbeville codes.
      if (substring(dec13$`Routing SYSID`[i],14,17) %in% c("TGU3","ROV8")){
          #check middle code against the heard code table. if match, then its a heart exam and needs to be counted. 
        if (substring(dec13$`Routing SYSID`[i],8,13) %in% heartcode$`Condition Code`){
          counter = counter + 1
        }
      }
    }
  }
 return(counter)  
}
 #identifies that the Dec 2013 observation index is #70. Then change the number of exams to the output of the function: 5933.
abbeville$Incoming.Examinations[which((abbeville$Month == 12 & abbeville$Year == 2013))] = dec13_counter(dec13,heartcode)

  #resort abbeville in temporal order: year, then month. 
abbeville = abbeville[order(abbeville$Year, abbeville$Month),]


####################### Completing the Imputation using MICE #######################################

#Imputation Problem:
  #using the mice package to impute missing data using 5 iterations of predictive mean matching.
  #averaging the imputed values and replacing the missing data with its imputed value. 
imputation = mice(abbeville, seed = 123456)

abbeville$Incoming.Examinations[which((abbeville$Month == 3 & abbeville$Year == 2006))] = as.integer((imputation$imp$Incoming.Examinations$`1`[1] + imputation$imp$Incoming.Examinations$`2`[1] + imputation$imp$Incoming.Examinations$`3`[1] + imputation$imp$Incoming.Examinations$`4`[1] + imputation$imp$Incoming.Examinations$`5`[1]) / 5)
abbeville$Incoming.Examinations[which((abbeville$Month == 6 & abbeville$Year == 2006))] = as.integer((imputation$imp$Incoming.Examinations$`1`[2] + imputation$imp$Incoming.Examinations$`2`[2] + imputation$imp$Incoming.Examinations$`3`[2] + imputation$imp$Incoming.Examinations$`4`[2] + imputation$imp$Incoming.Examinations$`5`[2]) / 5)
abbeville$Incoming.Examinations[which((abbeville$Month == 12 & abbeville$Year == 2008))] = as.integer((imputation$imp$Incoming.Examinations$`1`[3] + imputation$imp$Incoming.Examinations$`2`[3] + imputation$imp$Incoming.Examinations$`3`[3] + imputation$imp$Incoming.Examinations$`4`[3] + imputation$imp$Incoming.Examinations$`5`[3]) / 5)
abbeville$Incoming.Examinations[which((abbeville$Month == 5 & abbeville$Year == 2009))] = as.integer((imputation$imp$Incoming.Examinations$`1`[4] + imputation$imp$Incoming.Examinations$`2`[4] + imputation$imp$Incoming.Examinations$`3`[4] + imputation$imp$Incoming.Examinations$`4`[4] + imputation$imp$Incoming.Examinations$`5`[4]) / 5)
abbeville$Incoming.Examinations[which((abbeville$Month == 6 & abbeville$Year == 2010))] = as.integer((imputation$imp$Incoming.Examinations$`1`[5] + imputation$imp$Incoming.Examinations$`2`[5] + imputation$imp$Incoming.Examinations$`3`[5] + imputation$imp$Incoming.Examinations$`4`[5] + imputation$imp$Incoming.Examinations$`5`[5]) / 5)
abbeville$Incoming.Examinations[which((abbeville$Month == 1 & abbeville$Year == 2011))] = as.integer((imputation$imp$Incoming.Examinations$`1`[6] + imputation$imp$Incoming.Examinations$`2`[6] + imputation$imp$Incoming.Examinations$`3`[6] + imputation$imp$Incoming.Examinations$`4`[6] + imputation$imp$Incoming.Examinations$`5`[6]) / 5)
abbeville$Incoming.Examinations[which((abbeville$Month == 12 & abbeville$Year == 2011))] = as.integer((imputation$imp$Incoming.Examinations$`1`[7] + imputation$imp$Incoming.Examinations$`2`[7] + imputation$imp$Incoming.Examinations$`3`[7] + imputation$imp$Incoming.Examinations$`4`[7] + imputation$imp$Incoming.Examinations$`5`[7]) / 5)


################# Outputing the completed dataset to a .csv ##############################

write.csv(abbeville, "C:/Users/joshj/Documents/DS700-Intro to Data Science/Final Assignment/Dataset_clean.csv", row.names = F)
