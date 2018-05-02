library(shiny)
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(leaflet)
library(RColorBrewer)
library(htmltools)
library(scales)
library(DT)

#setwd("~/Desktop/git_proj/Anthony_Parrillo_Shiny_Project")
turnstileinfo = read.table('turnstile_160109.txt', header = TRUE, sep = ",")

recordMonth = 1
recordDate = 9

while (recordMonth != 7) { #Records in all of the text files from 160109 to 160702
  recordDate = recordDate + 7
  if (recordDate >= 10 & recordDate < 31) {
    DateString = as.character(recordDate)
  } else if (recordDate < 10) {
    DateString = paste0('0', recordDate)
  } else {
    if (recordMonth == 1 | recordMonth == 3 | recordMonth == 5) {
      recordDate = recordDate - 31
      DateString = paste0('0', recordDate)
      recordMonth = recordMonth + 1
    } else if (recordMonth == 2) {
      recordDate = recordDate - 29
      DateString = paste0('0', recordDate)
      recordMonth = recordMonth + 1
    } else {
      recordDate = recordDate - 30
      DateString = paste0('0', recordDate)
      recordMonth = recordMonth + 1
    }
  }
  
  MonthString = as.character(recordMonth)
  
  FileToOpenName = paste0('turnstile_160', MonthString, DateString, '.txt')
  NewData = read.table(FileToOpenName, header = TRUE, sep = ',')
  turnstileinfo = rbind(turnstileinfo, NewData) #adds new dataset as rows underneath previous data set
}

turnstileinfo = turnstileinfo %>% filter(!C.A %in% c('TRAM1', 'TRAM2', 'N519', 'N519A', 'R406', 'R407'), ENTRIES != 0) #Remove TRAM data, and turnstiles with no ENTRY data

#Removes PATH train stations
for (i in 1:9) {
  PATHSTRING = paste0('PTH0', i)
  turnstileinfo = turnstileinfo %>% filter(!C.A == PATHSTRING)
}

for (i in 10:21) {
  PATHSTRING = paste0('PTH', i)
  turnstileinfo = turnstileinfo %>% filter(!C.A == PATHSTRING)
}

#Get the number of ENTRIES per day
entrydata = as.data.frame(turnstileinfo %>% group_by(C.A, UNIT, SCP, STATION, LINENAME, DATE) %>% summarise(FIRSTENTRY = ENTRIES[1], LASTENTRY = max(ENTRIES), TOTAL = max(ENTRIES) - ENTRIES[1]))
#Only keep "reasonable data" 
ent = as.data.frame(entrydata %>% filter(TOTAL < 12500))
#Store bad data for later analysis
baddata = as.data.frame(entrydata %>% filter(TOTAL > 12500)) #collect data that is unreasonnable for one day
#Number of entries by station

daytotals = ent %>% group_by(DATE) %>% summarise(DAYTOTAL = sum(TOTAL))

stationtotals = ent %>% group_by(STATION, LINENAME) %>% summarise(TOTAL6MONTHS = sum(TOTAL)) %>% arrange(desc(TOTAL6MONTHS))

MTAStationLocationsorig = read.csv('MTA_StationLocations.csv', header = TRUE)
#Filter out Staten Island Trains(there were none in the data set)
MTAStationLocations = MTAStationLocationsorig %>% filter(Borough != 'SI')

MTAStationLocations[,6] = toupper(MTAStationLocations[,6]) # make all entries uppercase
MTAStationLocations[,6] = gsub(' - ', '-', MTAStationLocations[,6]) #Make station entries more matchable
names(MTAStationLocations)[6] = 'STATION' #Matching Column name "STATION"
MTAStationLocations = MTAStationLocations %>% select(STATION, Daytime.Routes, Borough, GTFS.Latitude, GTFS.Longitude)

stationtotals = stationtotals %>% arrange(STATION)
MTAStationLocations = MTAStationLocations %>% arrange(STATION)
stationtotals$STATION = as.character(stationtotals$STATION)

#Match Station Names Before Merging DataSets
stationtotals[5,1] = MTAStationLocations[5,1]
stationtotals[31,1] = MTAStationLocations[39,1]
stationtotals[32,1] = MTAStationLocations[40,1]
stationtotals[33,1] = MTAStationLocations[41,1]
stationtotals[65,1] = MTAStationLocations[90,1]
stationtotals[67,1] = MTAStationLocations[91,1]
stationtotals[68:70,1] = MTAStationLocations[94,1]
stationtotals[75,1] = MTAStationLocations[101,1]
stationtotals[77:78,1] = MTAStationLocations[103,1]
stationtotals[81,1] = MTAStationLocations[106,1]
stationtotals[82,1] = MTAStationLocations[107,1]
stationtotals[98:99,1] = MTAStationLocations[123,1]
stationtotals[114,1] = '75 ST-ELDERTS LANE'
MTAStationLocations[142,1] = '75 ST-ELDERTS LANE'
stationtotals[123,1] = MTAStationLocations[151,1]
stationtotals[124,1] = MTAStationLocations[152,1]
stationtotals[125,1] = MTAStationLocations[153,1]
stationtotals[133,1] = MTAStationLocations[163,1]
stationtotals[138,1] = MTAStationLocations[171,1]
stationtotals[139,1] = MTAStationLocations[170,1]
stationtotals[141,1] = MTAStationLocations[174,1]
stationtotals[142:143,1] = MTAStationLocations[176,1]
stationtotals[166:167,1] = MTAStationLocations[205,1]
stationtotals[171,1] = MTAStationLocations[210,1]
stationtotals[177,1] = MTAStationLocations[218,1]
stationtotals[185,1] = MTAStationLocations[234,1]
stationtotals[189,1] = MTAStationLocations[241,1]
stationtotals[205:206,1] = MTAStationLocations[260,1]
stationtotals[207,1] = MTAStationLocations[262,1]
stationtotals[212,1] = MTAStationLocations[267,1]
stationtotals[214,1] = MTAStationLocations[271,1]
stationtotals[218,1] = MTAStationLocations[276,1]
stationtotals[223,1] = MTAStationLocations[285,1]
stationtotals[224,1] = MTAStationLocations[286,1]
stationtotals[228,1] = MTAStationLocations[291,1]
stationtotals[229,1] = MTAStationLocations[292,1]
stationtotals[232,1] = MTAStationLocations[295,1]
stationtotals[235,1] = MTAStationLocations[298,1]
stationtotals[236,1] = MTAStationLocations[299,1]
stationtotals[240:242,1] = MTAStationLocations[300,1]
stationtotals[248,1] = MTAStationLocations[316,1]
stationtotals[251,1] = MTAStationLocations[317,1]
stationtotals[260,1] = MTAStationLocations[333,1]
stationtotals[262,1] = MTAStationLocations[335,1]
stationtotals[263,1] = MTAStationLocations[336,1]
stationtotals[270,1] = MTAStationLocations[341,1]
stationtotals[273,1] = MTAStationLocations[350,1]
stationtotals[280,1] = MTAStationLocations[357,1]
stationtotals[281,1] = MTAStationLocations[358,1]
stationtotals[284,1] = MTAStationLocations[361,1]
stationtotals[293,1] = MTAStationLocations[372,1]
stationtotals[297,1] = MTAStationLocations[378,1]
stationtotals[301,1] = MTAStationLocations[383,1]
stationtotals[302,1] = MTAStationLocations[385,1]
stationtotals[305,1] = MTAStationLocations[390,1]
stationtotals[311,1] = MTAStationLocations[397,1]
stationtotals[317:318,1] = MTAStationLocations[402,1]
stationtotals[332,1] = MTAStationLocations[417,1]
stationtotals[339,1] = MTAStationLocations[424,1]
stationtotals[351:352,1] = MTAStationLocations[439,1]
stationtotals[354,1] = MTAStationLocations[441,1]
stationtotals[363,1] = MTAStationLocations[453,1]
stationtotals[364,1] = MTAStationLocations[454,1]
MTAStationLocations[455,1] = 'W 4 ST-WASHINGTON SQ'
stationtotals[365,1] = 'W 4 ST-WASHINGTON SQ'
stationtotals[366,1] = MTAStationLocations[456,1]
stationtotals[367,1] = MTAStationLocations[461,1]
stationtotals[368,1] = MTAStationLocations[462,1]
stationtotals[377,1] = MTAStationLocations[15,1]
stationtotals[378,1] = MTAStationLocations[23,1]
stationtotals[379,1] = MTAStationLocations[24,1]
stationtotals[380,1] = MTAStationLocations[33,1]
stationtotals[381,1] = MTAStationLocations[35,1]
stationtotals[390,1] = MTAStationLocations[68,1]
stationtotals[397,1] = MTAStationLocations[86,1]
stationtotals[398,1] = MTAStationLocations[87,1]
stationtotals[403,1] = MTAStationLocations[129,1]
stationtotals[404,1] = MTAStationLocations[131,1]
stationtotals[415,1] = MTAStationLocations[230,1]
stationtotals[419,1] = MTAStationLocations[247,1]
stationtotals[420,1] = MTAStationLocations[252,1]
stationtotals[427,1] = MTAStationLocations[319,1]
stationtotals[428,1] = MTAStationLocations[329,1]
stationtotals[432,1] = MTAStationLocations[342,1]
stationtotals[433,1] = MTAStationLocations[344,1]
stationtotals[434,1] = MTAStationLocations[361,1]
stationtotals[435,1] = MTAStationLocations[369,1]
stationtotals[440,1] = MTAStationLocations[425,1]
stationtotals[442,1] = MTAStationLocations[431,1]
stationtotals[444,1] = MTAStationLocations[450,1]
stationtotals[445,1] = MTAStationLocations[457,1]
stationtotals[448,1] = MTAStationLocations[460,1]
stationtotals[451,1] = MTAStationLocations[342,1]


#1 Line
onelinetotal = as.data.frame(stationtotals %>% filter(str_detect(LINENAME, '1')) %>% group_by(STATION) %>% summarise(TOTAL6MONTHS = sum(TOTAL6MONTHS)))
onelinelocation = as.data.frame(MTAStationLocations %>% filter(str_detect(Daytime.Routes, '1')) %>% arrange(desc(GTFS.Latitude)))
onelinedata = right_join(onelinetotal, onelinelocation, BY = "STATION")

onelinedata[10,2] = onelinetotal[8,2]
onelinedata[36,2] = 0

finalonelinedata = onelinedata %>% select(-Daytime.Routes) %>% filter(TOTAL6MONTHS != 0) %>% mutate(LineID = '1')

#2 Line
twolinetotal = as.data.frame(stationtotals %>% filter(str_detect(LINENAME, '2')) %>% group_by(STATION) %>% summarise(TOTAL6MONTHS = sum(TOTAL6MONTHS)))
twolinelocation = as.data.frame(MTAStationLocations %>% filter(str_detect(Daytime.Routes, '2')) %>% arrange(desc(GTFS.Latitude)))
twolinedata = right_join(twolinetotal, twolinelocation, BY = "STATION")

finaltwolinedata = twolinedata %>% select(-Daytime.Routes) %>% filter(TOTAL6MONTHS != 0) %>% mutate(LineID = '2')

#3 Line
threelinetotal = as.data.frame(stationtotals %>% filter(str_detect(LINENAME, '3')) %>% group_by(STATION) %>% summarise(TOTAL6MONTHS = sum(TOTAL6MONTHS)))
threelinelocation = as.data.frame(MTAStationLocations %>% filter(str_detect(Daytime.Routes, '3')) %>% arrange(desc(GTFS.Latitude)))
threelinedata = right_join(threelinetotal, threelinelocation, BY = "STATION")

finalthreelinedata = threelinedata %>% select(-Daytime.Routes) %>% filter(TOTAL6MONTHS != 0) %>% mutate(LineID = '3')

#4 Line
fourlinetotal = as.data.frame(stationtotals %>% filter(str_detect(LINENAME, '4')) %>% group_by(STATION) %>% summarise(TOTAL6MONTHS = sum(TOTAL6MONTHS)))
fourlinelocation = as.data.frame(MTAStationLocations %>% filter(str_detect(Daytime.Routes, '4')) %>% arrange(desc(GTFS.Latitude)))
fourlinedata = right_join(fourlinetotal, fourlinelocation, BY = "STATION")

fourlinedata[3,2] = onelinetotal[13,2]

finalfourlinedata = fourlinedata %>% select(-Daytime.Routes) %>% filter(TOTAL6MONTHS != 0) %>% mutate(LineID = '4')

#5 Line
fivelinetotal = as.data.frame(stationtotals %>% filter(str_detect(LINENAME, '5')) %>% group_by(STATION) %>% summarise(TOTAL6MONTHS = sum(TOTAL6MONTHS)))
fivelinelocation = as.data.frame(MTAStationLocations %>% filter(str_detect(Daytime.Routes, '5')) %>% arrange(desc(GTFS.Latitude)))
fivelinedata = right_join(fivelinetotal, fivelinelocation, BY = "STATION")

fivelinedata[1:2,2] = 0
fivelinedata[4:5,2] = 0
fivelinedata[7:8,2] = 0
fivelinedata[10,2] = 0
fivelinedata[12,2] = 0
fivelinedata[14,2] = 0

finalfivelinedata = fivelinedata %>% select(-Daytime.Routes) %>% filter(TOTAL6MONTHS != 0) %>% mutate(LineID = '5')

#6 Line
sixlinetotal = as.data.frame(stationtotals %>% filter(str_detect(LINENAME, '6')) %>% group_by(STATION) %>% summarise(TOTAL6MONTHS = sum(TOTAL6MONTHS)))
sixlinelocation = as.data.frame(MTAStationLocations %>% filter(str_detect(Daytime.Routes, '6')) %>% arrange(desc(GTFS.Latitude)))
sixlinedata = right_join(sixlinetotal, sixlinelocation, BY = "STATION")

sixlinedata[16,2] = 0

finalsixlinedata = sixlinedata %>% select(-Daytime.Routes) %>% filter(TOTAL6MONTHS != 0) %>% mutate(LineID = '6')

#7 Line
sevenlinetotal = as.data.frame(stationtotals %>% filter(str_detect(LINENAME, '7')) %>% group_by(STATION) %>% summarise(TOTAL6MONTHS = sum(TOTAL6MONTHS)))
sevenlinelocation = as.data.frame(MTAStationLocations %>% filter(str_detect(Daytime.Routes, '7')) %>% arrange(desc(GTFS.Latitude)))
sevenlinedata = right_join(sevenlinetotal, sevenlinelocation, BY = "STATION")

sevenlinedata[5,2] = sevenlinetotal[9,2]
sevenlinedata[16,2] = sevenlinetotal[11,2]

finalsevenlinedata = sevenlinedata %>% select(-Daytime.Routes) %>% filter(TOTAL6MONTHS != 0) %>% mutate(LineID = '7')

#A Line
Alinetotal = as.data.frame(stationtotals %>% filter(str_detect(LINENAME, 'A')) %>% group_by(STATION) %>% summarise(TOTAL6MONTHS = sum(TOTAL6MONTHS)))
Alinelocation = as.data.frame(MTAStationLocations %>% filter(str_detect(Daytime.Routes, 'A')) %>% arrange(desc(GTFS.Latitude)))
Alinedata = right_join(Alinetotal, Alinelocation, BY = "STATION")

Alinedata[18,2] = Alinetotal[40,2]
Alinedata[20,2] = Alinetotal[42,2]

finalAlinedata = Alinedata %>% select(-Daytime.Routes) %>% filter(TOTAL6MONTHS != 0) %>% mutate(LineID = 'A')

#B Line
Blinetotal = as.data.frame(stationtotals %>% filter(str_detect(LINENAME, 'B')) %>% group_by(STATION) %>% summarise(TOTAL6MONTHS = sum(TOTAL6MONTHS)))
Blinelocation = as.data.frame(MTAStationLocations %>% filter(str_detect(Daytime.Routes, 'B')) %>% arrange(desc(GTFS.Latitude)))
Blinedata = right_join(Blinetotal, Blinelocation, BY = "STATION")

Blinedata[15,2] = Blinetotal[30,2]
Blinedata[27,2] = 0

finalBlinedata = Blinedata %>% select(-Daytime.Routes) %>% filter(TOTAL6MONTHS != 0) %>% mutate(LineID = 'B')

#C Line
Clinetotal = as.data.frame(stationtotals %>% filter(str_detect(LINENAME, 'C')) %>% group_by(STATION) %>% summarise(TOTAL6MONTHS = sum(TOTAL6MONTHS)))
Clinelocation = as.data.frame(MTAStationLocations %>% filter(str_detect(Daytime.Routes, 'C')) %>% arrange(desc(GTFS.Latitude)))
Clinedata = right_join(Clinetotal, Clinelocation, BY = "STATION")

Clinedata[8,2] = Clinetotal[22,2]
Clinedata[26,2] = Clinetotal[30,2]

finalClinedata = Clinedata %>% select(-Daytime.Routes) %>% filter(TOTAL6MONTHS != 0) %>% mutate(LineID = 'C')

#D Line
Dlinetotal = as.data.frame(stationtotals %>% filter(str_detect(LINENAME, 'D')) %>% group_by(STATION) %>% summarise(TOTAL6MONTHS = sum(TOTAL6MONTHS)))
Dlinelocation = as.data.frame(MTAStationLocations %>% filter(str_detect(Daytime.Routes, 'D')) %>% arrange(desc(GTFS.Latitude)))
Dlinedata = right_join(Dlinetotal, Dlinelocation, BY = "STATION")

Dlinedata[20,2] = 0
Dlinedata[28,2] = 0

finalDlinedata = Dlinedata %>% select(-Daytime.Routes) %>% filter(TOTAL6MONTHS != 0) %>% mutate(LineID = 'D')

#E Line
Elinetotal = as.data.frame(stationtotals %>% filter(str_detect(LINENAME, 'E')) %>% group_by(STATION) %>% summarise(TOTAL6MONTHS = sum(TOTAL6MONTHS)))
Elinelocation = as.data.frame(MTAStationLocations %>% filter(str_detect(Daytime.Routes, 'E')) %>% arrange(desc(GTFS.Latitude)))
Elinedata = right_join(Elinetotal, Elinelocation, BY = "STATION")

Elinedata[18,2] = Elinetotal[27,2]

finalElinedata = Elinedata %>% select(-Daytime.Routes) %>% filter(TOTAL6MONTHS != 0) %>% mutate(LineID = 'E')

#F Line
Flinetotal = as.data.frame(stationtotals %>% filter(str_detect(LINENAME, 'F')) %>% group_by(STATION) %>% summarise(TOTAL6MONTHS = sum(TOTAL6MONTHS)))
Flinelocation = as.data.frame(MTAStationLocations %>% filter(str_detect(Daytime.Routes, 'F')) %>% arrange(desc(GTFS.Latitude)))
Flinedata = right_join(Flinetotal, Flinelocation, BY = "STATION")

Flinedata[12,2] = 0
Flinedata[19,2] = Flinetotal[37,2]
Flinedata[25,2] = Flinetotal[38,2]
Flinedata[29,2] = Flinetotal[9,2]

finalFlinedata = Flinedata %>% select(-Daytime.Routes) %>% filter(TOTAL6MONTHS != 0) %>% mutate(LineID = 'F')

#G Line
Glinetotal = as.data.frame(stationtotals %>% filter(str_detect(LINENAME, 'G')) %>% group_by(STATION) %>% summarise(TOTAL6MONTHS = sum(TOTAL6MONTHS)))
Glinelocation = as.data.frame(MTAStationLocations %>% filter(str_detect(Daytime.Routes, 'G')) %>% arrange(desc(GTFS.Latitude)))
Glinedata = right_join(Glinetotal, Glinelocation, BY = "STATION")

Glinedata[8,2] = Glinetotal[22,2]
Glinedata[9,2] = Glinetotal[7,2]
Glinedata[17,2] = Glinetotal[5,2]

finalGlinedata = Glinedata %>% select(-Daytime.Routes) %>% filter(TOTAL6MONTHS != 0) %>% mutate(LineID = 'G')

#J Line
Jlinetotal = as.data.frame(stationtotals %>% filter(str_detect(LINENAME, 'J')) %>% group_by(STATION) %>% summarise(TOTAL6MONTHS = sum(TOTAL6MONTHS)))
Jlinelocation = as.data.frame(MTAStationLocations %>% filter(str_detect(Daytime.Routes, 'J')) %>% arrange(desc(GTFS.Latitude)))
Jlinedata = right_join(Jlinetotal, Jlinelocation, BY = "STATION")

Jlinedata[2,2] = Jlinetotal[17,2]

finalJlinedata = Jlinedata %>% select(-Daytime.Routes) %>% filter(TOTAL6MONTHS != 0) %>% mutate(LineID = 'J')

#L Line
Llinetotal = as.data.frame(stationtotals %>% filter(str_detect(LINENAME, 'L')) %>% group_by(STATION) %>% summarise(TOTAL6MONTHS = sum(TOTAL6MONTHS)))
Llinelocation = as.data.frame(MTAStationLocations %>% filter(str_detect(Daytime.Routes, 'L')) %>% arrange(desc(GTFS.Latitude)))
Llinedata = right_join(Llinetotal, Llinelocation, BY = "STATION")

Llinedata[3,2] = Llinetotal[3,2]
Llinedata[23,2] = Llinetotal[13,2]

finalLlinedata = Llinedata %>% select(-Daytime.Routes) %>% filter(TOTAL6MONTHS != 0) %>% mutate(LineID = 'L')

#M Line
Mlinetotal = as.data.frame(stationtotals %>% filter(str_detect(LINENAME, 'M')) %>% group_by(STATION) %>% summarise(TOTAL6MONTHS = sum(TOTAL6MONTHS)))
Mlinelocation = as.data.frame(MTAStationLocations %>% filter(str_detect(Daytime.Routes, 'M')) %>% arrange(desc(GTFS.Latitude)))
Mlinedata = right_join(Mlinetotal, Mlinelocation, BY = "STATION")

Mlinedata[22,2] = 0
Mlinedata[24,2] = Mlinetotal[19,2]
Mlinedata[25,2] = Mlinetotal[32,2]
Mlinedata[31,2] = Mlinetotal[37,2]

finalMlinedata = Mlinedata %>% select(-Daytime.Routes) %>% filter(TOTAL6MONTHS != 0) %>% mutate(LineID = 'M')

#N Line
Nlinetotal = as.data.frame(stationtotals %>% filter(str_detect(LINENAME, 'N')) %>% group_by(STATION) %>% summarise(TOTAL6MONTHS = sum(TOTAL6MONTHS)))
Nlinelocation = as.data.frame(MTAStationLocations %>% filter(str_detect(Daytime.Routes, 'N')) %>% arrange(desc(GTFS.Latitude)))
Nlinedata = right_join(Nlinetotal, Nlinelocation, BY = "STATION")

Nlinedata[6,2] = Nlinetotal[17,2]
Nlinedata[18,2] = 0

finalNlinedata = Nlinedata %>% select(-Daytime.Routes) %>% filter(TOTAL6MONTHS != 0) %>% mutate(LineID = 'N')

#Q Line
Qlinetotal = as.data.frame(stationtotals %>% filter(str_detect(LINENAME, 'Q')) %>% group_by(STATION) %>% summarise(TOTAL6MONTHS = sum(TOTAL6MONTHS)))
Qlinelocation = as.data.frame(MTAStationLocations %>% filter(str_detect(Daytime.Routes, 'Q')) %>% arrange(desc(GTFS.Latitude)))
Qlinedata = right_join(Qlinetotal, Qlinelocation, BY = "STATION")

Qlinedata[1:3,2] = 0
Qlinedata[5,2] = 0

finalQlinedata = Qlinedata %>% select(-Daytime.Routes) %>% filter(TOTAL6MONTHS != 0) %>% mutate(LineID = 'Q')

#R Line
Rlinetotal = as.data.frame(stationtotals %>% filter(str_detect(LINENAME, 'R')) %>% group_by(STATION) %>% summarise(TOTAL6MONTHS = sum(TOTAL6MONTHS)))
Rlinelocation = as.data.frame(MTAStationLocations %>% filter(str_detect(Daytime.Routes, 'R')) %>% arrange(desc(GTFS.Latitude)))
Rlinedata = right_join(Rlinetotal, Rlinelocation, BY = "STATION")

Rlinedata[3,2] = Rlinetotal[16,2]
Rlinedata[30,2] = Rlinetotal[27,2]
Rlinedata[31,2] = Rlinetotal[36,2]
Rlinedata[35,2] = Rlinetotal[12,2]

finalRlinedata = Rlinedata %>% select(-Daytime.Routes) %>% filter(TOTAL6MONTHS != 0) %>% mutate(LineID = 'R')

#S Line
Slinetotal = as.data.frame(stationtotals %>% filter(str_detect(LINENAME, 'S')) %>% group_by(STATION) %>% summarise(TOTAL6MONTHS = sum(TOTAL6MONTHS)))
Slinelocation = as.data.frame(MTAStationLocations %>% filter(str_detect(Daytime.Routes, 'S')) %>% arrange(desc(GTFS.Latitude)))
Slinedata = right_join(Slinetotal, Slinelocation, BY = "STATION")

finalSlinedata = Slinedata %>% select(-Daytime.Routes) %>% filter(TOTAL6MONTHS != 0) %>% mutate(LineID = 'S')

#Z Line
Zlinetotal = as.data.frame(stationtotals %>% filter(str_detect(LINENAME, 'Z')) %>% group_by(STATION) %>% summarise(TOTAL6MONTHS = sum(TOTAL6MONTHS)))
Zlinelocation = as.data.frame(MTAStationLocations %>% filter(str_detect(Daytime.Routes, 'Z')) %>% arrange(desc(GTFS.Latitude)))
Zlinedata = right_join(Zlinetotal, Zlinelocation, BY = "STATION")

Zlinedata[2,2] = Zlinetotal[12,2]

finalZlinedata = Zlinedata %>% select(-Daytime.Routes) %>% filter(TOTAL6MONTHS != 0) %>% mutate(LineID = 'Z')

#combine the data from all the lines
finalturnstiledata = rbind(finalonelinedata, finaltwolinedata, finalthreelinedata, finalfourlinedata, finalfivelinedata, finalsixlinedata, 
                           finalsevenlinedata, finalAlinedata, finalBlinedata, finalClinedata, finalDlinedata, finalElinedata, finalFlinedata, finalGlinedata,
                           finalJlinedata, finalLlinedata, finalMlinedata, finalNlinedata, finalQlinedata, finalRlinedata, finalSlinedata, finalZlinedata)

#Creates the drop-down menu list by line
linelist = c('1','2','3','4','5','6','7','A','B','C','D','E','F','G','J','L','M','N','Q','R','S','Z')

borototals = as.data.frame(finalturnstiledata %>% group_by(Borough) %>% summarise(Total = sum(TOTAL6MONTHS)))

borototals$Total = borototals$Total/1000000
perc = percent(borototals$Total/sum(borototals$Total))
