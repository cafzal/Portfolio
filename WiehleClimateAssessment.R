# Goal: Future projections of precipitation (and temperature) to influence stream discharge and ultimately stream restoration design.
# 1 year storm used for channel design. 100 year storm used for floodplain analysis.

# Discharge (now). 1 year = 80 cfs. 100 year = 968 cfs.
# Flow in cfs 80*(24*60*60)*0.0283 = 195609.6 m^3/day
# Area in km^2 1.14121*1000000 = 1141210 m^2
# Bankfull discharge depth 195609.6/1141210 = 0.1714054

# Flow in cfs 968*(24*60*60)*0.0283 = 2366876 m^3/day
# Area in km^2 1.14121*1000000 = 1141210 m^2
# 100 yr discharge depth = 2.074006

# Precip (now). 1 year, 24 hr = 2.62 in. 100 year, 24 hr = 8.3 in
# Precip (mm/day). 1 year: 66.548. 100 year: 210.82

# NASA NEX GDDP Data
# RCP 4.5

# Temp : kelvin  (1.8*(K - 273.15) + 32) = F
# Precip : KG/M^2*S P*86400 = mm/day

noaa(
  datasetid = "PRECIP_HLY",
  locationid = "ZIP:28801",
  datatypeid = "HPCP",
  limit = 5,
  token = "YOUR_TOKEN"
)


Pout2 <-
  ncdc(
    datasetid = 'GHCND',
    stationid = 'GHCND:US1VAFX0001',
    datatypeid = 'PRCP',
    startdate = '2017-01-01',
    enddate = '2017-12-31',
    limit = 365,
    token = "ykyiJzUNRbUtLjCmbxZEKjOljMGUtToH"
  )
P2017 <- Pout2$data

plot(P2017$value)
summary(P2017$value)
sum(P2017$value)

Pout1 <-
  ncdc(
    datasetid = 'GHCND',
    stationid = 'GHCND:USC00448737',
    datatypeid = 'PRCP',
    startdate = '1925-04-01',
    enddate = '2018-03-31',
    limit = 29037,
    token = "ykyiJzUNRbUtLjCmbxZEKjOljMGUtToH"
  )

Tout1 <-
  ncdc(
    datasetid = 'GHCND',
    stationid = 'GHCND:USC00448737',
    datatypeid = 'TMAX',
    startdate = '1925-04-01',
    enddate = '2018-03-31',
    limit = 29037,
    token = "ykyiJzUNRbUtLjCmbxZEKjOljMGUtToH"
  )

USC00448737

monitors <- c("US1VAFX0001", "USC00448737")
obs <- meteo_pull_monitors(monitors)
obs_covr <- meteo_coverage(obs)

load(EcoHydRology)

restonNEX_GDDPprojections$Prmm = 0
restonNEX_GDDPprojections$Tminc = 0
restonNEX_GDDPprojections$Tmaxc = 0

restonNEX_GDDPprojections <- read.csv("restonNEX-GDDPprojections.csv")

P = "pr"

for (i in 1:nrow(restonNEX_GDDPprojections))
{
if (restonNEX_GDDPprojections$Variable[i] == "pr")
   {
   restonNEX_GDDPprojections$Prmm[i] = restonNEX_GDDPprojections$Value[i]*86400
   }
 else if (restonNEX_GDDPprojections$Variable[i] == "tasmin")
   {
   restonNEX_GDDPprojections$Tminc[i] = ((1.8*(restonNEX_GDDPprojections$Value[i]-273.15))+32)
   restonNEX_GDDPprojections$Tmaxc[i] = 0
   restonNEX_GDDPprojections$Prmm[i] = 0
   }
 else (restonNEX_GDDPprojections$Variable[i] == "tasmax")
   {
     restonNEX_GDDPprojections$Tmaxc[i] = ((1.8*(restonNEX_GDDPprojections$Value[i]-273.15))+32)
     restonNEX_GDDPprojections$Tminc[i] = 0
     restonNEX_GDDPprojections$Prmm[i] = 0
   }
 }

restonNEX_GDDPprecip <- read.csv("wiehleprecipprojections.csv")
restonNEX_GDDPprecip$Date <-
  as.Date(restonNEX_GDDPprecip$Date, format = "%m/%d/%Y")

restonNEX_GDDPprecip$Prmm = 0
restonNEX_GDDPprecip$BFPlus = 0
for (i in 1:nrow(restonNEX_GDDPprecip))
{
  restonNEX_GDDPprecip$Prmm[i] = restonNEX_GDDPprecip$Value[i] * 86400
  
  if (restonNEX_GDDPprecip$Prmm[i] > 66.548)
  {
    restonNEX_GDDPprecip$BFPlus[i] = 1
  }
  
}
summary(restonNEX_GDDPprecip$Prmm)
sum(restonNEX_GDDPprecip$Prmm)

restonNEX_GDDPprecip$BFPlus <-
  as.numeric(as.character(restonNEX_GDDPprecip$BFPlus))

sum(restonNEX_GDDPprecip$BFPlus)
plot(
  restonNEX_GDDPprecip$Date,
  restonNEX_GDDPprecip$BFPlus,
  type = "h",
  main = "Frequency of Bankfull Events RCP 4.5",
  xlab = "Year",
  ylab = "Occurence"
)

D <- ts(restonNEX_GDDPprecip$BFPlus, frequency = 3650)
Dec = 1:8
Dsum <- aggregate(D, FUN = sum)
plot(
  Dsum,
  xlab = "Decade",
  ylab = "Relative Bankfull Frequency",
  axes = FALSE,
  frame.plot = TRUE,
  main = "Frequency of Bankfull Events RCP 4.5"
)
Axis(side = 1, labels = TRUE)
polygon(c(Dec, rev(Dec)),
        c(as.vector(Dsum), rep(0.001, length(Dsum))), col = "grey")
abline(lm(formula = Dsum ~ Dec), col = "red")

Prin <- (0.0393701 * restonNEX_GDDPprecip$Prmm)
summary(Prin)
plot(
  restonNEX_GDDPprecip$Date,
  Prin,
  type = "l",
  xlab = "Date",
  ylab = "Precipitation (in)",
  main = "Projected Precipitation RCP 4.5"
)
abline(lm(formula = Prin ~ restonNEX_GDDPprecip$Date), col = "red")
plot(
  restonNEX_GDDPprecip$Date[(79 * 365):(80 * 365)],
  Prin[(79 * 365):(80 * 365)],
  type = "l",
  xlab = "Date",
  ylab = "Precipitation (in)",
  main = "Projected Precipitation RCP 4.5"
)
lines(restonNEX_GDDPprecip$Date[1:365],
      Prin[1:365],
      col = "red",
      type = "l")

lm(restonNEX_GDDPprecip$Prmm ~ restonNEX_GDDPprecip$Date)

restonNEX_GDDPprecip$Year <-
  format(as.Date(restonNEX_GDDPprecip$Date, format = "%d/%m/%Y"), "%Y")

Y <- ts(restonNEX_GDDPprecip$Prmm, frequency = 365)
Yr = 2018:2097
Ymax <- aggregate(Y, FUN = max)
Ymaxin <- Ymax * 0.0393701
summary(Ymaxin)

Y2 <- ts(restonNEX_GDDPprecip$Prmm, frequency = (365 * 2))
Yr = 2018:2097
Y2max <- aggregate(Y2, FUN = max)
Y2maxin <- Y2max * 0.0393701
summary(Y2maxin)
# Use rainfall intensity for flow calcs

plot(
  Yr,
  Ymaxin,
  type = "l",
  xlab = "Year",
  ylab = "1 Year Storm Intensity (in/day)",
  main = "Projected Precipitation RCP 4.5"
)
polygon(c(Yr, rev(Yr)),
        c(as.vector(Ymaxin), rep(0.001, length(Ymaxin))), col = "light blue")
abline(lm(formula = Ymaxin ~ Yr), col = "red")
plot(lm(Y ~ restonNEX_GDDPprecip$Date))
# R squared / R value

#Min temp c 4.5
Tmin <- read.csv("Tmin.csv")
Tmin$Date <- as.Date(Tmin$Date, format = "%m/%d/%Y")
Tmin$Tmin <- ((1.8 * (Tmin$Value - 273.15)) + 32)

#Max temp c 4.5
Tmax <- read.csv("Tmax.csv")
Tmax$Date <- as.Date(Tmax$Date, format = "%m/%d/%Y")
Tmax$Tmax <- ((1.8 * (Tmax$Value - 273.15)) + 32)

summary(Tmax$Tmax)
plot(
  Tmax$Date,
  Tmax$Tmax,
  type = "l",
  xlab = "Year",
  ylab = "Daily Max Temperature (F)",
  main = "Projected Max Temperature RCP 4.5"
)
abline(lm(formula = Tmax$Tmax ~ Tmax$Date), col = "red")

plot(
  Tmax8$Date[(60 * 365):(61 * 365)],
  Tmax8$Tmax[(60 * 365):(61 * 365)],
  type = "l",
  xlab = "Year",
  ylab = "Daily Max Temperature (F)",
  main = "Projected Max Temperature RCP 4.5"
)[(79 * 365):(80 * 365)]
lines(Tmax8$Date[(5 * 365):(6 * 365)], Tmax8$Tmax[(5 * 365):(6 * 365)], col =
        "grey", type = "l")

 VSA45 <- Lumped_VSA_model(restonNEX_GDDPprecip$Date, restonNEX_GDDPprecip$Prmm, Tmax$Tmax, Tmin$Tmin, Depth = NULL, SATper = NULL, AWCper = NULL,
                 percentImpervious = 0, no_wet_class = 10, Tp = 5, latitudeDegrees = 38.96, albedo = 0.23,
                 StartCond = "avg", PETin = NULL, AWC = Depth * AWCper, SAT = Depth * SATper, SW1 = NULL,
                 BF1 = 1, PETcap = 5, rec_coef = 0.1, Se_min = 78, C1 = 3.1, Ia_coef = 0.05,
                 PreviousOutput = NULL, runoff_breakdown = RunoffBreakdown(Tp, HrPrcDelay = (Tp/2 - 4)))

 VSA85 <- Lumped_VSA_model(dateSeries = restonNEX_GDDPprecip$Date, 	P = restonNEX_GDDPprecip$Prmm, percentImpervious = 27,
                  Tmax = Tmax8$TmaxC, Tmin = Tmin8$TminC, latitudeDegrees=38.96, Tp = 5.8, Depth = 2010,
                  SATper = 0.27, AWCper = 0.13, StartCond = "avg")

summary(VSA85$modeled_flow)
plot(
  VSA85$Date,
  VSA85$modeled_flow,
  type = "l",
  ylab = "Flow Depth (mm/day)",
  xlab = "Date",
  main = "Modeled Flow"
)
abline(lm(formula = VSA85$modeled_flow ~ VSA85$Date), col = "red")

plot(
  VSA85$Date,
  VSA85$SoilWater,
  type = "l",
  ylab = "Soil Water Content (mm/day)",
  xlab = "Date",
  main = "Modeled Soil Water Content"
)
abline(lm(formula = VSA85$SoilWater ~ VSA85$Date), col = "red")

plot(
  VSA85$Date,
  VSA85$Se,
  type = "l",
  ylab = "Groundwater Storage (mm/day)",
  xlab = "Date",
  main = "Modeled Groundwater Storage"
)
abline(lm(formula = VSA85$Se ~ VSA85$Date), col = "red")

plot(
  VSA85$Date,
  VSA85$impervRunoff,
  type = "l",
  ylab = "Runoff (mm/day)",
  xlab = "Date",
  main = "Impervious Runoff"
)
abline(lm(formula = VSA85$impervRunoff ~ VSA85$Date), col = "red")

# Convert to cfs
VSA85$modeled_flowCFS <-
  (((VSA85$modeled_flow / 1000) * 1141210) / (24 * 60 * 60)) * 35.3147
summary(VSA85$modeled_flowCFS)
plot(VSA85$Date, VSA85$modeled_flowCFS, type = "l")


# NASA NEX GDDP Data
# RCP 8.5

restonNEX_GDDPprecip8 <- read.csv("wiehleprecipprojections85.csv")
restonNEX_GDDPprecip8$Date <-
  as.Date(restonNEX_GDDPprecip8$Date, format = "%m/%d/%Y")

for (i in 1:nrow(restonNEX_GDDPprecip8))
{
  restonNEX_GDDPprecip8$Prmm[i] = restonNEX_GDDPprecip8$Value[i] * 86400
  
  if (restonNEX_GDDPprecip8$Prmm[i] > 66.548)
  {
    restonNEX_GDDPprecip8$BFPlus[i] = 1
  }
}

restonNEX_GDDPprecip$BFPlus <-
  as.numeric(as.character(restonNEX_GDDPprecip$BFPlus))

sum(restonNEX_GDDPprecip$BFPlus)
plot(restonNEX_GDDPprecip$Date,
     restonNEX_GDDPprecip$BFPlus,
     type = "h")

summary(restonNEX_GDDPprecip8$Prmm)
plot(restonNEX_GDDPprecip8$Date,
     restonNEX_GDDPprecip8$Prmm,
     type = "l")
lm(restonNEX_GDDPprecip8$Prmm ~ restonNEX_GDDPprecip8$Date)
abline(lm(formula = restonNEX_GDDPprecip8$Prmm ~ restonNEX_GDDPprecip8$Date),
       col = "red")

restonNEX_GDDPprecip8$Year <-
  format(as.Date(restonNEX_GDDPprecip8$Date, format = "%d/%m/%Y"), "%Y")

Y8 <- ts(restonNEX_GDDPprecip8$Prmm, frequency = 365)
Yr8 = 2018:2097
Ymax8 <- aggregate(Y, FUN = max)
Ymaxin8 <- Ymax8 * 0.0393701
summary(Ymaxin8)
# Use rainfall intensity for flow calcs

plot(
  Yr8,
  Ymaxin8,
  type = "l",
  xlab = "Year",
  ylab = "1 Year Storm Intensity (in/day)",
  main = "Projected Precipitation RCP 8.5"
)
abline(lm(formula = Ymaxin8 ~ Yr8), col = "red")
plot(lm(Y8 ~ restonNEX_GDDPprecip8$Date))
# R squared / R value

#Min temp c 8.5
Tmin8 <- read.csv("Tmin8.csv")
Tmin8$Date <- as.Date(Tmin8$Date, format = "%m/%d/%Y")
Tmin8$Tmin <- ((1.8 * (Tmin8$Value - 273.15)) + 32)
Tmin8$TminC <- (Tmin8$Value - 273.15)

#Max temp c 8.5
Tmax8 <- read.csv("Tmax8.csv")
Tmax8$Date <- as.Date(Tmax8$Date, format = "%m/%d/%Y")
Tmax8$Tmax <- ((1.8 * (Tmax8$Value - 273.15)) + 32)
Tmax8$TmaxC <- (Tmax8$Value - 273.15)

summary(Tmax8$Tmax)
plot(
  Tmax8$Date,
  Tmax8$Tmax,
  type = "l",
  xlab = "Year",
  ylab = "Daily Max Temperature (F)",
  main = "Projected Max Temperature RCP 8.5"
)
abline(lm(formula = Tmax8$Tmax ~ Tmax8$Date), col = "red")

VSA85 <- Lumped_VSA_model(dateSeries = restonNEX_GDDPprecip8$Date, 	P = restonNEX_GDDPprecip8$Prmm, percentImpervious = 27,
                          Tmax = Tmax8$Tmax, Tmin = Tmin8$Tmin, latitudeDegrees=38.96, Tp = 5.8, Depth = 2010,
                          SATper = 0.27, AWCper = 0.13, StartCond = "avg")

summary(VSA85$modeled_flow)
plot(VSA85$Date, VSA85$modeled_flow, type="l")
abline(lm(formula = VSA85$modeled_flow ~ VSA85$Date), col="red")
# Convert to cfs
