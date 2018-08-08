require(SWATmodel)
library(SWATmodel)
setwd("C:/Users/Cameron Afzal/Documents/SWAT 4/Scenarios/Default/TxtInOut/")


SV_Likelihood<-function(AllData,evals)
{	
  
  current_epsilon = 1
  current_beta = 0.5
  current_sigma_0 = 0.2
  current_sigma_1 = 0
  current_phi = 0.5
  sd_mult = 0.1
  
  for (i in 1:evals)
  { 
    
    epsilon = rnorm(1, current_epsilon, sd_mult*(10 - 0.5))
    beta = rnorm(1, current_beta, sd_mult*(1 - 0))
    sigma_0 = rnorm(1, current_sigma_0, sd_mult*(1 - 0))
    sigma_1 = rnorm(1, current_sigma_1, sd_mult*(1 - 0))
    phi = rnorm(1, current_phi, sd_mult*(1 - 0))
    
    if (epsilon > 10)
    {epsilon = -1*epsilon + 20}
    if (epsilon < 0.5)
    {epsilon = -1*epsilon + 1}
    
    if (beta > 1)
    {beta = -1*beta + 2}
    if (beta < 0)
    {beta = -1*beta + 0}
    
    if (sigma_0 > 1)
    {sigma_0 = -1*sigma_0 + 2}
    if (sigma_0 < 0)
    {sigma_0 = -1*sigma_0 + 0}
    
    if (sigma_1 > 1)
    {sigma_1 = -1*sigma_1 + 2}
    if (sigma_1 < 0)
    {sigma_1 = -1*sigma_1 + 0}
    
    if (phi > 0.6)
    {phi = -1*phi + 1.2}
    if (phi < 0)
    {phi = -1*phi + 0}
    
    #Remove autocorrelation from residuals
    AllData$Res_Norm = 0
    for (c in 2:nrow(AllData))
    {AllData$Res_Norm[c] = AllData$Res[c] - phi*AllData$Res[c-1]}
    
    #Remove heteroscedasticity
    sigma_t = sigma_0 + sigma_1*abs(AllData$Sim)
    AllData$Res_Norm_Hom = AllData$Res_Norm/sigma_t
    AllData$Res_Norm_Hom[1] = 0
    
    #Compute Likelihood
    n = nrow(AllData)
    M1 = gamma(1 + beta)/(sqrt(gamma(3*(1+beta)/2))*sqrt(gamma((1 + beta)/2)))
    M2 = 1
    
    mew_e = M1*(epsilon - epsilon^-1)
    sigma_e = sqrt((M2 - M1^2)*(epsilon^2 + epsilon^-2) + 2*(M1^2) - M2)
    
    A1_mcmc = gamma(3*(1 + beta)/2)
    A2_mcmc = gamma((1 + beta)/2)
    wb = sqrt(A1_mcmc)/((1+beta)*A2_mcmc^(3/2))
    cb = (A1_mcmc/A2_mcmc)^(1/(1 + beta))
    
    alpha_wt = (mew_e + sigma_e*AllData$Res_Norm_Hom)/(epsilon^sign(mew_e + sigma_e*AllData$Res_Norm_Hom))
    new_l = n*log((2*sigma_e*wb)/(epsilon + epsilon^-1)) - sum(log(sigma_t)) - cb*sum(abs(alpha_wt)^(2/(1 + beta)))
    
    if (is.nan(new_l)) {new_l = -100000}
    
    if (i == 1)
    {max_l = new_l}
    
    if (new_l > max_l)
    {max_l = new_l
    current_epsilon = epsilon
    current_beta = beta
    current_sigma_0 = sigma_0
    current_sigma_1 = sigma_1
    current_phi = phi}
  }
  
  Results = matrix()
  Results[1] = max_l
  Results[2] = current_epsilon
  Results[3] = current_beta
  Results[4] = current_sigma_0
  Results[5] = current_sigma_1
  Results[6] = phi
  
  return(Results)
}


rm(.Random.seed, envir=globalenv())
inpath = 'C:/Users/Cameron Afzal/Documents/SWAT 4/'
outpath = 'C:/Users/Cameron Afzal/Documents/SWAT 4/'
SWAT_inpath = 'C:/Users/Cameron Afzal/Documents/SWAT 4/Scenarios/Default/TxtInOut/'

bsn_infile = paste(SWAT_inpath,'basins.bsn',sep="")
bsn_outfile = paste(SWAT_inpath,'basins.bsn',sep="")
rch_infile = paste(SWAT_inpath,'output.rch',sep="")
hru_infile = paste(SWAT_inpath, 'output.hru', sep="")

#Sample Parameters
current_SFTMP = -2.83236
current_SMTMP = 1.195584
current_SMFMX = 1.720141
current_TIMP = 0.885352
current_ESCO = 0.711496
current_EPCO = 0.789392
current_SURLAG = 0.041252
current_GW_DELAY = 0.792206
current_ALPHA_BF = 0.213899
current_CN_Mult = 0.513899
current_SOL_ALB = 0.1

#MCMC control parameters
sd_mult = 0.1
runs = 6

# Set dates
Dates = seq(as.Date("2013/06/04"),as.Date("2018/06/03"),"days")


#Initial Parameter Values
Results = data.frame(matrix(nrow = runs))
GL_Params = matrix(ncol=6)
GL_Params_flow = matrix(ncol=6)
GL_Params_panelVWC = matrix(ncol=6)
GL_Params_fieldVWC = matrix(ncol=6)
FlowResults = data.frame(matrix(nrow = length(Dates)))

for (m in 1:runs)
{
#m=1
  #Sample Parameters
  SFTMP = rnorm(1, current_SFTMP, sd_mult*(3 - -3))
  SMTMP = rnorm(1, current_SMTMP, sd_mult*(3 - -3))
  SMFMX = rnorm(1, current_SMFMX, sd_mult*(5 - 0.01))
  TIMP = rnorm(1, current_TIMP, sd_mult*(1 - 0))
  ESCO = rnorm(1, current_ESCO, sd_mult*(1 - 0.5))
  EPCO = rnorm(1, current_EPCO, sd_mult*(1 - 0.5))
  SURLAG = rnorm(1, current_SURLAG, sd_mult*(1 - 0.01))
  GW_DELAY = rnorm(1, current_GW_DELAY, sd_mult*(3 - 0.01))
  ALPHA_BF = rnorm(1, current_ALPHA_BF, sd_mult*(0.5 - 0.001))
  CN_Mult = rnorm(1, current_CN_Mult, sd_mult*(1 - 0.1))
  SOL_ALB = rnorm(1, current_SOL_ALB, sd_mult*(0.9 - 0.05))
  
  #Reflective Boundaries
  if (SFTMP > 3)
  {SFTMP = -1*SFTMP + 2*3}
  if (SFTMP < -3)
  {SFTMP = -1*SFTMP + 2*-3}
  
  if (SMTMP > 3)
  {SMTMP = -1*SMTMP + 2*3}
  if (SMTMP < -3)
  {SMTMP = -1*SMTMP + 2*-3}
  
  if (SMFMX > 5)
  {SMFMX = -1*SMFMX + 2*5}
  if (SMFMX < 0.01)
  {SMFMX = -1*SMFMX + 2*0.01}
  
  if (TIMP > 1)
  {TIMP = -1*TIMP + 2*1}
  if (TIMP < 0)
  {TIMP = -1*TIMP + 2*0}
  
  if (EPCO > 1)
  {EPCO = -1*EPCO + 2*1}
  if (EPCO < 0.5)
  {EPCO = -1*EPCO + 2*0.5}
  
  if (ESCO > 1)
  {ESCO = -1*ESCO + 2*1}
  if (ESCO < 0.5)
  {ESCO = -1*ESCO + 2*0.5}
  
  if (SURLAG > 1)
  {SURLAG = -1*SURLAG + 2*1}
  if (SURLAG < 0.01)
  {SURLAG = -1*SURLAG + 2*0.01}
  
  if (GW_DELAY > 3)
  {GW_DELAY = -1*GW_DELAY + 2*3}
  if (GW_DELAY < 0.01)
  {GW_DELAY = -1*GW_DELAY + 2*0.01}
  
  if (ALPHA_BF > 0.5)
  {ALPHA_BF = -1*ALPHA_BF + 2*0.5}
  if (ALPHA_BF < 0.001)
  {ALPHA_BF = -1*ALPHA_BF + 2*0.001}
  
  if (CN_Mult > 1)
  {CN_Mult = -1*CN_Mult + 2*1}
  if (CN_Mult < 0.1)
  {CN_Mult = -1*CN_Mult + 2*0.1}
    
  if (SOL_ALB > 0.9)
  {SOL_ALB = -1*SOL_ALB + 2*0.9}
  if (SOL_ALB < 0.05)
  {SOL_ALB = -1*SOL_ALB + 2*0.05}
  
  #Store Results
  Results$SFTMP[m] = SFTMP
  Results$SMTMP[m] = SMTMP
  Results$SMFMX[m] = SMFMX
  Results$TIMP[m] = TIMP
  Results$ESCO[m] = ESCO
  Results$EPCO[m] = EPCO
  Results$SURLAG[m] = SURLAG
  Results$GW_DELAY[m] = GW_DELAY
  Results$ALPHA_BF[m] = ALPHA_BF
  Results$CN_Mult[m] = CN_Mult
  Results$SOL_ALB[m] = SOL_ALB
  
  #Read in .bsn file and modify variables
  bsn_data <- file(bsn_infile, open = "r")
  bsn <- readLines(bsn_data)
  bsn[4] = paste("          ",sprintf("%.3f", SFTMP),"  | SFTMP : Snowfall temperature [?C]")
  bsn[5] = paste("          ",sprintf("%.3f", SMTMP),"  | SMTMP : Snowfall temperature [?C]")
  bsn[6] = paste("          ",sprintf("%.3f", SMFMX),"  | SMFMX : Snowfall temperature [?C]")
  bsn[7] = paste("          ",sprintf("%.3f", SMFMX),"  | SMFMX : Snowfall temperature [?C]")
  bsn[8] = paste("          ",sprintf("%.3f", TIMP),"   | TIMP : Snowfall temperature [?C]")
  bsn[13] = paste("          ",sprintf("%.3f", ESCO),"  | ESCO : Snowfall temperature [?C]")
  bsn[14] = paste("          ",sprintf("%.3f", EPCO),"  | EPCO : Snowfall temperature [?C]")
  bsn[20] = paste("          ",sprintf("%.3f", SURLAG),"  | SURLAG : Snowfall temperature [?C]")
  write.table(bsn, file = bsn_outfile,col.names = FALSE,row.names = FALSE,quote = FALSE)
  closeAllConnections() 
  
  #Read in .gw files and modify variables
  infile = paste(inpath,'GW_Files.txt',sep="")
  GW_files = read.delim(infile,sep="",header=TRUE)
  for (i in 1:length(GW_files[,]))
    {
      gw_infile = paste(SWAT_inpath,GW_files$Files[i],sep="")
      gw_outfile = paste(SWAT_inpath,GW_files$Files[i],sep="")
      gw_data <- file(gw_infile, open = "r")
      gw <- readLines(gw_data)
      gw[4] = paste("         ",sprintf("%.3f", GW_DELAY),"    | GW_DELAY : GW_DELAY : Groundwater delay [days]")
      gw[5] = paste("         ",sprintf("%.3f", ALPHA_BF),"    | ALPHA_BF : ALPHA_BF : Baseflow alpha factor [days]")
      write.table(gw, file = gw_outfile,col.names = FALSE,row.names = FALSE,quote = FALSE)
      closeAllConnections() 
    }

  #Read in .mgt files for panels only and modify variables
  infile = paste(inpath,'MGT_Files.txt',sep="")
  MGT_files = read.delim(infile,sep=",",header=TRUE)
  for (i in 1:nrow(MGT_files))
  {
    mgt_infile = paste(SWAT_inpath,MGT_files$Files[i],sep="")
    mgt_outfile = paste(SWAT_inpath,MGT_files$Files[i],sep="")
    mgt_data <- file(mgt_infile, open = "r")
    mgt <- readLines(mgt_data, n=34)
    CN2 = CN_Mult*MGT_files$CN[i]
    mgt[11] = paste("          ",sprintf("%.2f", CN2),"    | CN2: Initial SCS CN II value")
    write.table(mgt, file = mgt_outfile,col.names = FALSE,row.names = FALSE,quote = FALSE)
    closeAllConnections() 
  }
  
  #Read in .sol files for panels only and modify variables
  infile = paste(inpath,'SOL_Files2.txt',sep="")
  SOL_Files = read.delim(infile,sep="",header=TRUE)
  for (i in 1:nrow(SOL_Files))
  {
    sol_infile = paste(SWAT_inpath,SOL_Files$Files[i],sep="")
    sol_outfile = paste(SWAT_inpath,SOL_Files$Files[i],sep="")
    sol_data <- file(sol_infile, open = "r")
    sol <- readLines(sol_data, n=34)
    sol[17] = paste(" Soil Albedo (Moist)      :       ",sprintf("%.2f", SOL_ALB),"      ",sprintf("%.2f", SOL_ALB),"")
    write.table(sol, file = sol_outfile,col.names = FALSE,row.names = FALSE,quote = FALSE)
    closeAllConnections() 
  }
  
  #Call SWAT2012
  system2("swat2012.exe")
  
  #Specify SWAT reach to keep
  SWAT_Reach = 14
  
  #Read in SWAT model output .rch file
  output_raw = read.delim(rch_infile,skip=9,sep="")
  output.df = data.frame(output_raw)
  All_Flow = output.df[,2:8]
  Reach_Flow = All_Flow[All_Flow[,1] == SWAT_Reach,]
  ModelFlow = data.frame(matrix(nrow=length(Dates)))
  ModelFlow$Date = Dates
  ModelFlow$modeled_flow = Reach_Flow[,7]*(1e13)
  
  #Read in SWAT model output .hru file
  ## Time series for single HRU point, find time, pair up to measurement
  ## Make combined vector of all sim observed
  ## Incorp in likelihood fun and add values 
  
  soilDepth = 1000
  output_raw1 = read.delim(hru_infile,skip=9,sep="")
  output1.df = data.frame(output_raw1)
  All_HRU = output1.df[,1:17]
  All_HRU$Date = as.Date(ISOdate(All_HRU$X2013,All_HRU$X6,All_HRU$X4))
  LC = "BERM"
  Panel_HRU = All_HRU[All_HRU[,1] == LC,]
  Panel_HRU$SoilWater = Panel_HRU[,17]
  Panel_HRU$SoilWaterAdj = (Panel_HRU$SoilWater/soilDepth + 0.1)*100
  #Panel_HRU$Date = as.Date(ISOdate(Panel_HRU$X2013,Panel_HRU$X6,Panel_HRU$X4))
  SimVWC_Btw1 = Panel_HRU[Panel_HRU[,3] == 100001,]
  SimVWC_Btw3 = Panel_HRU[Panel_HRU[,3] == 100003,]
  SimVWC_Btw5 = Panel_HRU[Panel_HRU[,3] == 100005,]
  SimVWC_Btw9 = Panel_HRU[Panel_HRU[,3] == 100009,]
  SimVWC_Btw10 = Panel_HRU[Panel_HRU[,3] == 100010,]
  SimVWC_Und1 = Panel_HRU[Panel_HRU[,3] == 60001,]
  SimVWC_Und3 = Panel_HRU[Panel_HRU[,3] == 60003,]
  SimVWC_Und4 = Panel_HRU[Panel_HRU[,3] == 60004,]
  SimVWC_Und9 = Panel_HRU[Panel_HRU[,3] == 60009,]
  SimVWC_Und10 = Panel_HRU[Panel_HRU[,3] == 60010,]

  SimVWC_Fiel1010 = All_HRU[All_HRU[,3] == 100010,]
  SimVWC_Fiel1010$SoilWater = SimVWC_Fiel1010[,17]
  SimVWC_Fiel1010$SoilWaterAdj = (SimVWC_Fiel1010$SoilWater/soilDepth + 0.1)*100
  SimVWC_Fiel108 = All_HRU[All_HRU[,3] == 100008,]
  SimVWC_Fiel108$SoilWater = SimVWC_Fiel108[,17]
  SimVWC_Fiel108$SoilWaterAdj = (SimVWC_Fiel108$SoilWater/soilDepth + 0.1)*100
  SimVWC_Fiel124 = All_HRU[All_HRU[,3] == 120004,]
  SimVWC_Fiel124$SoilWater = SimVWC_Fiel124[,17]
  SimVWC_Fiel124$SoilWaterAdj = (SimVWC_Fiel124$SoilWater/soilDepth + 0.1)*100
  SimVWC_Fiel88 = All_HRU[All_HRU[,3] == 80008,]
  SimVWC_Fiel88$SoilWater = SimVWC_Fiel88[,17]
  SimVWC_Fiel88$SoilWaterAdj = (SimVWC_Fiel88$SoilWater/soilDepth + 0.1)*100
  SimVWC_Fiel127 = All_HRU[All_HRU[,3] == 120007,]
  SimVWC_Fiel127$SoilWater = SimVWC_Fiel127[,17]
  SimVWC_Fiel127$SoilWaterAdj = (SimVWC_Fiel127$SoilWater/soilDepth + 0.1)*100
  SimVWC_Fiel129 = All_HRU[All_HRU[,3] == 120009,]
  SimVWC_Fiel129$SoilWater = SimVWC_Fiel129[,17]
  SimVWC_Fiel129$SoilWaterAdj = (SimVWC_Fiel129$SoilWater/soilDepth + 0.1)*100
  
  #Read in observed discharge data
  hobo = read.csv("C:/Users/Cameron Afzal/Documents/SWAT 4/hobodata.csv")
  watershedAreaM2 = 41900
  dh = ISOdate(hobo$Year, hobo$Month, hobo$Day)
  hobo$Date1 = as.Date(dh)
  hoboflow <- aggregate(x = hobo[c("Flow.cms")],
                        FUN = mean,
                        by = list(Group.date = hobo$Date1))
  hoboflow$hoboflow_cms = hoboflow$Flow.cms*(1e13)
  hoboflow$Date = as.Date(hoboflow$Group.date)

  #Read in observed soil moisture data
  infile = "C:/Users/Cameron Afzal/Documents/SWAT 4/soilmoisture.csv"
  All_ObsVWC = read.csv(infile,sep=",")
  All_ObsVWC$Date = as.Date(ISOdate(All_ObsVWC$Year,All_ObsVWC$Month,All_ObsVWC$Day))
  Panels_ObsVWC = All_ObsVWC[All_ObsVWC$Location != "Field", ]
  ObsVWC = data.frame(matrix(nrow=length(Dates)))
  ObsVWC$Date = Dates
  
  Panels_ObsVWC_Btw = Panels_ObsVWC[Panels_ObsVWC$Location == "Between",]
  ObsVWC_Btw1 = Panels_ObsVWC_Btw[Panels_ObsVWC_Btw$HRU==1,]
  ObsVWC_Btw3 = Panels_ObsVWC_Btw[Panels_ObsVWC_Btw$HRU==3,]
  ObsVWC_Btw5 = Panels_ObsVWC_Btw[Panels_ObsVWC_Btw$HRU==5,]
  ObsVWC_Btw9 = Panels_ObsVWC_Btw[Panels_ObsVWC_Btw$HRU==9,]
  ObsVWC_Btw10 = Panels_ObsVWC_Btw[Panels_ObsVWC_Btw$HRU==10,]
  Panels_ObsVWC_Und = Panels_ObsVWC[Panels_ObsVWC$Location == "Under",]
  ObsVWC_Und1 = Panels_ObsVWC_Und[Panels_ObsVWC_Und$HRU==1,]
  ObsVWC_Und3 = Panels_ObsVWC_Und[Panels_ObsVWC_Und$HRU==3,]
  ObsVWC_Und4 = Panels_ObsVWC_Und[Panels_ObsVWC_Und$HRU==4,]
  ObsVWC_Und9 = Panels_ObsVWC_Und[Panels_ObsVWC_Und$HRU==9,]
  ObsVWC_Und10 = Panels_ObsVWC_Und[Panels_ObsVWC_Und$HRU==10,]
  
  VWC_Btw1 = merge(ObsVWC_Btw1, SimVWC_Btw1, by="Date")
  VWC_Btw3 = merge(ObsVWC_Btw3, SimVWC_Btw3, by="Date")
  VWC_Btw5 = merge(ObsVWC_Btw5, SimVWC_Btw5, by="Date")
  VWC_Btw9 = merge(ObsVWC_Btw9, SimVWC_Btw9, by="Date")
  VWC_Btw10 = merge(ObsVWC_Btw10, SimVWC_Btw10, by="Date")
  VWC_Und1 = merge(ObsVWC_Und1, SimVWC_Und1, by="Date")
  VWC_Und3 = merge(ObsVWC_Und3, SimVWC_Und3, by="Date")
  VWC_Und4 = merge(ObsVWC_Und4, SimVWC_Und4, by="Date")
  VWC_Und9 = merge(ObsVWC_Und9, SimVWC_Und9, by="Date")
  VWC_Und10 = merge(ObsVWC_Und10, SimVWC_Und10, by="Date")
  All_VWC = rbind(VWC_Btw1, VWC_Btw3, VWC_Btw5, VWC_Btw9, VWC_Btw10, VWC_Und1, VWC_Und3, VWC_Und4, VWC_Und9, VWC_Und10)
  
  Field_ObsVWC = All_ObsVWC[All_ObsVWC$Location =="Field",]
  ObsVWC_Fiel1010 = Field_ObsVWC[Field_ObsVWC$HRU==10 & Field_ObsVWC$Subbasin==10,]
  ObsVWC_Fiel108 = Field_ObsVWC[Field_ObsVWC$HRU==8 & Field_ObsVWC$Subbasin==10,]
  ObsVWC_Fiel124 = Field_ObsVWC[Field_ObsVWC$HRU==4 & Field_ObsVWC$Subbasin==12,]
  ObsVWC_Fiel88 = Field_ObsVWC[Field_ObsVWC$HRU==8 & Field_ObsVWC$Subbasin==8,]
  ObsVWC_Fiel127 = Field_ObsVWC[Field_ObsVWC$HRU==7 & Field_ObsVWC$Subbasin==12,]
  ObsVWC_Fiel129 = Field_ObsVWC[Field_ObsVWC$HRU==9 & Field_ObsVWC$Subbasin==12,]

  VWC_Fie1010 = merge(ObsVWC_Fiel1010, SimVWC_Fiel1010, by="Date")
  VWC_Fie108 = merge(ObsVWC_Fiel108, SimVWC_Fiel108, by="Date")
  VWC_Fie124 = merge(ObsVWC_Fiel124, SimVWC_Fiel124, by="Date")
  VWC_Fie88 = merge(ObsVWC_Fiel88, SimVWC_Fiel88, by="Date")
  VWC_Fie127 = merge(ObsVWC_Fiel127, SimVWC_Fiel127, by="Date")
  VWC_Fie129 = merge(ObsVWC_Fiel129, SimVWC_Fiel129, by="Date")
  All_VWC_field = rbind(VWC_Fie1010, VWC_Fie108, VWC_Fie124, VWC_Fie88, VWC_Fie127, VWC_Fie129)

  #Compute likelihood function for flow
  AllData_flow = merge(ModelFlow, hoboflow, by="Date")
  AllData_flow$Res = (AllData_flow$hoboflow_cms - AllData_flow$modeled_flow)
  AllData_flow$Sim = AllData_flow$modeled_flow
  L_Results_flow = SV_Likelihood(AllData_flow,1000)
  new_l_flow = L_Results_flow[1]
  Results$L_flow[m] = new_l_flow
  GL_Params_flow = rbind(GL_Params_flow,L_Results_flow)

  #Compute likelihood function for panel VWC
  All_VWC$Res = abs(All_VWC$SWC - All_VWC$SoilWaterAdj)
  All_VWC$Sim = All_VWC$SoilWaterAdj
  L_Results_panelVWC = SV_Likelihood(All_VWC,1000)
  new_l_panelVWC = L_Results_panelVWC[1]
  Results$L_panelVWC[m] = new_l_panelVWC
  GL_Params_panelVWC = rbind(GL_Params_panelVWC,L_Results_panelVWC)
  
  #Compute likelihood function for field VWC
  All_VWC_field$Res = abs(All_VWC_field$SWC - All_VWC_field$SoilWaterAdj)
  All_VWC_field$Sim = All_VWC_field$SoilWaterAdj
  L_Results_fieldVWC = SV_Likelihood(All_VWC_field,1000)
  new_l_fieldVWC = L_Results_fieldVWC[1]
  Results$L_fieldVWC[m] = new_l_fieldVWC
  GL_Params_fieldVWC = rbind(GL_Params_fieldVWC,L_Results_fieldVWC)
  
  new_l = new_l_flow + new_l_panelVWC + new_l_fieldVWC
  Results$L_sum[m] = new_l
  
  #Calculate acceptance ratio
  if (m == 1){current_l = new_l}
  alpha = min(1,exp(new_l - current_l))
  Results$alpha[m] = alpha
  
  #Accept or Reject
  u = runif(1,0,1)
  if(u < alpha)
  {Results$Accept[m] = 1
    current_l = new_l
    #current_USLE_Mult = USLE_Mult
  }
  else
  {Results$Accept[m] = 0}
  
  #Calculate NSE
  obs <- AllData_flow$hoboflow_cms
  sim <- AllData_flow$modeled_flow
  Results$NSE[m] = 1 - (sum((sim - obs)^2))/(sum((obs - mean(obs))^2))
  
  #Write MCMC output
  write.csv(Results,paste(outpath,'SWAT_MCMC_results.csv',sep=""))
  write.csv(GL_Params_flow,paste(outpath,'SWAT_MCMC_params_flow.csv',sep=""),row.names = FALSE)
  write.csv(GL_Params_panelVWC,paste(outpath,'SWAT_MCMC_params_panelVWC.csv',sep=""),row.names = FALSE)
  write.csv(GL_Params_fieldVWC,paste(outpath,'SWAT_MCMC_params_fieldVWC.csv',sep=""),row.names = FALSE)

  if (Results$Accept[m] == 1) {
    write.csv(AllData_flow$Sim,paste(outpath, m, 'SimFlow.csv',sep=""),row.names=FALSE)
  }
  
  print(m)

}

plot(AllData_flow$Date, AllData_flow$hoboflow_cms/(1e13), type="l", col="blue", xlab="Date", ylab="Flow (cms)", main="Observed (blue) vs. Simulated (red) Streamflow", sub="SWAT MCMC Algorithm Calibration", ylim=c(0,.01))
lines(AllData_flow$Date, AllData_flow$Sim/(1e13), col="red")
summary(AllData_flow$hoboflow_cms/(1e13))
sum(AllData_flow$hoboflow_cms/(1e13))
summary(AllData_flow$Sim/(1e13))
sum(AllData_flow$Sim/(1e13))

plot(All_VWC$Date, All_VWC$SWC, ylim=c(10,45), col="blue", main="Observed(blue) vs Simulated (red) Panel VWC")
lines(All_VWC$Date, All_VWC$Sim, type="p", col="red")
plot(All_VWC_field$Date, All_VWC_field$SWC, ylim=c(10,45), col="blue", main="Observed(blue) vs Simulated (red) Field VWC")
lines(All_VWC_field$Date, All_VWC_field$Sim, type="p", col="red")


