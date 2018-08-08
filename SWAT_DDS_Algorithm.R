library("SWATmodel")
setwd("")
require("SWATmodel")


inpath = ''
outpath = ''
SWAT_inpath = ''

bsn_infile = paste(SWAT_inpath,'basins.bsn',sep="")
bsn_outfile = paste(SWAT_inpath,'basins.bsn',sep="")
rch_infile = paste(SWAT_inpath,'output.rch',sep="")  

# DDS algorithm
# Returns numIter length list of entries to be peturbed
probPeturb<-function(x, numIter){
  # Input is xBounds & numIter.  
  # Returns numIter entry list with the indices which will be peturbed
  xDims<-nrow(x)
  probabilityVector<-1-log(1:numIter)/log(numIter)
  peturbIdx<-apply(matrix(unlist(lapply(probabilityVector, function(x) as.logical(rbinom(xDims, 1, x)))), byrow=TRUE, ncol=xDims), 1, which)
  return(peturbIdx)
}

# Define Calibration Parameters and Feasible Parameter Ranges
xBounds.df = data.frame(matrix(ncol=2,nrow=11))
colnames(xBounds.df)<-c("min", "max")

# SFTMP
xBounds.df$min[1] = -3
xBounds.df$max[1] = 3

# SMTMP
xBounds.df$min[2] = -3
xBounds.df$max[2] = 3

# SMFMX
xBounds.df$min[3] = 0.01
xBounds.df$max[3] = 5

# TIMP
xBounds.df$min[4] = 0
xBounds.df$max[4] = 1

# ESCO
xBounds.df$min[5] = 0.5
xBounds.df$max[5] = 1

#EPCO
xBounds.df$min[6] = 0.5
xBounds.df$max[6] = 1

# SURLAG
xBounds.df$min[7] = 0.01
xBounds.df$max[7] = 1

# GW_DELAY
xBounds.df$min[8] = 0.01
xBounds.df$max[8] = 1

# ALPHA_BF
xBounds.df$min[9] = 0.001
xBounds.df$max[9] = 0.5

# CN_Mult
xBounds.df$min[10] = 0.1
xBounds.df$max[10] = 1

# SOL_ALB
xBounds.df$min[11] = 0.05
xBounds.df$max[11] = 0.9

#Initial Parameter Values

# Generate initial first guess
#xBounds.df<-data.frame(col1 = rep(10,10), col2=rep(100, 10))
x_init<-c(-3, -3, 0.01, 0, 0.5, 0.5, 0.01, 0.01, 0.001, 0.1, 0.05)
x_best = data.frame(x_init)

# Evaluate first cost function
NSE_init = -9999
NSE_best <- NSE_init

r= 0.2
numIter = 5
runs = numIter
Results = data.frame(matrix(nrow = runs))

# Select which entry to peturb at each iteration

peturbIdx<-probPeturb(xBounds.df, numIter)
# Peturb each entry by N(0,1)*r(x_max - x_min) reflecting if @ boundaries
sigma<-xBounds.df$max - xBounds.df$min
  
for (j in 1:numIter)
  {

    # Set up test x
    x_test<-as.matrix(x_best)
    
    # Get entries we will peturb
    if (length(peturbIdx[[j]]) == 0 || is.null(peturbIdx[[j]]))
    {
      peturbIdx[[j]] = as.integer(runif(1,1,10))
    }
    
    idx<-peturbIdx[[j]]
    
    #if (idx == 0)
    #{
    #  idx = 1
    #}
    
    # Initialize vector of peturbations initially zeros with same length of x so we will add this vector to peturb x
    peturbVec<-rep(0, length(x_test))
    # Generate the required number of random normal variables
    N<-rnorm(length(x_test), mean=0, sd=1)
    
    # Set up vector of peturbations
    peturbVec[idx]<-r*N[idx]*sigma[idx]
    
    # Temporary resulting x value if we peturbed it
    testPeturb<-x_test + peturbVec  
    # Find the values in testPeturb that have boundary violations.  Store the indices in boundaryViolationsIdx
    boundaryViolationIdx<-which(testPeturb<xBounds.df$min | testPeturb > xBounds.df$max)
    
    # Reset those violated indices to the opposite peturbation direction
    peturbVec[boundaryViolationIdx]<-(-1*r*N[boundaryViolationIdx]*sigma[boundaryViolationIdx])
    
    # Find values still at violations of min or max and set them to the minimum or maximum values
    testPeturb<-x_test + peturbVec
    minViolationIdx<-which(testPeturb<xBounds.df$min)
    maxViolationIdx<-which(testPeturb>xBounds.df$max)
    testPeturb[minViolationIdx]<-xBounds.df$min[minViolationIdx]
    testPeturb[maxViolationIdx]<-xBounds.df$max[maxViolationIdx]
    
    # Peturb the test vector
    x_test<-x_test + peturbVec  
    
    #SFTMP = x_best$x_test[1]
    
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
    #runSWAT2012()
    system("swat2012.exe")
    
    #Specify SWAT reach to keep
    SWAT_Reach = 14
    
    #Read in SWAT model output .rch file
    output_raw = read.delim(rch_infile,skip=9,sep="")
    output.df = data.frame(output_raw)
    All_Flow = output.df[,2:9]
    Reach_Flow = All_Flow[All_Flow[,1] == SWAT_Reach,]
    Dates = seq(as.Date("2013/06/04"),as.Date("2018/06/03"),"days")
    ModelFlow = data.frame(matrix(nrow=length(Dates)))
    ModelFlow$Date = Dates
    ModelFlow$modeled_flow = Reach_Flow[,7]
    
    #Read in observed data
    hobo = read.csv("C:/Users/Cameron Afzal/Documents/SWAT 4/hobodata.csv")
    watershedAreaM2 = 41900
    #output$flowDepth = ((output$FLOW_OUTcms*86400)/41900)*1000
    #d = ISOdate(output$YR, output$MO, output$DA)
    #output$Date = as.Date(d)
    #precip$Date = output$Date
    dh = ISOdate(hobo$Year, hobo$Month, hobo$Day)
    hobo$Date1 = as.Date(dh)
    hoboflow <- aggregate(x = hobo[c("Flow.cms")],
                          FUN = mean,
                          by = list(Group.date = hobo$Date1))
    hoboflow$hoboflow_cms = hoboflow$Flow.cms
    hoboflow$Date = as.Date(hoboflow$Group.date)
    
    #Merge both datasets
    AllData = merge(ModelFlow, hoboflow, by="Date")
    AllData$Res = AllData$hoboflow_cms - AllData$modeled_flow
    #AllData$Res_Norm = 0
    #for (c in 2:nrow(AllData))
    #{AllData$Res_Norm[c] = AllData$Res[c] - phi*AllData$Res[c-1]}
    #sigma_t = sigma_0 + sigma_1*AllData$modeled_flow
    #AllData$Res_Norm_Hom = AllData$Res_Norm/sigma_t
    #AllData$Res_Norm_Hom[1] = 0
    Results$mdate = Results$Date
    
    #Step 4: Code in the NSE and pass it through to the algorithm in the next if statement
    
    obs <- AllData$hoboflow_cms
    sim <- AllData$modeled_flow
    
    NSE = 1 - (sum((sim - obs)^2))/(sum((obs - mean(obs))^2))
    
    #Check if this simulation is better
    if (NSE > NSE_best)
    {
      x_best = x_test
      NSE_best = NSE      
    }
    print_str = paste("Eval:",j,"   NSE:",NSE,"   NSE Best:",NSE_best) 
    print(print_str)
}

plot(AllData$Date, obs, type="l", col="blue", xlab="Date", ylab="Flow (cms)", main="Observed (blue) vs. Simulated (red) Streamflow", sub="SWAT DDS Algorithm Calibration", ylim=c(0,.01))
lines(AllData$Date, sim, col="red")