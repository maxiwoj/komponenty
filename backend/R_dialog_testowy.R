###MODEL SINGLE DOSE; prototype DIALOG. 16/03/2019

#REQUIRED PACKAGES:
require(distr)
require(data.table)
require(dplyr)
require(deSolve)

#PARAMETERS:
#oral parameters:
#oral bolus dose [mg]
oral_dose <- 880 #1000
inf_dose <- 0
inf_time <- 2
#define number of individuals:
individual_count <- 1
#define number of females:
female_count <- 0
#define age range [years]:
min_age <- 24  #minimal age
max_age <- 24 #maximal age
#time of the end of simulation [h]:
t_end <- 15


set.seed(1111)




#Weibull age distribution parameters:
scale_M <- 24.7 #scale parameter for males' age distribution
shape_M <- 2.1 #shape parameter for males' age distribution
scale_F <- 24.7 #scale parameter for females' age distribution
shape_F <- 1.9 #shape parameter for females' age distribution

a <- c(0.2434986, 1.3321865) #Olek 16.02.2018


#Functions to transform normal distribution into log-normal
#Functions of mean and CV:
#meanlog:
m_lognormal <-
  function(m, cv) {
    (log(m) - 0.5 * log(1 + (cv * m) ^ 2 / (m ^ 2)))
  }
#sdlog:
s_lognormal <- function(m, cv) {
  (sqrt(log(1 + (cv * m) ^ 2 / (m ^ 2))))
}

#Functions of mean and SD:
#meanlog:
m_lognormal_2 <-
  function(m, stdev) {
    (log(m) - 0.5 * log(1 + (stdev) ^ 2 / (m ^ 2)))
  }
#sdlog:
s_lognormal_2 <- function(m, stdev) {
  (sqrt(log(1 + (stdev) ^ 2 / (m ^ 2))))
}


#FULL PBPK MODEL ACCOUNTING FOR INTERINDIVIDUAL VARIABILITY
males_count <-
  individual_count - female_count #number of males in the study

#AGE RANDOMIZATION
if (min_age != max_age) {
  d_age_M <-
    Weibull(shape = shape_M, scale = scale_M) #distribution of males age
  d_age_F <-
    Weibull(shape = shape_F, scale = scale_F) #distribution of females age
  plot(d_age_M, main = "Males' age distribution")
  plot(d_age_F, main = "Females' age distribution")
  d_age_M_truncate <-
    Truncate(d_age_M, lower = min_age, upper = max_age)
  d_age_F_truncate <-
    Truncate(d_age_F, lower = min_age, upper = max_age)
  assign('M_age', round(d_age_M_truncate@r(males_count)))
  assign('F_age', round(d_age_F_truncate@r(female_count)))
  if (males_count > 0) {
    hist(M_age, main = "Histogram of men's age", xlab = "age")
  }
  if (female_count > 0) {
    hist(F_age, main = "Histogram of women's age", xlab = "age")
  }
} else {
  assign('M_age', rep(min_age, males_count))
  assign('F_age', rep(min_age, female_count))
  if (males_count > 0) {
    hist(M_age, main = "Histogram of men's age", xlab = "age")
  }
  if (female_count > 0) {
    hist(F_age, main = "Histogram of women's age", xlab = "age")
  }
}




#HEIGHT RANDOMIZATION [cm] FROM SEX- AND AGE-DEPENDENT NORMAL DISTRIBUTIONS. DISTRIBUTIONS ACCORDING TO [Millar 1986]
category_F_height <- function(x)
{
  if (x <= 29)
    round(rnorm(n = 1, 162, 6.4), digits = 1)
  else if (x >= 30 &
           x <= 39)
    round(rnorm(1, 161.4, 6.3), digits = 1)
  else if (x >= 40 &
           x <= 49)
    round(rnorm(1, 160.9, 6.5), digits = 1)
  else if (x >= 50 &
           x <= 59)
    round(rnorm(1, 159.7, 6.4), digits = 1)
  else if (x >= 60)
    round(rnorm(1, 157.6, 6.4), digits = 1)
  else
    round(rnorm(1, 160.7, 6.6), digits = 1)
}

category_M_height <- function(x) {
  if (x <= 29)
    round(rnorm(1, 175.8, 6.8), digits = 1)
  else if (x >= 30 &
           x <= 39)
    round(rnorm(1, 174.8, 6.7), digits = 1)
  else if (x >= 40 &
           x <= 49)
    round(rnorm(1, 173.6, 7.4), digits = 1)
  else if (x >= 50 &
           x <= 59)
    round(rnorm(1, 172.9, 6.4), digits = 1)
  else if (x >= 60)
    round(rnorm(1, 171.2, 6.4), digits = 1)
  else
    round(rnorm(1, 174.1, 7), digits = 1)
}
assign('F_height', as.numeric(lapply(F_age, category_F_height)))
assign('M_height', as.numeric(lapply(M_age, category_M_height)))

if (males_count > 0) {
  hist(M_height, main = "Histogram of men's height", xlab = "height [kg]")
}
if (female_count > 0) {
  hist(F_height, main = "Histogram of women's height", xlab = "height [kg]")
}


#BODY WEIGHT RANDOMIZATION [kg] FROM SEX- AND AGE-DEPENDENT NORMAL DISTRIBUTIONS (TRUNCATED AT 30). DISTRIBUTIONS ACCORDING TO [Millar 1986]
# WEIGHT ------------------------------------------------------------------
category_F_weight <-
  function(x) {
    if (x <= 29)
      round(Truncate(Norm(57.2, 8.8), lower = 30)@r(1), digits = 1)
    else if (x >= 30 &
             x <= 39)
      round(Truncate(Norm(61.2, 11.3), lower = 30)@r(1), digits = 1)
    else if (x >= 40 &
             x <= 49)
      round(Truncate(Norm(63.7, 11.9), lower = 30)@r(1), digits = 1)
    else if (x >= 50 &
             x <= 59)
      round(Truncate(Norm(65.2, 11.2), lower = 30)@r(1), digits = 1)
    else if (x >= 60)
      round(Truncate(Norm(64, 10.8), lower = 30)@r(1), digits = 1)
    else
      round(Truncate(Norm(61.5, 11.1), lower = 30)@r(1), digits = 1)
  }
category_M_weight <-
  function(x) {
    if (x <= 29)
      round(Truncate(Norm(73.9, 11.3), lower = 30)@r(1), digits = 1)
    else if (x >= 30 &
             x <= 39)
      round(Truncate(Norm(77.1, 12.4), lower = 30)@r(1), digits = 1)
    else if (x >= 40 &
             x <= 49)
      round(Truncate(Norm(78.8, 12.9), lower = 30)@r(1), digits = 1)
    else if (x >= 50 &
             x <= 59)
      round(Truncate(Norm(78.3, 11.2), lower = 30)@r(1), digits = 1)
    else if (x >= 60)
      round(Truncate(Norm(78.1, 11.9), lower = 30)@r(1), digits = 1)
    else
      round(Truncate(Norm(76.7, 12.1), lower = 30)@r(1), digits = 1)
  }
assign('F_weight', as.numeric(lapply(F_age, category_F_weight)))
assign('M_weight', as.numeric(lapply(M_age, category_M_weight)))
if (males_count > 0) {
  hist(M_weight, main = "Histogram of men's weight", xlab = "weight [kg]")
}
if (female_count > 0) {
  hist(F_weight, main = "Histogram of women's weight", xlab = "weight [kg]")
}


assign('Population',
       data.frame(
         sex = c(rep("M", males_count), rep("F", female_count)),
         age = c(M_age, F_age),
         height = c(M_height, F_height),
         weight = c(M_weight, F_weight)
       ))


#CARDIAC OUTPUT IS AGE, SEX AND SURFACE AREA DEPENDANT. IT IS CALCULATED ALGEBRAICALLY ACCORDING TO THE FORMULAS IN [Tanner 1949]
# CARDIAC OUTPUT ----------------------------------------------------------
Population["BSA"] <-
  (Population$weight ^ 0.425 * Population$height ^ 0.725) * 0.007184 #Body surface area according to [DuBois-DuBois 1916]
Population["mean CO Tanner formula [L/min]"] <-
  ifelse(
    Population$sex == "M",
    (1.1 * Population$BSA - 0.05 *
       Population$age + 5.5),
    (1.7 * Population$BSA - 0.04 * Population$age + 3.5)
  )

Population["mean CO Tanner formula [L/h]"] <-
  Population$`mean CO Tanner formula [L/min]` * 60 #units from [L/min] to [L/h]
hist(Population$`mean CO Tanner formula [L/h]`) #display a histogram of cardiac output

#SEX DEPENDENT BLOOD FLOWS for healthy population according to [Simycp Simulator v.16]
# BLOOD FLOWS [L/h] -------------------------------------------------------
Population["Qad [L/h]"] <-
  ifelse(
    Population$sex == "M",
    Population$`mean CO Tanner formula [L/h]` * 0.05,
    Population$`mean CO Tanner formula [L/h]` * 0.085
  )
Population["Qbo [L/h]"] <-
  Population$`mean CO Tanner formula [L/h]` * 0.05
Population["Qbr [L/h]"] <-
  Population$`mean CO Tanner formula [L/h]` * 0.12
Population["Qgu [L/h]"] <-
  ifelse(
    Population$sex == "M",
    Population$`mean CO Tanner formula [L/h]` * 0.16,
    Population$`mean CO Tanner formula [L/h]` * 0.17
  )
Population["Qhe [L/h]"] <-
  ifelse(
    Population$sex == "M",
    Population$`mean CO Tanner formula [L/h]` * 0.04,
    Population$`mean CO Tanner formula [L/h]` * 0.05
  )
Population["Qki [L/h]"] <-
  ifelse(
    Population$sex == "M",
    Population$`mean CO Tanner formula [L/h]` * 0.19,
    Population$`mean CO Tanner formula [L/h]` * 0.17
  )

Population["Qh [L/h]"] <-
  ifelse(
    Population$sex == "M",
    Population$`mean CO Tanner formula [L/h]` * 0.19,
    Population$`mean CO Tanner formula [L/h]` * 0.215
  )
Population["Qlu [L/h]"] <- Population$`mean CO Tanner formula [L/h]`
Population["Qmu [L/h]"] <-
  ifelse(
    Population$sex == "M",
    Population$`mean CO Tanner formula [L/h]` * 0.17,
    Population$`mean CO Tanner formula [L/h]` * 0.12
  )
Population["Qsk [L/h]"] <-
  Population$`mean CO Tanner formula [L/h]` * 0.05
Population["Qsp [L/h]"] <-
  ifelse(
    Population$sex == "M",
    Population$`mean CO Tanner formula [L/h]` * 0.02,
    Population$`mean CO Tanner formula [L/h]` * 0.03
  )
Population["Qre [L/h]"] <-
  ifelse(
    Population$sex == "M",
    Population$`mean CO Tanner formula [L/h]` * 0.14,
    Population$`mean CO Tanner formula [L/h]` * 0.14
  )
#control whether the sum of tissue blood flowa is equal to total cardiac output
Population["CO - Q sum"] <-
  Population$`mean CO Tanner formula [L/h]` - (apply(Population[, c(8:10, 12:14, 16:17, 19)], 1, sum))


# ORGAN VOLUMES -----------------------------------------------------------
#BW fraction (according to Simcyp Simulator) * random BW / tissue density
Population["Vad [L]"] <- (0.259 * Population$weight) / 0.923
Population["Vbo [L]"] <- (0.090 * Population$weight) / 1.850
Population["Vbr [L]"] <- (0.017 * Population$weight) / 1.04
Population["Vgu [L]"] <- (0.016 * Population$weight) / 1.04
Population["Vhe [L]"] <- (0.005 * Population$weight) / 1.04
Population["Vki [L]"] <- (0.004 * Population$weight) / 1.05
Population["Vli [L]"] <- (0.022 * Population$weight) / 1.08
Population["Vlu [L]"] <- (0.007 * Population$weight) / 1.05
Population["Vmu [L]"] <- (0.403 * Population$weight) / 1.04
Population["Vsk [L]"] <- (0.043 * Population$weight) / 1.1
Population["Vsp [L]"] <- (0.002 * Population$weight) / 1.06
Population["Vre [L]"] <- (0.057 * Population$weight) / 1.05
Population["Vpl [L]"] <- (0.044 * Population$weight) / 1.025
Population["Vrb [L]"] <- (0.031 * Population$weight) / 1.125
Population["Vbl [L]"] <- Population$`Vpl [L]` + Population$`Vrb [L]`

#control the sum of body volumes:
Population["V sum"] <- apply(Population[, c(21:34)], 1, sum)

# MPPGL according to [Barter 2008] -------------------------------------------------------------------
Population["mean MPPGL [mg/g]"] <-
  10 ^ (
    1.407 + 0.0158 * Population$age - 0.00038 * (Population$age ^ 2) + 0.0000024 *
      (Population$age ^ 3)
  )

RANDOM_MPPGL <- function(x) {
  Lnorm(m_lognormal(x, 0.269), s_lognormal(x, 0.269))@r(1)
}  #CV=26.9%

Population["random MPPGL [mg/g]"] <-
  as.numeric(lapply(Population$`mean MPPGL [mg/g]`, RANDOM_MPPGL))

# CYPs IN HEART TISSUE according to [deLozier 2007] ----------------------------------------------------
Population["CYP2C8_H [pmol/mg tissue]"] <-
  cbind(Lnorm(m_lognormal_2(0.2, 0.02), s_lognormal_2(0.2, 0.02))@r(individual_count))
Population["CYP2C9_H [pmol/mg tissue]"] <-
  cbind(Lnorm(m_lognormal(5.5, 0.3), s_lognormal(5.5, 0.3))@r(individual_count)) #CV=30% assumption
Population["CYP2J2_H [pmol/mg tissue]"] <-
  cbind(Lnorm(m_lognormal_2(0.17, 0.05), s_lognormal_2(0.17, 0.05))@r(individual_count)) #CV=30% assumption



# CYPs in LIVER according to [Simcyp Simulator v.16] -----------------------------------------------------------
#drawn from log-normal distributions

Population["CYP1A2_L [pmol/mg protein]"] <-
  cbind(Lnorm(m_lognormal(52, 0.67), s_lognormal(52, 0.67))@r(individual_count))
Population["CYP2B6_L [pmol/mg protein]"] <-
  cbind(Lnorm(m_lognormal(17, 1.22), s_lognormal(17, 1.22))@r(individual_count))
Population["CYP2C8_L [pmol/mg protein]"] <-
  cbind(Lnorm(m_lognormal(24, 0.81), s_lognormal(24, 0.81))@r(individual_count))
Population["CYP2C9_L [pmol/mg protein]"] <-
  cbind(Lnorm(m_lognormal(73, 0.54), s_lognormal(73, 0.54))@r(individual_count))
Population["CYP2C19_L [pmol/mg protein]"] <-
  cbind(Lnorm(m_lognormal(14, 1.06), s_lognormal(14, 1.06))@r(individual_count))
Population["CYP2D6_L [pmol/mg protein]"] <-
  cbind(Lnorm(m_lognormal(8, 0.61), s_lognormal(8, 0.61))@r(individual_count))
Population["CYP3A4_L [pmol/mg protein]"] <-
  cbind(Lnorm(m_lognormal(137, 0.41), s_lognormal(137, 0.41))@r(individual_count))






#LAG TIME OF ABSORPTION and BIOAVAILABILITY
# tlag and Fabs --------------------------------------------------------------------
Population["tlag"] <-
  cbind(Lnorm(m_lognormal(a[2], 0.3), s_lognormal(a[2], 0.3))@r(individual_count)) #time of gastric emptying
Population["Bioavailability"] <-
  cbind((Truncate(
    Norm(45.9, 9.3), lower = 33, upper = 62
  )@r(individual_count)) / 100) #BIOAVAILABILITY: F oral bioavailability of AMI is highly variable, ranging from 33 to 62% in humans
#Population["T_si"] <-
#  cbind(Lnorm(m_lognormal(3.4, 0.0919), s_lognormal(3.4, 0.0919))@r(individual_count)) #[h]
Population["FaFg"] <-
  cbind(Lnorm(m_lognormal(0.832, 0.131), s_lognormal(0.832, 0.131))@r(individual_count)) #[h]
######A BUG -> 12.12.2018 s_lognormal(3.832, 0.131) -> s_lognormal(0.832, 0.131)


#FREE FRACTION AND BLOOD TO PLASMA RATIOS OF AMITRIPTYLINE AND NORTRIPTYLINE
# fup and BP --------------------------------------------------------------------
Population["fup"] <-
  cbind(Lnorm(m_lognormal_2(0.060, 0.018), s_lognormal_2(0.060, 0.018))@r(individual_count)) #distribution according to [Rollins 1980, Baumann 1986, Brinkschulte 1982, Burch 1981, Schulz 1983] - pooled data
Population["BP"] <-
  cbind(Lnorm(m_lognormal_2(0.877, 0.168), s_lognormal_2(0.877, 0.168))@r(individual_count)) #distribution acording to [Rollins 1980, Schulz 1983] - pooled data
Population["BP_nor"] <-
  cbind(Lnorm(m_lognormal_2(1.97, 0.22), s_lognormal_2(1.97, 0.22))@r(individual_count)) #distribution according to [Rollins 1980]
Population["fup_nor"] <-
  0.8231 * Population$fup + 0.0394 #nortriptyline free fraction dependent on amitriptyline free fraction

# CARDIOMYOCYTE VOLUME AND SURFACE AREA according to [Polak 2012] --------------------------------------------------------------------
MV <- function(x) {
  #cr inline or give methods meaningful names
  sqrt(0.29602 ^ 2 + (c(1, x) %*% matrix(
    c(38.12228,-0.66728,-0.66728, 0.01639),
    nrow = 2,
    ncol = 2
  ) %*% t(t(c(
    1, x
  ))) / 1000))
}
Population["MV_SE"] <-
  cbind(mapply(MV, Population$age)) #standard deviation of myocyte volume
Population["MV [mcm^3]"] <-
  cbind(exp(rnorm(
    individual_count,
    (Population$age * 0.04551 + 7.36346),
    Population$MV_SE
  )))
Population["MSA [mcm^2]"] <- cbind(exp(rnorm(
  individual_count, 0.860 * log(Population$`MV [mcm^3]`),
  ((sqrt(
    0.102 ^ 2 + (log(Population$`MV [mcm^3]`)) ^ 2 * 0.002939 ^ 2
  )))
)))

Population["SA_pf [cm^2]"] <-
  cbind(2 * (0.87 * Population$height + 0.34 * Population$weight - 63.8))

# PERICARDIAL FLUID VOLUME ------------------------------------------------

pop2 <-
  Population[order(Population$weight), ] #order the population data according to BW
pop2["Vpf [L]"] <-
  sort((Lnorm(
    m_lognormal(0.03, 0.3), s_lognormal(0.03, 0.3)
  )@r(individual_count))) #random pericardial fluid volume assigned to the BW in ascending order
pop3 <-
  pop2[order(as.numeric(row.names(pop2))), ] #rearranging the table with virtual population data according to individual numbers
setDT(pop3, keep.rownames = TRUE)[]
pop3$rn <- as.numeric(as.character(pop3$rn))

# DOCKER ERROR 
# View(pop3)

# MODEL -------------------------------------------------------------------

#Arguments of the function are the model parameters that vary
ModelVar <- function (BW,
                      CO,
                      MPPGL,
                      Vad,
                      Vbl,
                      Vrb,
                      Vbo,
                      Vbr,
                      Vgu,
                      Vheart,
                      Vki,
                      Vli,
                      Vlu,
                      Vpl,
                      Vmu,
                      Vsk,
                      Vsp,
                      Qad,
                      Qbo,
                      Qbr,
                      Qgu,
                      Qheart,
                      Qki,
                      Qh,
                      Qlu,
                      Qmu,
                      Qsk,
                      Qsp,
                      Vpf,
                      CYP2C8_H,
                      CYP2C9_H,
                      CYP2J2_H,
                      Vre,
                      Qre,
                      CYP1A2_L,
                      CYP2B6_L,
                      CYP2C8_L,
                      CYP2C9_L,
                      CYP2C19_L,
                      CYP2D6_L,
                      CYP3A4_L,
                      #CYP2C9_I,
                      #CYP2C19_I,
                      #CYP2D6_I,
                      #CYP3A4_I,
                      tlag,
                      Bioavailability,
                      #T_si,
                      FaFg,
                      t_end,
                      oral_dose,
                      fup,
                      fup_nor,
                      BP,
                      BP_nor,
                      MV,
                      MSA,
                      SA_pf)
{
  times <- seq(0, t_end, by = 0.1)
  x <-
    c(28.73022,
      0.1230352,
      0.1575158,
      2.996919,
      2.177201) #vector of optimized parameters: Kpre_nor, fuha, fuhn, CLefflux, CLuptake
  
  
  # PHYSICO-CHEMICAL PARAMETERS OF AMITRIPTYLINE AND NORTRIPTYLINE ------------------------------------------------------------------
  #AMITRIPTILINE
  MW_ami <- 277.4 #[g/mol] Molecular weight
  pKa_ami <- 9.41 # pKa [Cantu 2005]
  HBD <-
    0#number of hydrogen bond donors: [PubChem Compound Database; CID = 2160];
  PSA <-
    3.2 #Polar Surface Area:  [PubChem Compound Database; CID = 2160]
  logD55 <-
    1.22 #octanol/water distribution coefficient at pH 5.5: calculated; [Tsopelas 2015]
  #NORTRIPTYLINE
  MW_nor <- 263.384 #[g/mol] [PubChem Compound Database; CID = 4543]
  pKa_nor <- 10.1 #pKa [Cantu 2005]
  
  # PHYSIOLOGICAL PARAMETERS ------------------------------------------------------------------
  #Tissue densities:
  liver_density <- 1080 #[g/L]
  heart_density <- 1050 #[g/L] [Kim 2005, Yan 2006]
  #Tissue volumes [L]
  #Full PBPK:
  Vhe = Vheart - Vpf #heart
  Vendo = 0.5 * Vhe #endocardium
  Vmid = 0.3 * Vhe #midmyocaridum
  Vepi = 0.2 * Vhe #epicardium
  Vendo_ic = 87.5 / 100 * Vendo #intracellular space of endocardium [Sjogaard 1982] -> data for skeletal muscle; calculated the proportion of extracellular water to total water in skeletal muscle
  Vepi_ic = 87.5 / 100 * Vepi #intracellular space of epicardium
  Vmid_ic = 87.5 / 100 * Vmid #intracellular space of midmyocardium
  Vhe_ec = 12.5 / 100 * Vhe #extracellular space of heart tissue
  Vve = (2 / 3) * Vbl		#venous blood; assumed 2/3 of total blood according to volmues published in CPT. Regarding the distribution of blood volume within the circulation, the greatest volume resides in the venous vasculature, where 70-80% of the blood volume is found. -> http://www.cvphysiology.com/Blood%20Pressure/BP019
  Var = Vbl - Vve		#arterial blood
  Vplas_ven = Vpl * (Vve / (Vve + Var))  #venous plasma
  Vplas_art = Vpl * (Var / (Vve + Var)) 	#arterial plasma
  #myocyte volume
  ML <- 134 #[mcm] myocyte length [Tracy 2011, Gerdes 1995]
  MB <- ML / 7 #=2r [mcm] myocyte breadth ML:MB = 7:1 [Gerdes 1995]
  MVol <-
    MV * (10 ^ -15) #[L] random age dependent myocate volume in mcm3 -> changing to liters
  #cells amounts
  cell_amount_epi <- Vepi_ic / MVol #epicardium
  cell_amount_mid <- Vmid_ic / MVol #midmyocardium
  cell_amount_endo <- Vendo_ic / MVol #endocardium
  #surface area
  SA_epi <- (cell_amount_epi * MSA) / (10 ^ 8) #[cm^2]
  SA_mid <- (cell_amount_mid * MSA) / (10 ^ 8) #[cm^2]
  SA_endo <- (cell_amount_endo * MSA) / (10 ^ 8) #[cm^2]
  
  #Minimum-PBPK:
  V1 = BW - (Var + Vve) - Vli - Vhe #rest of the body compartment
  # Tissue blood flows
  Qpf = 0.011 # pericardium: fitted in [Tylutki 2017]
  Qhe = Qheart - Qpf     #heart
  Qha = Qh - Qgu - Qsp #hepatic artery
  QC  = CO - Qh - Qhe #remaining compartment in minimum-PBPK
  # PARAMETERS FOR ICF and ECF in heart tissue
  pH_ic <- 7.2 #[Vaugha-Jones 2009, Zheng 2005]
  pH_ec <- 7.4 #[Vaugha-Jones 2009, Zheng 2005]
  pH_pf <-
    7.57 #Hutchin 1971; SD=0.11 #11 patients with heart lesions; no pericardial effusion
  #amitriptyline - base;
  #Henderson_Hasselbalch equation for base compound -> fraction of un-ionized base in heart compartments:
  funionized_ic <- 1 / (1 + 10 ^ (pKa_ami - pH_ic))
  funionized_ec <- 1 / (1 + 10 ^ (pKa_ami - pH_ec))
  funionized_pf <- 1 / (1 + 10 ^ (pKa_ami - pH_pf))
  
  
  # DOSING and ABSORPTION ------------------------------------------------------------------
  #IV Bolus Dose [mg]
  IVDOSE = 0
  #IV INFUSION RATE
  r = inf_dose# [mg]
  t = inf_time #time of infusion [h]
  inf = r / t #infusion rate [mg/h]
  #Oral Bolus Dose [mg]
  PODOSE = oral_dose
  
  #Absorption:
  PAMPA <- 12.3 * 10 ^ -6 #[cm/s] [Oja 2015]
  Pdiff_ami <- PAMPA #[cm/s]
  
  
  #metabolism kinetics
  #AMITRIPTILINE (A)
  #LIVER (L)
  #N-demethylation
  #Vmax for amitriptyline after [Venkatakrishnan 2001] [pmol/min/pmol CYP]
  #Km for amitriptyline [mcM]
  # 1.CYP1A2
  V_1A2_ami <- 1.79 * MW_ami * 10 ^ -9 #[mg/min/pmol CYP]
  K_1A2_ami <- 63.5 * MW_ami * 10 ^ -3 #[mg/L]
  # 2.CYP2B6
  V_2B6_ami <- 0.25 * MW_ami * 10 ^ -9 #[mg/min/pmol CYP]
  K_2B6_ami <- 56.7 * MW_ami * 10 ^ -3 #[mg/L]
  # 3.CYP2C8
  V_2C8_ami <- 0.7 * MW_ami * 10 ^ -9 #[mg/min/pmol CYP]
  K_2C8_ami <- 9.74 * MW_ami * 10 ^ -3 #[mg/L]
  # 4.CYP2C9
  V_2C9_ami <- 3.97 * MW_ami * 10 ^ -9 #[mg/min/pmol CYP]
  K_2C9_ami <- 50.5 * MW_ami * 10 ^ -3 #[mg/L]
  # 5.CYP2C19
  V_2C19_ami <- 4.22 * MW_ami * 10 ^ -9 #[mg/min/pmol CYP]
  K_2C19_ami <- 8.52 * MW_ami * 10 ^ -3 #[mg/L]
  # 6.CYP2D6
  V_2D6_ami <- 1.49 * MW_ami * 10 ^ -9 #[mg/min/pmol CYP]
  K_2D6_ami <- 7.12 * MW_ami * 10 ^ -3 #[mg/L]
  # 7.CYP3A4
  V_3A4_ami <-
    3.37 * MW_ami * 10 ^ -9 #[mg/min/pmol CYP]#[Ghahramani 1997]
  K_3A4_ami <- 213.8 * MW_ami * 10 ^ -3 #[mg/L]
  #E-10-hydroxylation:
  #1.CYP2B6
  V_2B6_ami_h <- 0.13 * MW_ami * 10 ^ -9 #[mg/min/pmol CYP]
  K_2B6_ami_h <- 98 * MW_ami * 10 ^ -3 #[mg/L]
  #2.CYP2D6
  V_2D6_ami_h <- 2.71 * MW_ami * 10 ^ -9 #[mg/min/pmol CYP]
  K_2D6_ami_h <- 4.75 * MW_ami * 10 ^ -3 #[mg/L]
  #3.CYP3A4
  V_3A4_ami_h <- 0.4 * MW_ami * 10 ^ -9 #[mg/min/pmol CYP]
  K_3A4_ami_h <- 69.3 * MW_ami * 10 ^ -3 #[mg/L]
  
  
 
  
  
  
  # DISTRIBUTION ---------------------------------------------------------
  #Drug binding
  #Amitriptyline:
  fu_pf = fup #fraction unbound in pericardial fluid; equality assumed. Unknown. proteins in PF < proteins in blood;
  fu_ht_ami = 0.0012 #fraction of amitriptyline unbound in heart tissue; [Mikkelsen 2017]
  fu_ec = fu_pf #fraction unbound in extracellular fluid assumed to equal free fraction in pericardial fluid
  
  
  # TISSUE TO PLASMA PARTITION COEFFICIENT ----------------------------------
  #Calculated in Simcyp Simulator v.16, Method 2
  #Amitriptyline:
  Kpad = 4.10# adipose
  Kpbo = 4.14# bone
  Kpbr = 3.05# brain
  Kpgu = 11.73# gut
  Kphe = 11.77 # heart
  Kppf = 2.6 #[Moriya 2000]; postmortem data
  Kpec = 1 #partition coefficient between plasma and extracellular fluid
  Kpki = 9.79# kidney
  Kpli = 19.80# liver
  Kplu = 2.05# lung
  Kpmu = 9.85# muscle
  Kpsk = 5.61# skin
  Kpsp = 11.02# spleen
  Kpre = 1 #rest of the body
  #Nortriptyline TISSUE TO BLOOD PARTITION COEFFICIENT:
  Kpli_nor = 59.08 / BP_nor# liver
  Kphe_nor = 35.63 / BP_nor# heart
  
  #passive permeability surface area product in heart tissue
  PSA_pf = Pdiff_ami * SA_pf * (10 ^ -3) * 60 * 60 #[L/h] passive permeability surface area product
  PSA_epi = Pdiff_ami * SA_epi * (10 ^ -3) * 60 * 60 #[L/h] passive permeability surface area product
  PSA_mid = Pdiff_ami * SA_mid * (10 ^ -3) * 60 * 60 #[L/h] passive permeability surface area product
  PSA_endo = Pdiff_ami * SA_endo * (10 ^ -3) * 60 * 60 #[L/h] passive permeability surface area product
  
  # ELIMINATION --------------------------------------------------------------
  #1) LIVER
  #ISEF
  #values from Simcyp. rCYP system: Lymph B
  ISEF1A2 = 11.1
  ISEF2C19 = 3.07
  ISEF2D6 = 0.74
  ISEF2C9 = 5.73
  ISEF3A4 = 3.92
  ISEF2B6 = 3.7
  ISEF2C8 = 3.7
  #fumic
  fumic <- 0.82# [Venkatakrishnan 2001]
  fumic_nor <-
    0.82 # the same value as for ami -> [McLure 2002]: ami and nor the same ranges 0.3-0.7
  #2) KIDNEY
  CLrenal = 0.09 #CL renal [L/h]; [Karkkainen 1986]
  CLnonhep_nor = 0.39 #[L/h] [Jornil 2011]
  #3) HEART
  CYP3A4_H <-
    0 #CYP 3A4 abundance in average human heart [pmol/mg tissue][Thum 2000]
  
  
  # MODEL -------------------------------------------------------------------
  parameters <- c(
    BP = BP,
    Kpad = Kpad,
    Kpbo = Kpbo,
    Kpbr = Kpbr,
    Kpgu = Kpgu,
    Vendo = Vendo,
    Vmid = Vmid,
    Vepi = Vepi,
    Kppf = Kppf,
    Kpki = Kpki,
    CLrenal = CLrenal,
    fup = fup,
    Qgu = Qgu,
    Kpgu = Kpgu,
    Kpsp = Kpsp,
    Kpli = Kpli,
    Kplu = Kplu,
    Kpmu = Kpmu,
    Kpsk = Kpsk,
    PODOSE = PODOSE,
    funionized_ic = funionized_ic,
    funionized_ec = funionized_ec,
    funionized_pf = funionized_pf,
    fu_ec = fu_ec,
    PSA_pf = PSA_pf,
    PSA_epi = PSA_epi,
    PSA_mid = PSA_mid,
    PSA_endo = PSA_endo,
    Kpec = Kpec
  )
  
  # State variables
  state <- c(
    INFUSION = r,
    Aad = 0,
    Abo = 0,
    Abr = 0,
    Agu = 0,
    Ahe_ec = 0,
    Aepi = 0,
    Amid = 0,
    Aendo = 0,
    Apf = 0,
    Aki = 0,
    Ali = 0,
    Alu = 0,
    Amu = 0,
    Ask = 0,
    Asp = 0,
    Ave = IVDOSE,
    Aar = 0,
    Are = 0,
    D = PODOSE * Bioavailability,
    NOR = PODOSE * (1 - Bioavailability/FaFg) * FaFg * MW_nor/MW_ami,
    Ali_nor = 0,
    Cbl_nor = 0,
    Cre_nor = 0,
    Che_nor = 0
  )
  ###Differential equations - mg/hr
  PBPKModel = function(times, state, parm) {
    with(as.list(c(state, parm)), {
      inf <- ifelse(times <= t, inf, 0)
      
    
      F_Dose <-
        ifelse(times < tlag, 0, a[1] * D ) #
      
      Nor_formed <- 
        ifelse(times < tlag, 0, a[1] * NOR)
      
      
      #A_portal_vein <-
      # ifelse(times < tlag, 0, (a[1] * (D/Vgu)) / (a[3] + (D/Vgu))) #
      #AMI concentrations:
      Cadipose <- Aad / Vad    #adipose
      Cbone <- Abo / Vbo		   #bone
      Cbrain <- Abr / Vbr		   #brain
      Cgut <- Agu / Vgu			   #gut
      Cheart_ec <- Ahe_ec / Vhe_ec #cardiac extracellular fluid
      Cendo <- Aendo / Vendo   #endocardium
      Cmid <- Amid / Vmid      #miocardium
      Cepi <- Aepi / Vepi      #epicardium
      Cpf <- Apf / Vpf         #pericardial fluid
      Ckidney <- Aki / Vki	   #kidney
      Cliver <- Ali / Vli		   #liver
      Clung <- Alu / Vlu		   #lung
      Cmuscle <- Amu / Vmu	   #muscle
      Cskin <- Ask / Vsk		   #skin
      Cspleen <- Asp / Vsp	   #spleen
      Cvenous <- Ave / Vve     #venous blood
      Carterial <- Aar / Var	 #arterial blood
      Crest <- Are / Vre 			#rest of body
      Cplasmavenous <- Cvenous / BP	#venous plasma concentration
      Cliverfree <-
        Cliver * (fup / BP) * x[2]  #liver - free concentration
      Ckidneyfree <-
        Ckidney * (fup / BP)	 #kidney - free concentration
      #NORTRIPTYLINE Concentrations
      Cliver_nor <- Ali_nor / Vli #liver
      Cliverfree_nor = Cliver_nor * (fup_nor / BP_nor) * x[3]#liver - free concentration for nortriptyline
      Cplasma_nor = Cbl_nor / BP_nor #venous plasma nortriptyline concentration
      
      
      #metabolism
      #AMITRIPTILINE (A)
      #LIVER (L)
      #N-demethylation
      #Vmax for amitriptyline after [Venkatakrishnan 2001] [pmol/min/pmol CYP]
      #Km for amitriptyline [mcM]
      # 1.CYP1A2
      CLint_1A2 <-
        (ISEF1A2 * (V_1A2_ami / (K_1A2_ami + Cliverfree)) * CYP1A2_L) / fumic  #[L/min/mg of microsomal protein]
      # 2.CYP2B6
      CLint_2B6 <-
        (ISEF2B6 * (V_2B6_ami / (K_2B6_ami + Cliverfree)) * CYP2B6_L) / fumic  #[L/min/mg of microsomal protein]
      # 3.CYP2C8
      CLint_2C8 <-
        (ISEF2C8 * (V_2C8_ami / (K_2C8_ami + Cliverfree)) * CYP2C8_L) / fumic  #[L/min/mg of microsomal protein]
      # 4.CYP2C9
      CLint_2C9 <-
        (ISEF2C9 * (V_2C9_ami / (K_2C9_ami + Cliverfree)) * CYP2C9_L) / fumic  #[L/min/mg of microsomal protein]
      # 5.CYP2C19
      CLint_2C19 <-
        (ISEF2C19 * (V_2C19_ami / (K_2C19_ami + Cliverfree)) * CYP2C19_L) / fumic  #[L/min/mg of microsomal protein]
      # 6.CYP2D6
      CLint_2D6 <-
        (ISEF2D6 * (V_2D6_ami / (K_2D6_ami + Cliverfree)) * CYP2D6_L) / fumic  #[L/min/mg of microsomal protein]
      # 7.CYP3A4
      CLint_3A4 <-
        (ISEF3A4 * (V_3A4_ami / (K_3A4_ami + Cliverfree)) * CYP3A4_L) / fumic  #[L/min/mg of microsomal protein]
      #sum of intrinsic clearances for demethylation for all CYPs isoforms
      CLint_demethylation_L <-
        (
          CLint_1A2 + CLint_2B6 + CLint_2C8 + CLint_2C9 + CLint_2C19 + CLint_2D6 + CLint_3A4
        ) *
        60 #[L/h/mg of microsomal protein]
      
      #E-10-hydroxylation:
      #1.CYP2B6
      CLint_2B6_ami_h <-
        (ISEF2B6 * (V_2B6_ami_h / (K_2B6_ami_h + Cliverfree)) * CYP2B6_L) / fumic  #[L/min/mg of microsomal protein]
      #2.CYP2D6
      CLint_2D6_ami_h <-
        (ISEF2D6 * (V_2D6_ami_h / (K_2D6_ami_h + Cliverfree)) * CYP2D6_L) / fumic  #[L/min/mg of microsomal protein]
      #3.CYP3A4
      CLint_3A4_ami_h <-
        (ISEF3A4 * (V_3A4_ami_h / (K_3A4_ami_h + Cliverfree)) * CYP3A4_L) / fumic  #[L/min/mg of microsomal protein]
      #sum of intrinsic clearances for demethylation for all CYPs isoforms
      CLint_hydroxylation_L <-
        (CLint_2B6_ami_h + CLint_2D6_ami_h + CLint_3A4_ami_h) * 60 #[L/h/mg of microsomal protein]
      
      #Hepatic intrinsic clearance:
      CLint_ami_L <-
        (CLint_demethylation_L + CLint_hydroxylation_L) * MPPGL * Vli * liver_density #[L/h]
      
      #HEART
      #no scalar assumed in heart tissue
      #Vmax and Km for liver microsomes
      # 1.CYP2C8
      CLint_2C8_H <-
        ((V_2C8_ami / K_2C8_ami) * CYP2C8_H) / fumic  #[L/min/mg tissue]
      # 2.CYP2C9
      CLint_2C9_H <-
        ((V_2C9_ami / K_2C9_ami) * CYP2C9_H) / fumic  #[L/min/mg tissue]
      # 3.CYP2J2
      CLint_2J2_H <-  0 * CYP2J2_H #[L/min/mg tissue]
      # 4.CYP3A4
      CLint_3A4_H <-  ((V_3A4_ami / K_3A4_ami) * CYP3A4_H) / fumic
      #sum of intrinsic clearances for demethylation for all CYPs isoforms
      CLint_demethylation_H <-
        (CLint_2C8_H + CLint_2C9_H + CLint_2J2_H + CLint_3A4_H) * 60 #[L/h/mg tissue]
      #Cardiac intrinsic clearance:
      CLint_ami_H <-
        CLint_demethylation_H * Vhe * heart_density * 10 ^ 3  #[L/h]
      
      
      
      
      
      
      
      #NORTRIPTYLINE (N)
      #LIVER (L)
      #Vmax for N after [Olesen and Linnet 1997] [mol/h/mol CYP]
      #Km for Nortriptyline [mcmol/L]
      #demethylation
      # 1.CYP1A2
      V_1A2_nor <- 6.8 * MW_nor * 10 ^ -9 #[mg/h/pmol CYP]
      K_1A2_nor <- 54.2 * MW_nor * 10 ^ -3 #[mg/L]
      CLint_1A2_nor <-
        (ISEF1A2 * (V_1A2_nor / (K_1A2_nor + Cliverfree_nor)) * CYP1A2_L) / fumic_nor  #[L/h/mg of microsomal protein]
      # 2.CYP2C19
      V_2C19_nor <- 93.1 * MW_nor * 10 ^ -9 #[mg/h/pmol CYP]
      K_2C19_nor <- 118 * MW_nor * 10 ^ -3 #[mg/L]
      CLint_2C19_nor <-
        (ISEF2B6 * (V_2C19_nor / (K_2C19_nor + Cliverfree_nor)) * CYP2C19_L) /
        fumic_nor  #[L/h/mg of microsomal protein]
      # 3.CYP2D6
      V_2D6_nor_d <- 19.4 * MW_nor * 10 ^ -9 #[mg/h/pmol CYP]
      K_2D6_nor_d <- 0.48 * MW_nor * 10 ^ -3 #[mg/L]
      CLint_2D6_nor_d <-
        (ISEF2C8 * (V_2D6_nor_d / (K_2D6_nor_d + Cliverfree_nor)) * CYP2D6_L) /
        fumic_nor  #[L/h/mg of microsomal protein]
      #sum of intrinsic clearances for demethylation for all CYPs isoforms
      CLint_demethylation_nor_L <-
        CLint_1A2_nor + CLint_2C19_nor + CLint_2D6_nor_d #[L/h/mg of microsomal protein]
      #(E)-10-hydroxylation
      #1. CYP2D6
      V_2D6_nor_h <- 130 * MW_nor * 10 ^ -9 #[mg/h/pmol CYP]
      K_2D6_nor_h <- 0.74 * MW_nor * 10 ^ -3 #[mg/L]
      CLint_2D6_nor_h <-
        (ISEF2C8 * (V_2D6_nor_h / (K_2D6_nor_h + Cliverfree_nor)) * CYP2D6_L) /
        fumic_nor  #[L/h/mg of microsomal protein]
      #sum of intrinsic clearances for hydroxylation for all CYPs isoforms
      CLint_hydroxylation_nor_L <-
        CLint_2D6_nor_h #[L/h/mg of microsomal protein]
      
      #Hepatic intrinsic clearance:
      CLint_nor_L <-
        (CLint_demethylation_nor_L + CLint_hydroxylation_nor_L) * MPPGL * Vli * liver_density #[L/h]
      
      #HEART (H)
      #assumed no metabolism of nortriptyline in heart tissue
      
      
      Venous <- F_Dose +
        Qpf * (Cpf / Kppf) + Qad * (Cadipose / Kpad * BP) + Qbo * (Cbone / Kpbo *
                                                                     BP) + Qbr * (Cbrain / Kpbr * BP) + Qhe * (Cheart_ec / Kpec * BP) + Qki *
        (Ckidney / Kpki * BP) + Qh * (Cliver / Kpli * BP)  + Qmu * (Cmuscle / Kpmu *
                                                                      BP) + Qsk * (Cskin / Kpsk * BP) + Qre * (Crest / Kpre * BP)
      ## rates of changes
      dINFUSION <- -inf
      dAad <- Qad * (Carterial - Cadipose / Kpad * BP) #adipose
      dAbo <- Qbo * (Carterial - Cbone / Kpbo * BP) #bone
      dAbr <- Qbr * (Carterial - Cbrain / Kpbr * BP) #brain
      dAgu <-  Qgu * (Carterial - Cgut / Kpgu * BP) #gut
      #
      #heart tissue:
      dAhe_ec <-
        Qhe * (Carterial - Cheart_ec / Kpec * BP) + PSA_pf * ((Cpf * fu_pf * funionized_pf) - (Cheart_ec * fu_ec * funionized_ec)) + PSA_epi * ((Cepi * fu_ht_ami * funionized_ic) - (Cheart_ec * fu_ec * funionized_ec)) + PSA_mid * ((Cmid * fu_ht_ami * funionized_ic) - (Cheart_ec * fu_ec * funionized_ec)) + PSA_endo * ((Cendo * fu_ht_ami * funionized_ic) - (Cheart_ec * fu_ec * funionized_ec)) + (x[4] * fu_ht_ami * Cepi) - (x[5] * fu_ec * Cheart_ec) + (x[4] * fu_ht_ami * Cmid) - (x[5] * fu_ec * Cheart_ec) + (x[4] * fu_ht_ami * Cendo) - (x[5] * fu_ec * Cheart_ec)
      dAepi <-
        PSA_epi * ((Cheart_ec * fu_ec * funionized_ec) - (Cepi * fu_ht_ami * funionized_ic)) + (x[5] * fu_ec * Cheart_ec) - (x[4] * fu_ht_ami * Cepi) - (Cepi * fu_ht_ami  * (Vepi /
                                                                                                                                                                                Vhe) * CLint_ami_H)
      dAmid <-
        PSA_mid * ((Cheart_ec * fu_ec * funionized_ec) - (Cmid * fu_ht_ami * funionized_ic)) + (x[5] * fu_ec * Cheart_ec) - (x[4] * fu_ht_ami * Cmid) - (Cmid * fu_ht_ami  * (Vmid /
                                                                                                                                                                                Vhe) * CLint_ami_H)
      dAendo <-
        PSA_endo * ((Cheart_ec * fu_ec * funionized_ec) - (Cendo * fu_ht_ami * funionized_ic)) + (x[5] * fu_ec * Cheart_ec) - (x[4] * fu_ht_ami * Cendo) - (Cendo * fu_ht_ami * (Vendo /
                                                                                                                                                                                   Vhe) * CLint_ami_H)
      dApf <-
        Qpf * (Carterial - Cpf / Kppf) + PSA_pf * ((Cheart_ec * fu_ec * funionized_ec) - (Cpf * fu_pf * funionized_pf))
      #
      dAki <-
        Qki * (Carterial - Ckidney / Kpki * BP) - CLrenal * Ckidneyfree  #kidney
      dAli <-
        Qha * Carterial + Qgu * (Cgut / Kpgu * BP) + Qsp * (Cspleen / Kpsp * BP) - Qh *
        (Cliver / Kpli * BP) - Cliverfree * CLint_ami_L #liver
      dAlu <- Qlu * Cvenous - Qlu * (Clung / Kplu * BP) #lung
      dAmu <- Qmu * (Carterial - Cmuscle / Kpmu * BP)   #muscle
      dAsk <- Qsk * (Carterial - Cskin / Kpsk * BP)  		#skin
      dAsp <- Qsp * (Carterial - Cspleen / Kpsp * BP)  	#spleen
      dAve <- inf + Venous - Qlu * Cvenous   				#venous blood
      dAar <-
        Qlu * (Clung / Kplu * BP) - Qlu * Carterial  		#arterial blood
      dAre <-
        Qre * (Carterial - Crest / Kpre * BP)  			#rest of body
      dD <- -F_Dose  			#oral dosing
      dNOR <- -Nor_formed
      #mPBPK
      dAli_nor <-
        Nor_formed + (MW_nor / MW_ami) * (Cliverfree * CLint_demethylation_L * MPPGL * Vli * liver_density) + Qh *
        Cbl_nor - Cliver_nor / Kpli_nor * Qh - Cliverfree_nor * CLint_nor_L
      dCbl_nor <- 
        (((Cliver_nor / Kpli_nor) * Qh + QC * Cre_nor / x[1] + Qhe * Che_nor /
            Kphe_nor - (QC + Qhe + Qh + CLnonhep_nor) * Cbl_nor
        )) / (Var + Vve)
      dCre_nor <- ((QC * Cbl_nor - QC * Cre_nor / x[1])) / V1
      # dCre_nor <- ((MW_nor/MW_ami)*(Cgut*fugut_ami*CLuint_gut) +  (QC * Cbl_nor - QC * Cre_nor / x[1])) / V1
      dChe_nor <-
        ((MW_nor / MW_ami) * ((Cepi * fu_ht_ami * (Vepi / Vhe) * CLint_ami_H) + (Cmid * fu_ht_ami * (Vmid /
                                                                                                       Vhe) * CLint_ami_H) + (Cendo * fu_ht_ami * (Vendo / Vhe) * CLint_ami_H)
        ) + (Qhe * Cbl_nor - Qhe * Che_nor / Kphe_nor)) / Vhe
      
      #return the rate of changes
      list(
        c(
          dINFUSION,
          dAad,
          dAbo,
          dAbr,
          dAgu,
          dAhe_ec,
          dAepi,
          dAmid,
          dAendo,
          dApf,
          dAki,
          dAli,
          dAlu,
          dAmu,
          dAsk,
          dAsp,
          dAve,
          dAar,
          dAre,
          dD,
          dNOR,
          dAli_nor,
          dCbl_nor,
          dCre_nor,
          dChe_nor
        ),
        BL = Cplasmavenous,
        BL_NOR = Cplasma_nor,
        HT = (Ahe_ec + Aepi + Amid + Aendo) / Vhe,
        MID = (Amid / Vmid_ic),
        ENDO = (Aendo) / Vendo_ic,
        EPI = (Aepi) / Vepi_ic,
        EC = Cheart_ec,
        HT_NOR = Che_nor,
        logHT = log10((Ahe_ec + Aepi + Amid + Aendo) / Vhe),
        logBL = log10(Cplasmavenous),
        logBL_NOR = log10(Cplasma_nor),
        DOSE = D,
        A_NOR = NOR
      )
    })
  }
  
  out <-
    ode(
      y = state,
      times = times,
      func = PBPKModel,
      parm = parameters
    )
  results <- data.frame(out)
  par(mfcol = c(2, 1))
  
  return(results)
}


# APPLYING THE MODEL ------------------------------------------------------

for (i in pop3$rn) {
  nam <- paste("outputCurry", i, sep = "")
  assign (
    nam,
    ModelVar(
      pop3$weight[i],
      pop3$`mean CO Tanner formula [L/h]`[i],
      pop3$`random MPPGL [mg/g]`[i],
      pop3$`Vad [L]`[i],
      pop3$`Vbl [L]`[i],
      pop3$`Vrb [L]`[i],
      pop3$`Vbo [L]`[i],
      pop3$`Vbr [L]`[i],
      pop3$`Vgu [L]`[i],
      pop3$`Vhe [L]`[i],
      pop3$`Vki [L]`[i],
      pop3$`Vli [L]`[i],
      pop3$`Vlu [L]`[i],
      pop3$`Vpl [L]`[i],
      pop3$`Vmu [L]`[i],
      pop3$`Vsk [L]`[i],
      pop3$`Vsp [L]`[i],
      pop3$`Qad [L/h]`[i],
      pop3$`Qbo [L/h]`[i],
      pop3$`Qbr [L/h]`[i],
      pop3$`Qgu [L/h]`[i],
      pop3$`Qhe [L/h]`[i],
      pop3$`Qki [L/h]`[i],
      pop3$`Qh [L/h]`[i],
      pop3$`Qlu [L/h]`[i],
      pop3$`Qmu [L/h]`[i],
      pop3$`Qsk [L/h]`[i],
      pop3$`Qsp [L/h]`[i],
      pop3$`Vpf [L]`[i],
      pop3$`CYP2C8_H [pmol/mg tissue]`[i],
      pop3$`CYP2C9_H [pmol/mg tissue]`[i],
      pop3$`CYP2J2_H [pmol/mg tissue]`[i],
      pop3$`Vre [L]`[i],
      pop3$`Qre [L/h]`[i],
      pop3$`CYP1A2_L [pmol/mg protein]`[i],
      pop3$`CYP2B6_L [pmol/mg protein]`[i],
      pop3$`CYP2C8_L [pmol/mg protein]`[i],
      pop3$`CYP2C9_L [pmol/mg protein]`[i],
      pop3$`CYP2C19_L [pmol/mg protein]`[i],
      pop3$`CYP2D6_L [pmol/mg protein]`[i],
      pop3$`CYP3A4_L [pmol/mg protein]`[i],
      #pop3$`CYP2C9_I [nmol/small intestine]`[i],
      #pop3$`CYP2C19_I [nmol/small intestine]`[i],
      #pop3$`CYP2D6_I [nmol/small intestine]`[i],
      #pop3$`CYP3A4_I [nmol/small intestine]`[i],
      pop3$tlag[i],
      pop3$Bioavailability[i],
      #pop3$T_si[i],
      pop3$FaFg[i],
      t_end,
      oral_dose,
      pop3$fup[i],
      pop3$fup_nor[i],
      pop3$BP[i],
      pop3$BP_nor[i],
      pop3$`MV [mcm^3]`[i],
      pop3$`MSA [mcm^2]`[i],
      pop3$`SA_pf [cm^2]`[i]
      
      
    )#,
    #envir = .GlobalEnv
  )
  
}

results_list = lapply(ls(pattern = "outputCurry[0-9]"), get)#, envir = .GlobalEnv), get)

newDF <- bind_rows(results_list, .id = "id")

