# Stochastic reservoir optimization

import numpy as np
from pyomo.environ import *
from pyomo.opt import SolverFactory
from pyomo.core import *
from pyomo.pysp import *
from pyomo.util import *
import os
import matplotlib.pyplot as plt
MINLP_SOLVER_PATH_C =  os.path.join("C:\\", "couenne", "couenne.exe")

# Model
model = AbstractModel()

## Parameters ##
# Time periods
model.NumTimes = Param(within=PositiveIntegers)

def stages_rule(model):
    return set(range(1, model.NumTimes()+1))

model.Times = Set(initialize=stages_rule)

# Number of users per scenario
model.NumUsers = Param(within=PositiveIntegers)

# Categories of users
model.Users = Set()

# Min outflow
model.MinOut = Param(within=NonNegativeReals)

# Max outflow
model.MaxOut = Param(within=NonNegativeReals)

# Min capacity
model.MinCapacity = Param(within=NonNegativeReals)

# Max capacity
model.MaxCapacity = Param(within=NonNegativeReals)

# Per capita demand
model.DemandPerCapita = Param(within=NonNegativeReals)

# Demand tolerance +- <<< 1.0
model.DemandTolerance = Param(within=NonNegativeReals) 

# Benefit coefficient
model.BenefitPerCapita = Param(within=NonNegativeReals)

# Inflow
model.Inflow = Param(model.Times,within=NonNegativeReals)

# Initial reservoir storage
model.InitStorage = Param(within=NonNegativeReals)

# Safe/sustainable storage level
model.SustainStorage = Param(within=NonNegativeReals)

# Storage penalty if < sustainable level
model.StoragePenalty = Param(within=NonNegativeReals)

## Bounds and Variables ##
# Reservoir storage
def storage_bounds(model,t):
    return(model.MinCapacity,model.MaxCapacity)
model.Storage = Var(model.Times,bounds=storage_bounds,within=NonNegativeReals)

# Outflow (downstream) / environmental flows
def outflow_bounds(model,t):
    return(model.MinOut,model.MaxOut)
model.Outflow = Var(model.Times,bounds=outflow_bounds,within=NonNegativeReals)  

# Supply release
model.SupplyRelease = Var(model.Times, within=NonNegativeReals)

# Benefit
model.StageBenefit = Var(model.Times, within=Reals)

# Mass balance constraint: 
# storage[t] = storage[t-1] + inflow[t-1] - outflow[t-1] - demand[t-1]
def mass_balance_rule(model, t):
    if t == 1:
        return model.Storage[t] == model.InitStorage
    elif t > 1:
        return model.Storage[t] == model.Storage[t-1] + model.Inflow[t-1] - model.Outflow[t-1] - model.SupplyRelease[t-1]

model.MassBalance = Constraint(model.Times, rule=mass_balance_rule)

# Supply release constraint = (per capita demand)*(num users)
def min_supply_rule(model,t):
    return model.SupplyRelease[t]>=model.DemandPerCapita*model.NumUsers-model.DemandTolerance*(model.DemandPerCapita*model.NumUsers)

def max_demand_rule(model,t):
    return model.Outflow[t] >= model.MinOut+(model.SupplyRelease[t]-(model.DemandPerCapita*model.NumUsers))

model.MinSupply = Constraint(model.Times,rule=min_supply_rule)    

# Calculate benefit: meet demand (min deficit) per user, maintain sustainable storage
# if above safe storage: (benefit)*(supply release)-(deficient storage penalty coeff)*(1/storage)
def benefit_rule(model,t):
    if t < model.NumTimes:
        return model.StageBenefit[t]==((model.SupplyRelease[t]/model.NumUsers)*model.BenefitPerCapita)-(model.StoragePenalty*(1/model.Storage[t]))
    else: 
        return model.StageBenefit[t]==model.StageBenefit[t-1]
    
model.ComputeBenefit = Constraint(model.Times,rule=benefit_rule)
    
# Objective: Maximize user/system benefit
def total_benefit_rule(model):
    return dot_product(model.StageBenefit)

model.Objective_rule = Objective(rule=total_benefit_rule, sense=maximize)
