  # The following is a toolkit of various financial risk robust optimization methods including examples.
  
  # Problem 2 a: Max expected profit
# Demand scenarios
d = [i for i in range(0,101)]
scenarios = range(1,101)

# Parameters 
# Cost
c = 0.6
# Sale price
r = 1.5

# Stochastic linear program
M = ConcreteModel()
M.x = Var(within=NonNegativeReals)
M.p = Var(scenarios)
M.y = Var(scenarios)

# Constraints
M.c = ConstraintList()
for i in scenarios:
    # Sales <= demand
    M.c.add( M.y[i]<=d[i] )
    # Purchases >= sales
    M.c.add(M.x >= M.y[i])
    # Profit per scenario
    M.c.add( M.p[i]==r*M.y[i]-c*M.x)

# Max probabilistic sum of scenario profits
M.o = Objective(expr=sum(M.p[i] for i in scenarios)/100.0,sense=maximize)

solver = SolverFactory('glpk')
status = solver.solve(M)

print("Status = %s" % status.solver.termination_condition)

print("%s = %f" % (M.x, value(M.x)))
for i in scenarios:
    print("%s = %f" % (M.p[i], value(M.p[i])))
print("Objective = %f" % value(M.o))


# Problem 2 c: Min financial risk, profit threshold = 0
# Demand scenarios
d = [i for i in range(0,101)]
scenarios = range(1,101)
omega = 0
BigM = 1000

# Stochastic linear program
M = ConcreteModel()
M.x = Var(within=NonNegativeReals)
M.p = Var(scenarios)
M.y = Var(scenarios)
M.z = Var(scenarios,within=Binary)

# Constraints
M.c = ConstraintList()
for i in scenarios:
    # Sales <= demand
    M.c.add( M.y[i]<=d[i] )
    # Purchases >= sales
    M.c.add(M.x >= M.y[i])
    # Profit per scenario
    M.c.add( M.p[i]==1.5*M.y[i]-0.6*M.x)
    # Minimum profit constraints
    M.c.add( M.p[i]<= omega+BigM*M.z[i])
    M.c.add( M.p[i]>= omega-BigM*(1-M.z[i]))

epsilon = 0.001
M.f1 = Expression(expr=sum(M.p[i] for i in scenarios)/100.0)
M.f2 = Expression(expr= -sum(M.z[i] for i in scenarios)/100.0)
M.O = Objective(expr=(M.f1+epsilon*M.f2),sense=maximize)
solver = SolverFactory('glpk')
solver.solve(M)
print(str(value(M.x)))
print(value(M.O))

M.del_component(M.O)
M.del_component(M.f1)
M.del_component(M.f2)

# Max probabilistic sum of scenario profits
# Min risk of scenarios
M.f1 = Expression(expr=sum(M.p[i] for i in scenarios)/100.0)
M.f2 = Expression(expr= -sum(M.z[i] for i in scenarios)/100.0)

x_vals = []
f1_vals = []
f2_vals = []
o_vals = []
import random
epsilons = [random.random() for i in range(11)]

for j in epsilons:
    M.O = Objective(expr=M.f1+j*M.f2,sense=maximize)
    solver = SolverFactory('glpk')
    solver.solve(M)
    print(str(value(M.x)))
    x_vals.append(int(value(M.x)))
    f1_vals.append(value(M.f1))
    f2_vals.append(value(M.f2))
    o_vals.append(value(M.O))
    M.del_component(M.O)
   
epsilons2 = [random.uniform(1,5) for i in range(11)]

for j in epsilons2:
    M.O = Objective(expr=M.f1+j*M.f2,sense=maximize)
    solver = SolverFactory('glpk')
    solver.solve(M)
    print(str(value(M.x)))
    x_vals.append(int(value(M.x)))
    f1_vals.append(value(M.f1))
    f2_vals.append(value(M.f2))
    o_vals.append(value(M.O))
    M.del_component(M.O)   

M.del_component(M.f1)
M.del_component(M.f2)
    
M.f1 = Var()
M.f2 = Var()
M.C_f1 = Constraint(expr= M.f1==sum(M.p[i] for i in scenarios)/100.0)
M.C_f2 = Constraint(expr= M.f2== -sum(M.z[i] for i in scenarios)/100.0)
M.O_f1 = Objective(expr= M.f1  , sense=maximize)
M.O_f2 = Objective(expr= M.f2  , sense=maximize)

M.O_f2.deactivate()

solver = SolverFactory('glpk')
solver.solve(M);

print(str(value(M.x)))
print( 'f1 = ' + str(value(M.f1)) )
print( 'f2 = ' + str(value(M.f2)) )
f1_max = value(M.f1)

# ## max f2

M.O_f2.activate()
M.O_f1.deactivate()

solver = SolverFactory('glpk')
solver.solve(M);

print(str(value(M.x)))
print( 'f1 = ' + str(value(M.f1)) )
print( 'f2 = ' + str(value(M.f2)) )
f1_min = value(M.f1)

# Problem 2d: Min financial risk, absolute profit threshold = 0
# Demand scenarios
d = [i for i in range(0,101)]
scenarios = range(1,101)
omega = 0

# Stochastic linear program
M = ConcreteModel()
M.x = Var(within=NonNegativeReals)
M.p = Var(scenarios)
M.y = Var(scenarios)

# Constraints
M.c = ConstraintList()
for i in scenarios:
    # Sales <= demand
    M.c.add( M.y[i]<=d[i] )
    # Purchases >= sales
    M.c.add(M.x >= M.y[i])
    # Profit per scenario
    M.c.add( M.p[i]==1.5*M.y[i]-0.6*M.x)
    # Minimum profit constraint
    M.c.add( M.p[i]>= omega)
    
# Max probabilistic sum of scenario profits
M.o = Objective(expr=sum(M.p[i] for i in scenarios)/100.0,sense=maximize)

solver = SolverFactory('glpk')
status = solver.solve(M)

print("Status = %s" % status.solver.termination_condition)

print("%s = %f" % (M.x, value(M.x)))
for i in scenarios:
    print("%s = %f" % (M.p[i], value(M.p[i])))
print("Objective = %f" % value(M.o))
