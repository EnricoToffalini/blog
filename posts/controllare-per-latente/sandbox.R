
library(lavaan)

# set seed per riproducibility
set.seed(0)

# genera dati e metti in un dataframe
N = 10000
g = rnorm(N,0,1)
Verbal = g*0.7 + rnorm(N,0,0.51)
Visual = g*0.6 + rnorm(N,0,0.64)
Memory = g*0.7 + rnorm(N,0,0.51)
Fluid = g*0.8 + rnorm(N,0,0.36)

AdaptiveBehavior = g*0.5 + rnorm(N,0,1)

df = data.frame(verb1 = Verbal + rnorm(N,0,0.7),
                verb2 = Verbal + rnorm(N,0,0.7),
                verb3 = Verbal + rnorm(N,0,0.7),
                vis1 = Visual + rnorm(N,0,0.7),
                vis2 = Visual + rnorm(N,0,0.7),
                vis3 = Visual + rnorm(N,0,0.7),
                mem1 = Memory + rnorm(N,0,0.7),
                mem2 = Memory + rnorm(N,0,0.7),
                fluid1 = Fluid + rnorm(N,0,0.7),
                fluid2 = Fluid + rnorm(N,0,0.7),
                fluid3 = Fluid + rnorm(N,0,0.7),
                adapt1 = AdaptiveBehavior + rnorm(N,0,0.7),
                adapt2 = AdaptiveBehavior + rnorm(N,0,0.7),
                adapt3 = AdaptiveBehavior + rnorm(N,0,0.7)
)

#### Fittiamo il Modello Sbagliato
wrong_model = "
Verbal_factor =~ verb1 + verb2 + verb3
Visual_factor =~ vis1 + vis2 + vis3
Memory_factor =~ mem1 + mem2
Fluid_factor =~ fluid1 + fluid2 + fluid3

Adaptive_factor =~ adapt1 + adapt2 + adapt3

Adaptive_factor ~ Verbal_factor + Visual_factor + Memory_factor + Fluid_factor
"
fitWrong = sem(model=wrong_model, data=df, std.lv=T)
summary(fitWrong)


#### Fittiamo il Modello Giusto
ok_model = "
Verbal_factor =~ verb1 + verb2 + verb3
Visual_factor =~ vis1 + vis2 + vis3
Memory_factor =~ mem1 + mem2
Fluid_factor =~ fluid1 + fluid2 + fluid3

g =~ Verbal_factor + Visual_factor + Memory_factor + Fluid_factor

Adaptive_factor =~ adapt1 + adapt2 + adapt3

Adaptive_factor ~ g + Verbal_factor + Visual_factor + Fluid_factor
"
fitOk = sem(model=ok_model, data=df, std.lv=T)
summary(fitOk)

