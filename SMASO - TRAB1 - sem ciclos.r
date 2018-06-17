

setup <- function() {

# Set-up with 4 agents and 4 goods

# Initial quatities
  quant <- data.frame(Agriculture=c(420,210,210,210),Clothing=c(1.154,2.308,1.154,1.154),
  Transportation=c(0.03846,0.03846,0.07692,0.03846),Fuel=c(2.308,2.308,2.308,4.616))

# Vector of initial prices for all goods
  prices <- c(3,50,10000,50)

# Preferences for goods 
  beta <- data.frame(Agriculture=rep(0.5,4),Clothing=rep(0.05,4), Transportation=rep(0.3,4),
  Fuel=rep(0.15,4))

# Initial history (empty data frame to be filled in)
  hist <- data.frame(Agriculture=c(0),Clothing=c(0),Transportation=c(0),Fuel=c(0))

# Fixed consumption
  cons.fixed <- data.frame(Agriculture=rep(10.5,4),Clothing=rep(0.0577,4),
  Transportation=rep(0.001923,4),Fuel=rep(0.1154,4))

# Variable consumption
  cons.var <- data.frame(Agriculture=rep(10.5,4),Clothing=rep(0.0577,4),
  Transportation=rep(0.001923,4),Fuel=rep(0.1154,4))

# Production
  prod <- data.frame(Agriculture=c(84,0,0,0),Clothing=c(0,0.4616,0,0),
  Transportation=c(0,0, 0.015384, 0),Fuel=c(0,0,0,0.9232))
  
  #browser()

  dados1 <<- list()
  dados1[[1]] <<- quant
  dados1[[2]] <<- prices
  dados1[[3]] <<- beta
  dados1[[4]] <<- hist
  dados1[[5]] <<- cons.fixed
  dados1[[6]] <<- cons.var 
  dados1[[7]] <<- prod

# Set-up with 12 agents and 5 goods

# Initial quatities
  quant <- data.frame(
Agriculture=c(rep(420,3),rep(168,9)), Clothing=c(rep(0.9232,3),rep(2.308,3),rep(0.9232,6)),
Transportation=c(rep(0.030768,6),rep(0.7692,3),rep(0.030768,3)), Health=c(rep(11.04,9),rep(27.6,3)),
Money=rep(200,12)
)

# Vector of initial prices for all goods
  prices <- c(3,50,10000,10,1)

# Preferences for goods 
  beta <- data.frame(Agriculture=rep(0.32,12),Clothing=rep(0.03,12), Transportation=rep(0.2,12),
  Health=rep(0.2,12), Money=rep(0.13,12)
)

# Initial history (empty data frame to be filled in)
  hist <- data.frame(Agriculture=c(0),Clothing=c(0),Transportation=c(0),Health=c(0),Money=c(0))

# Preferences for goods 
  cons.fixed <- data.frame(Agriculture=rep(10.5,12),Clothing=rep(0.0577,12), Transportation=rep(0.001923), Health=rep(0.69,12), Money=rep(0,12))

  cons.var <- data.frame(Agriculture=rep(10.5,12),Clothing=rep(0.0577,12), Transportation=rep(0.001923), Health=rep(0.69,12), Money=rep(0,12))

# Production
  prod <- data.frame(
Agriculture=c(rep(84,3),rep(0,9)), Clothing=c(rep(0,3),rep(0.4616,3),rep(0,6)),
Transportation=c(rep(0,6),rep(0.015384,3),rep(0,3)), Health=c(rep(0,9),rep(5.52,3)),
Money=rep(0,12)
)

#browser()

  dados2 <<- list()
  dados2[[1]] <<- quant
  dados2[[2]] <<- prices
  dados2[[3]] <<- beta
  dados2[[4]] <<- hist
  dados2[[5]] <<- cons.fixed
  dados2[[6]] <<- cons.var
  dados2[[7]] <<- prod
}


##########################################
#### FUNÇÃO DE MANIPULAÇÃO DE AGENTES ####
##########################################

Agent.micro.econ <- function(dados,weeks, dinamic_prod = 1, dinamic_prod_var = 0.07, agent = 1) {

#browser()
variation <- data.frame(PriceVAR = 0, WealthVAR = 0)
hist.prod.var <- data.frame(ProdValue = 0)


# *************************************************************
# Read-in data

quant <- dados[[1]]
prices <- dados[[2]]
beta <- dados[[3]]
hist <- dados[[4]] 
cons.fixed <- dados[[5]] 
cons.var <- dados[[6]] 
prod <- dados[[7]]

# Number of agents and goods
  nagents <- nrow(quant)
  ngoods <- ncol(quant)
  offset <- 0
  cat("number of agents",nagents,"\n")
  cat("number of goods",ngoods,"\n")

# Maximum number of iterations
  it <- 75
  
  hist.prices <- hist
  hist.prices[1,1:ngoods] <- prices
  
  hist.iter.prices <- hist
  hist.iter.ex.demand <- hist
  hist.iter.prices[1,1:ngoods] <- prices

# Initial wealth
  wealth <- wealth(nagents,ngoods,offset,quant,prices)

  hist.wealth <- hist
  hist.wealth[1,1:ngoods] <- wealth
  
  # Initial production of Agent 1 - Agriculture
  hist.prod.var <- prod[1,1]

### Iterate for weeks
for (week in 1:weeks) {     
  cat("Week no.: ",week,"\n")

# Call market
  new.values <- market(nagents,ngoods,offset,quant,prices,beta,hist.iter.ex.demand,hist.iter.prices,it,week)
#  print(new.values)

  prices <- new.values[[1]]
  hist.prices <- rbind(hist.prices,prices)

  quant <- new.values[[2]]

# Initial wealth
#  cat("Initial wealth","\n")
#  print(round(wealth,1))

# Wealth after exchange on the market
  wealth <- wealth(nagents,ngoods,offset,quant,prices)
#  cat("Wealth after exchange on the market","\n")
#  print(round(wealth,1))
  hist.wealth <- rbind(hist.wealth, wealth)

 # browser()
# Se produção do agente é dinâmica varia a mesma
# aumentando ou diminuindo conforme a diferença de preços entre duas semanas

if (dinamic_prod & week > 0)
{
# calcula diferença de preços de um bem em duas (ou mais) semanas consecutivas
prices_dif_perc <- (hist.prices[week+1,agent] - hist.prices[week,agent])*100/hist.prices[week,agent]
# calcula alteração da riqueza total do agente
wealth_dif_perc <- (hist.wealth[week+1,agent] - hist.wealth[week,agent])*100/hist.wealth[week,agent]
variation <- rbind(variation, c(prices_dif_perc,wealth_dif_perc))

# se o preço diminui e a riqueza diminuiu então diminui produção
if (nrow(variation) > 1 & variation[nrow(variation),"PriceVAR"] < variation[nrow(variation)-1,"PriceVAR"] & variation[nrow(variation),"WealthVAR"] < variation[nrow(variation)-1, "WealthVAR"])
{
prod[agent,agent] <- prod[agent,agent]-prod[agent,agent]*dinamic_prod_var
hist.prod.var <- rbind(hist.prod.var, prod[agent,agent])
}
# se o preço aumenta e a riqueza diminui então aumenta produção
else if (nrow(variation) > 1 & variation[nrow(variation),"PriceVAR"] > variation[nrow(variation)-1,"PriceVAR"] & variation[nrow(variation),"WealthVAR"] < variation[nrow(variation)-1, "WealthVAR"])
{
prod[agent,agent] <- prod[agent,agent]+prod[agent,agent]*dinamic_prod_var
hist.prod.var <- rbind(hist.prod.var, prod[agent,agent])
} else {
hist.prod.var <- rbind(hist.prod.var, prod[agent,agent])
}

}
  
    
  
  quant <- quant - (cons.fixed + cons.var) + prod 

#  cat ("Quantities after production / consuption ", "\n")
#  print(quant)

# Wealth after production / consumption
# wealth <- wealth(nagents,ngoods,offset,quant,prices)
#  cat("Wealth after production / consumption","\n")
#  print(round(wealth,1))

 
  } # end iterate weeks
  
  #browser()
  
  #imprime variação da produção dinamica agricula
  cat("Evolution of Agriculture Dinamic Production","\n")
  hpv <- hist.prod.var
  print(hpv)
  #windows()
  #plot(hist.prod.var, ylab="Evolution of Agriculture Dinamic Production", xlab=nrow(hist.prod.var), main ="Agent 1 - Agriculture")
  
  cat("Evolution of prices","\n")
  e <- hist.prices
  print(e)
  
  for(l in 1:ncol(e)){
  ##windows()
  ##plot(e[,l], ylab="Evolution of prices", xlab=nrow(e), main =paste(colnames(e)[l]))
  }

  cat("Evolution of wealth","\n")
  f <- hist.wealth
  print(f)
 
  
  
  for(l in 1:ncol(f)){
  ##windows()
  ##plot(f[,l], ylab="Evolution of wealth", xlab=nrow(f), main =paste(colnames(f)[l]))
  }
 return (hist.wealth)
  
  
 
  } # end function Agent.micro.econ


# *****************************************************************
# Function market establishes new price and quantities (= desired)

  market <- function(nagents,ngoods,offset,quant,prices,beta,hist.iter.ex.demand,hist.iter.prices,it,week) { 


# inicialização das variáveis    			

#cria tabela das quantidades desejadas preenchida com 0’s
  desired<- quant - quant

# escreve preços que agentes usam para optimizar utilidade
# cat("Initial prices", round(prices,2),"\n") 

# Initial values of excess demand (use values different from 0)
  tot.ex.demand <- rep(1,ngoods)

# processo iterativo 
  for (iteration in 1:it) {
# cat("*** Iteration:", iteration, "\n") 

# If total excess demand has reached 0 in previous round, terminate iterations
  if ( sum(abs(tot.ex.demand) ) < 0.2) {
#  cat("Reached excess demand 0", "\n") & 
   break}

#### AGENTES
  budget.restr <- rep(0,nagents)

  for (agent in 1:nagents) { 	# for each agent
  for (good in offset+1:(offset+ngoods)) { 	# for each good

# calcula a budget.restr que cada agente tem de respeitar
  budget.restr[agent]<- sum(quant[agent,]*prices)    

# utiliza formulas de lagrange para obter solução maximizadora da utililidade
  desired[agent,good]<- beta[agent,good]*budget.restr[agent]/(sum(beta[agent,])*prices[good])    
	} # agent
	} # good
# obtém tabela nagents x ngoods com quant ideal de cada bem para cada agente
  ex.demand<- (desired-quant) 

#### LEILOEIRO

# para cada bem
  for (good in 1:ngoods) { 

# Calcula excesso de procura 
  tot.ex.demand[good]<-sum(ex.demand[,good])  

# e re ajusta os preços 
  prices[good]<-prices[good]*(1+tot.ex.demand[good]/(2* sum(quant[,good]) )) 

  } # para cada bem / good

# difunde os valores do excesso de procura 
# cat("Excess Demand", round(tot.ex.demand,1), "\n") 
hist.iter.ex.demand <- rbind(hist.iter.ex.demand,tot.ex.demand)

# difunde os novos preços
# cat("New Prices", round(prices,2), "\n") 
hist.iter.prices <- rbind(hist.iter.prices,prices)

} 		# fim processo iterativo

if (week == 1){
# Output evolution of excess demand
  cat("Evolution of excess demand","\n")
  a <- round(hist.iter.ex.demand,2)
  print(a)
  
  for(l in 1:ncol(a)){
  #windows()
  ##plot(a[,l], ylab="Evolution of excess demand", xlab=nrow(a), main =paste(colnames(a)[l]))
  }

# Output evolution of prices
  cat("Evolution of prices","\n")
  b <- round(hist.iter.prices,3)
  print(b) 
   
  for(l in 1:ncol(b)){
  #windows()
  #plot(b[,l], ylab="Evolution of prices", xlab=nrow(b), main =paste(colnames(b)[l]))
  }

# Output initial and final quantities 
  cat("Initial quantities","\n") 
  
  print(quant)
  
  for(l in 1:ncol(quant)){
  #windows()
  ##plot(quant[,l], ylab="Initial Quantities", xlab=nrow(quant), main =paste(colnames(quant)[l]))
  }

  cat("Final quantities after exchange on the market","\n")
  d <- round(desired,3) 
  print(d)
  
  for(l in 1:ncol(d)){
  #windows()
  ##plot(d[,l], ylab="Final quantities after exchange on the market", xlab=nrow(d), main =paste(colnames(d)[l]))
  }
}

  new.values <- list()
  new.values[[1]] <- prices
  new.values[[2]] <- desired
  return(new.values)

} # end function market

wealth <- function(nagents,ngoods,offset,quant,prices) {
  wealth <- rep(0,nagents)
  for (agent in 1:nagents) { 	# for each agent
  for (good in offset+1:(offset+ngoods)) { 	# for each good 
  wealth[agent]<- sum(quant[agent,]*prices)  
  }}
  return(wealth)
  }

  #variavel global
  #final.wealth <- data.frame()
  
  
  
  setup()
  hist.wealth <- Agent.micro.econ(dados2,10, dinamic_prod = 0, dinamic_prod_var = 0.23, agent = 1)
  #Agent.micro.econ(dados2,6)
  final.wealth <- data.frame(FinalWealth = hist.wealth[nrow(hist.wealth), 1])
  
  
  #Testes Riqueza final com alteração de percentagem de variação da produção
  #começa com alteração de 1% na produção 
  dinamic_prod <- 1
  
	
  valor <- 0.45
  teste <- 1
  
  #if (dinamic_prod){
  
  #for(teste in 1:100)
  #{
  #chama função com novos parametros para o agente 1 (Agricultura)
  cat("Variação da Produção:")
  print(valor*100) 
  cat("%", "\n")
  
  hist.wealth <- Agent.micro.econ(dados2, 10, dinamic_prod = 1, dinamic_prod_var = valor, agent = 1)
  
  #data.frame com valores finais de riqueza para o agente 1 (Agricultura)
  final.wealth <- rbind(final.wealth, hist.wealth[nrow(hist.wealth), 1])
  #browser()
  
  #valor <- valor + 0.01
  #teste <- teste + 1
  #}
  
  #}
  
  
  
  