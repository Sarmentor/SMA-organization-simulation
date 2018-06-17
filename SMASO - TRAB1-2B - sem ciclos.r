

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

# Production - Agente agricultor também produz roupas
  prod <- data.frame(Agriculture=c(84,0,0,0.9232),Clothing=c(0,0.4616,0,0),
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
Agriculture=c(rep(420,3),rep(168,9)), Clothing=c(2.308,rep(0.9232,2),rep(2.308,3),rep(0.9232,6)),
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
Agriculture=c(rep(84,3),rep(0,9)), Clothing=c(0.4616,rep(0,2),rep(0.4616,3),rep(0,6)),
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

Agent.micro.econ <- function(dados,weeks, dinamic_prod = 1, dinamic_prod_var_FARMER = 0.07, dinamic_prod_var_CLOTHING = 0.07,   agent = c(1,2), strat = "ALLC") {

#browser()
variation <- data.frame(PriceVAR_FARMER = 0, PriceVAR_CLOTHING = 0, WealthVAR = 0)
#3 estratégias diferentes possíveis
strat_vector <- c("TFT","ALLC","ALLD")
hist.prod.var <- data.frame(ProdValueFarm = 0, ProdValueClothes = 0)

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
   
  rname <- NULL
  for (num_agent in 1:nagents){
	rname <- rbind(rname, paste("Agente ", num_agent))
  }
  rownames(quant) = rname
  colnames(beta) = colnames(quant)
  colnames(hist) = colnames(quant)
  rownames(cons.fixed) = rname
  rownames(cons.var) = rname
  rownames(prod) = rname
  
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
  
  hist.wealthByAgent <- NULL
  hist.wealthByAgent <- wealth
  arrWeeks <- data.frame(Week = "Semana no.: 0",stringsAsFactors = FALSE)

# Initial production of Agent 1 - Agriculture
  hist.prod.var <- c(prod[1,1],prod[1,2])

  ### Iterate for weeks
for (week in 1:weeks) {     
  cat("Week no.: ",week,"\n")
  arrWeeks <- rbind(arrWeeks, paste("Semana no.:", week))
# Call market
  new.values <- market(nagents,ngoods,offset,quant,prices,beta,hist.iter.ex.demand,hist.iter.prices,it,week)
#  #print(new.values)

  prices <- new.values[[1]]
  hist.prices <- rbind(hist.prices,prices)

  quant <- new.values[[2]]

# Initial wealth
#  cat("Initial wealth","\n")
#  #print(round(wealth,1))

# Wealth after exchange on the market
  wealth <- wealth(nagents,ngoods,offset,quant,prices)
#  cat("Wealth after exchange on the market","\n")
#  #print(round(wealth,1))
  hist.wealth <- rbind(hist.wealth, wealth)
  hist.wealthByAgent <- rbind(hist.wealthByAgent, wealth)

 # browser()
# Se produção do agente é dinâmica varia a mesma
# aumentando ou diminuindo conforme a diferença de preços entre duas semanas
# função varia a produção dos dois bens produzidos agricultura e roupas(por exemplo roupas de canhamo)

if (dinamic_prod & week > 0 & strat == "ALLC")
{
#browser()
# calcula diferença de preços de um bem em duas (ou mais) semanas consecutivas para a Agricultura
prices_dif_perc_farmer <- (hist.prices[week+1,agent[1]] - hist.prices[week,agent[1]])*100/hist.prices[week,agent[1]]
# calcula diferença de preços de um bem em duas (ou mais) semanas consecutivas para a CLOTHING
prices_dif_perc_CLOTHING <- (hist.prices[week+1,agent[2]] - hist.prices[week,agent[2]])*100/hist.prices[week,agent[2]]
# calcula alteração da riqueza total do agente
wealth_dif_perc <- (hist.wealthByAgent[week+1,agent[1]] - hist.wealthByAgent[week,agent[1]])*100/hist.wealthByAgent[week,agent[1]]

variation <- rbind(variation, c(prices_dif_perc_farmer, prices_dif_perc_CLOTHING, wealth_dif_perc))
#browser()
# se o preço do produto agricula diminui e a riqueza diminuiu então diminui produção da Agricultura e de Roupas 
if (nrow(variation) > 1 & variation[nrow(variation),"PriceVAR_FARMER"] < variation[nrow(variation)-1,"PriceVAR_FARMER"] & variation[nrow(variation),"WealthVAR"] < variation[nrow(variation)-1, "WealthVAR"])
{
prod[agent[1],agent[1]] <- prod[agent[1],agent[1]]-prod[agent[1],agent[1]]*dinamic_prod_var_FARMER
prod[agent[1],agent[2]] <- prod[agent[1],agent[2]]-prod[agent[1],agent[2]]*dinamic_prod_var_CLOTHING
hist.prod.var <- rbind(hist.prod.var, c(prod[agent[1],agent[1]],prod[agent[1],agent[2]]))
}
# se o preço do produto agricula aumenta e a riqueza diminui então aumenta produção da Agricultura e de Roupas
else if (nrow(variation) > 1 & variation[nrow(variation),"PriceVAR_FARMER"] > variation[nrow(variation)-1,"PriceVAR_FARMER"] & variation[nrow(variation),"WealthVAR"] < variation[nrow(variation)-1, "WealthVAR"])
{
prod[agent[1],agent[1]] <- prod[agent[1],agent[1]]+prod[agent[1],agent[1]]*dinamic_prod_var_FARMER
prod[agent[1],agent[2]] <- prod[agent[1],agent[2]]+prod[agent[1],agent[2]]*dinamic_prod_var_CLOTHING
hist.prod.var <- rbind(hist.prod.var, c(prod[agent[1],agent[1]],prod[agent[1],agent[2]]))
}  else {
hist.prod.var <- rbind(hist.prod.var, c(prod[agent[1],agent[1]],prod[agent[1],agent[2]]))
}
# se o preço do produto CLOTHING diminui e a riqueza diminuiu então diminui produção de Roupas
# if (nrow(variation) > 1 & variation[nrow(variation),"PriceVAR_CLOTHING"] < variation[nrow(variation)-1,"PriceVAR_CLOTHING"] & variation[nrow(variation),"WealthVAR"] < variation[nrow(variation)-1, "WealthVAR"])
# {
# }
# se o preço do produto CLOTHING aumenta e a riqueza diminui então aumenta produção de Roupas
# if (nrow(variation) > 1 & variation[nrow(variation),"PriceVAR_CLOTHING"] > variation[nrow(variation)-1,"PriceVAR_CLOTHING"] & variation[nrow(variation),"WealthVAR"] < variation[nrow(variation)-1, "WealthVAR"])
# {

# }

}

if (dinamic_prod & week > 0 & strat == "TFT")
{
# calcula diferença de preços de um bem em duas (ou mais) semanas consecutivas para a Agricultura
prices_dif_perc_farmer <- (hist.prices[week+1,agent[1]] - hist.prices[week,agent[1]])*100/hist.prices[week,agent[1]]
# calcula diferença de preços de um bem em duas (ou mais) semanas consecutivas para a CLOTHING
prices_dif_perc_CLOTHING <- (hist.prices[week+1,agent[2]] - hist.prices[week,agent[2]])*100/hist.prices[week,agent[2]]
# calcula alteração da riqueza total do agente
wealth_dif_perc <- (hist.wealthByAgent[week+1,agent[1]] - hist.wealthByAgent[week,agent[1]])*100/hist.wealthByAgent[week,agent[1]]

variation <- rbind(variation, c(prices_dif_perc_farmer, prices_dif_perc_CLOTHING, wealth_dif_perc))
# se o preço do produto agricula diminui e a riqueza diminuiu então diminui produção da Agricultura
if (nrow(variation) > 1 & variation[nrow(variation),"PriceVAR_FARMER"] < variation[nrow(variation)-1,"PriceVAR_FARMER"] & variation[nrow(variation),"WealthVAR"] < variation[nrow(variation)-1, "WealthVAR"])
{
prod[agent[1],agent[1]] <- prod[agent[1],agent[1]]-prod[agent[1],agent[1]]*dinamic_prod_var_FARMER
hist.prod.var <- rbind(hist.prod.var, c(prod[agent[1],agent[1]],prod[agent[1],agent[2]]))
}
# se o preço do produto agricula aumenta e a riqueza diminui então aumenta produção da Agricultura
if (nrow(variation) > 1 & variation[nrow(variation),"PriceVAR_FARMER"] > variation[nrow(variation)-1,"PriceVAR_FARMER"] & variation[nrow(variation),"WealthVAR"] < variation[nrow(variation)-1, "WealthVAR"])
{
prod[agent[1],agent[1]] <- prod[agent[1],agent[1]]+prod[agent[1],agent[1]]*dinamic_prod_var_FARMER
}
# se o preço do produto CLOTHING diminui e a riqueza diminuiu então diminui produção de Roupas
if (nrow(variation) > 1 & variation[nrow(variation),"PriceVAR_CLOTHING"] < variation[nrow(variation)-1,"PriceVAR_CLOTHING"] & variation[nrow(variation),"WealthVAR"] < variation[nrow(variation)-1, "WealthVAR"])
{
prod[agent[1],agent[2]] <- prod[agent[1],agent[2]]-prod[agent[1],agent[2]]*dinamic_prod_var_CLOTHING
hist.prod.var <- rbind(hist.prod.var, c(prod[agent[1],agent[1]],prod[agent[1],agent[2]]))

}
# se o preço do produto CLOTHING aumenta e a riqueza diminui então aumenta produção de Roupas
if (nrow(variation) > 1 & variation[nrow(variation),"PriceVAR_CLOTHING"] > variation[nrow(variation)-1,"PriceVAR_CLOTHING"] & variation[nrow(variation),"WealthVAR"] < variation[nrow(variation)-1, "WealthVAR"])
{
prod[agent[1],agent[2]] <- prod[agent[1],agent[2]]+prod[agent[1],agent[2]]*dinamic_prod_var_CLOTHING
hist.prod.var <- rbind(hist.prod.var, c(prod[agent[1],agent[1]],prod[agent[1],agent[2]]))

}

}

if (dinamic_prod & week > 0 & strat == "ALLD")
{
# calcula diferença de preços de um bem em duas (ou mais) semanas consecutivas para a Agricultura
prices_dif_perc_farmer <- (hist.prices[week+1,agent[1]] - hist.prices[week,agent[1]])*100/hist.prices[week,agent[1]]
# calcula diferença de preços de um bem em duas (ou mais) semanas consecutivas para a CLOTHING
prices_dif_perc_CLOTHING <- (hist.prices[week+1,agent[2]] - hist.prices[week,agent[2]])*100/hist.prices[week,agent[2]]
# calcula alteração da riqueza total do agente
wealth_dif_perc <- (hist.wealthByAgent[week+1,agent[1]] - hist.wealthByAgent[week,agent[1]])*100/hist.wealthByAgent[week,agent[1]]

variation <- rbind(variation, c(prices_dif_perc_farmer, prices_dif_perc_CLOTHING, wealth_dif_perc))
# se o preço do produto agricula diminui e a riqueza diminuiu então diminui produção da Agricultura e aumenta produção de Roupas
if (nrow(variation) > 1 & variation[nrow(variation),"PriceVAR_FARMER"] < variation[nrow(variation)-1,"PriceVAR_FARMER"] & variation[nrow(variation),"WealthVAR"] < variation[nrow(variation)-1, "WealthVAR"])
{
prod[agent[1],agent[1]] <- prod[agent[1],agent[1]]-prod[agent[1],agent[1]]*dinamic_prod_var_FARMER
prod[agent[1],agent[2]] <- prod[agent[1],agent[2]]+prod[agent[1],agent[2]]*dinamic_prod_var_CLOTHING
hist.prod.var <- rbind(hist.prod.var, c(prod[agent[1],agent[1]],prod[agent[1],agent[2]]))
}
# se o preço do produto agricula aumenta e a riqueza diminui então aumenta produção da Agricultura e diminui produção de Roupas
else if (nrow(variation) > 1 & variation[nrow(variation),"PriceVAR_FARMER"] > variation[nrow(variation)-1,"PriceVAR_FARMER"] & variation[nrow(variation),"WealthVAR"] < variation[nrow(variation)-1, "WealthVAR"])
{
prod[agent[1],agent[1]] <- prod[agent[1],agent[1]]+prod[agent[1],agent[1]]*dinamic_prod_var_FARMER
prod[agent[1],agent[2]] <- prod[agent[1],agent[2]]-prod[agent[1],agent[2]]*dinamic_prod_var_CLOTHING
hist.prod.var <- rbind(hist.prod.var, c(prod[agent[1],agent[1]],prod[agent[1],agent[2]]))
}  else {
hist.prod.var <- rbind(hist.prod.var, c(prod[agent[1],agent[1]],prod[agent[1],agent[2]]))
}
# se o preço do produto CLOTHING diminui e a riqueza diminuiu então diminui produção de Roupas
# if (nrow(variation) > 1 & variation[nrow(variation),"PriceVAR_CLOTHING"] < variation[nrow(variation)-1,"PriceVAR_CLOTHING"] & variation[nrow(variation),"WealthVAR"] < variation[nrow(variation)-1, "WealthVAR"])
# {

# }
# se o preço do produto CLOTHING aumenta e a riqueza diminui então aumenta produção de Roupas
# if (nrow(variation) > 1 & variation[nrow(variation),"PriceVAR_CLOTHING"] > variation[nrow(variation)-1,"PriceVAR_CLOTHING"] & variation[nrow(variation),"WealthVAR"] < variation[nrow(variation)-1, "WealthVAR"])
# {
# }

}

if(dinamic_prod == 0)
{
hist.prod.var <- rbind(hist.prod.var, c(prod[agent[1],agent[1]],prod[agent[1],2]))
}
  
    
  
  quant <- quant - (cons.fixed + cons.var) + prod 

#  cat ("Quantities after production / consuption ", "\n")
#  #print(quant)

# Wealth after production / consumption
# wealth <- wealth(nagents,ngoods,offset,quant,prices)
#  cat("Wealth after production / consumption","\n")
#  #print(round(wealth,1))

 
  } # end iterate weeks
  
  #browser()
  
  cat("Evolution of prices","\n")
  e <- hist.prices
  #print(e)
  
  for(l in 1:ncol(e)){
  #windows()
  #plot(e[,l], ylab="Evolution of prices", xlab=nrow(e), main =paste(colnames(e)[l]))
  }

  cat("Evolution of wealth","\n")
  f <- hist.wealth
  #print(f)
  
  for(l in 1:ncol(f)){
  #windows()
  #plot(f[,l], ylab="Evolution of wealth", xlab=nrow(f), main =paste(colnames(f)[l]))
  }
  
 colnames(hist.wealthByAgent) <- rname[,1]
 rownames(hist.wealthByAgent) <- arrWeeks[,1]
 colnames(hist.prod.var) <- c("Agente 1 - Agriculture","Agente 1 - Clothing")
 rownames(hist.prod.var) <- arrWeeks[,1]
 browser()
 return (hist.wealthByAgent)
  
  
 
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
  #print(a)
  
  for(l in 1:ncol(a)){
  #windows()
  #plot(a[,l], ylab="Evolution of excess demand", xlab=nrow(a), main =paste(colnames(a)[l]))
  }

# Output evolution of prices
  cat("Evolution of prices","\n")
  b <- round(hist.iter.prices,3)
  #print(b) 
   
  for(l in 1:ncol(b)){
  #windows()
  #plot(b[,l], ylab="Evolution of prices", xlab=nrow(b), main =paste(colnames(b)[l]))
  }

# Output initial and final quantities 
  cat("Initial quantities","\n") 
  
  #print(quant)
  
  for(l in 1:ncol(quant)){
  #windows()
  #plot(quant[,l], ylab="Initial Quantities", xlab=nrow(quant), main =paste(colnames(quant)[l]))
  }

  cat("Final quantities after exchange on the market","\n")
  d <- round(desired,3) 
  #print(d)
  
  for(l in 1:ncol(d)){
  #windows()
  #plot(d[,l], ylab="Final quantities after exchange on the market", xlab=nrow(d), main =paste(colnames(d)[l]))
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
  hist.wealth <- Agent.micro.econ(dados2, 10, dinamic_prod = 0, dinamic_prod_var_FARMER = 0.2, dinamic_prod_var_CLOTHING = 0.2, agent = 1)
  #Agent.micro.econ(dados2,6)
  #final.wealth <- data.frame(FinalWealth = hist.wealth[nrow(hist.wealth), 1])
  
  
  #Testes Riqueza final com alteração de percentagem de variação da produção
  #começa com alteração de 1% na produção 
  
  
  browser()

 # inicialização de variáveis
 # valor - passo de variação de produção
 # teste - contador para o ciclo 
  valor_FARMER <- 0.01
  valor_CLOTHING <- 0.01
  teste <- 1
  valor <- 0.44
  valor_FARMER <- 0.44
  valor_CLOTHING <- 0.44

 # ciclo for para teste de variação da produção entre 1% e 30% (teste in 1:30)/1% e 100% (teste in 1:100) de dois produtos (um agricula e roupas)
 # por um agente agricula que também produz produtos vestuario
  # for(teste in 1:100)
  # {
  cat("Variação da Produção:")
  print(valor*100) 
  cat("%", "\n")
  strat <- "ALLC"
  #chama função com novos parametros para o agente 1 (Agricultura)
  hist.wealth <- Agent.micro.econ(dados2, 10, dinamic_prod = 1, dinamic_prod_var_FARMER = valor_FARMER, dinamic_prod_var_CLOTHING = valor_CLOTHING, agent = c(1,2),strat)
  
  #data.frame com valores finais de riqueza para o agente 1 (Agricultura)
  final.wealth <- rbind(final.wealth, hist.wealth[nrow(hist.wealth), 1])
  #browser()
  
  # incrementa variação da produção de produtos agriculas e 1%
  # valor_FARMER <- valor_FARMER + 0.01
  # incrementa variação da produção de produtos agriculas e 1%
  # valor_CLOTHING <- valor_CLOTHING + 0.01
 
  # incrementa valor de contador
  # teste <- teste + 1
  # valor <- valor + 0.01
  # }
   windows()
   xlabel = paste("Variação da Produção dos dois tipos de produtos e com estrategia ", strat ," (%)")
   plot(x=1:100,y=final.wealth[1:100,1], xlab= xlabel, ylab="Riqueza final do Agente 1 ao fim da simulação")
   abline(h=final.wealth[1,1],col=4,lty=2)

  
  