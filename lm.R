library(ggplot2)
library(deSolve)
library(RJSONIO)
library(tidyverse)


df = rjson::fromJSON(file="https://api.apify.com/v2/key-value-stores/K373S4uCFR9W1K8ei/records/LATEST?disableRedirect=true")
tdf2 <- data.frame(matrix(unlist(df$infectedDaily), nrow=length(df$infectedDaily), byrow=T))
tdf2$X1 <- as.numeric(as.character(tdf2$X1))
tdf2$X2 <- as.Date(as.character(tdf2$X2))
tdf2 <- tdf2[36:nrow(tdf2),]
names(tdf2) <- c("Pocet", "Den")

inf <- tdf2[tdf2$Pocet != 0, ]
Infected <- inf$Pocet
Day <- 1:(length(Infected))
N <- 10000000 # population of mainland china

old <- par(mfrow = c(1, 2))
plot(Day, Infected, type ="b")
plot(Day, Infected, log = "y")
abline(lm(log10(Infected) ~ Day))
title("Confirmed Cases 2019-nCoV Czechia", outer = TRUE, line = -2)

SIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta/N * I * S
    dI <- beta/N * I * S - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}


init <- c(S = N-Infected[1], I = Infected[1], R = 0)
RSS <- function(parameters) {
  names(parameters) <- c("beta", "gamma")
  out <- ode(y = init, times = Day, func = SIR, parms = parameters)
  fit <- out[ , 3]
  sum((Infected - fit)^2)
}

Opt <- optim(c(0.5, 0.5), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1)) # optimize with some sensible conditions
Opt$message
## [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"

Opt_par <- setNames(Opt$par, c("beta", "gamma"))
Opt_par
##      beta     gamma 
## 0.6746089 0.3253912

t <- 1:70 # time in days
fit <- data.frame(ode(y = init, times = t, func = SIR, parms = Opt_par))
col <- 1:3 # colour

matplot(fit$time, fit[ , 3], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col)
matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col, log = "y")
## Warning in xy.coords(x, y, xlabel, ylabel, log = log): 1 y value <= 0
## omitted from logarithmic plot

points(Day, Infected)
legend("bottomright", c("Susceptibles", "Infecteds", "Recovereds"), lty = 1, lwd = 2, col = col, inset = 0.05)
title("SIR model 2019-nCoV Czechia", outer = TRUE, line = -2)


R0 <- setNames(Opt_par["beta"] / Opt_par["gamma"], "R0")

fit[fit$I == max(fit$I), "I", drop = FALSE] # height of pandemic

max_infected <- max(fit$I)
max_infected / 5 # severe cases
max_infected * 0.06 # cases w
max(fit$I) * 0.02 # max deaths with supposed 2% fatality rate

# simdf <- data_frame("R0"=R0, "Maxf"=max_infected / 5, "IntCare"=max_infected * 0.06)
# simdf

ggplot(fit[0:35,], aes(time, I))+
  geom_line(lwd=1, color="#00868B")+
  geom_point(aes(), color="#DC143C", size=6)+
  ggtitle("COV19CZ")+xlab("Days")+ ylab("Infected")+ theme_minimal()+
  geom_text(aes(label=ceiling(I)), hjust=1, vjust=0, color="white", size=6)+
  theme(panel.background = element_rect(fill ="goldenrod", color = "black"),
        plot.background = element_rect(fill = "#2b374b", color="black"),
        legend.position = "none",plot.title = element_text(color="#17202A"),
        panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major = element_line(
          colour = "black",
          size = 1,
          linetype = "dotted",
          lineend = NULL,
          color = NULL,
          arrow = NULL,
          inherit.blank = FALSE
        ), axis.text = element_text(size=15, colour = "black"),
        axis.title = element_text(size = 15),
        title = element_text(size=20)
        
        
        
  )


