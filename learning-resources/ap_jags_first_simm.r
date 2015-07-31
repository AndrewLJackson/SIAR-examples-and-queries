library(rjags)
library(siar)

# Load in the data
data(geese1demo); data(sourcesdemo)
consumers = geese1demo[,2]
sources = sourcesdemo[1:2,4:5]
con_grid = seq(-35,-5,length=100)
plot(con_grid,dnorm(con_grid,mean=sources[2,1],sd=sources[2,2]),
     type='l',col='red',xlab='d13C',ylab='Probability density')
lines(con_grid,dnorm(con_grid,mean=sources[1,1],sd=sources[1,2]),
      col='blue')
points(consumers,rep(0,9))
legend('topright',legend=c('Grass','Zostera','Consumers'),
       lty=c(1,1,-1),pch=c(-1,-1,1),col=c('red','blue','black'),
       bty = "L")


# run the model

modelstring ='
model {
for(i in 1:N) { 
y[i] ~ dnorm(p_1*s_1+p_2*s_2,1/pow(sigma,2)) 
}
p_1 ~ dunif(0,1)
p_2 <- 1-p_1
s_1 ~ dnorm(s_1_mean,s_1_prec)
s_2 ~ dnorm(s_2_mean,s_2_prec)
sigma ~ dunif(0,10)
}
'
data=list(y=consumers,s_1_mean=sources[1,1],s_1_prec=1/sources[1,2]^2,
          s_2_mean=sources[2,1],s_2_prec=1/sources[2,2]^2,
          N=length(consumers))
model=jags.model(textConnection(modelstring), data=data)
output=coda.samples(model=model,variable.names=c("p_1","p_2"),
                    n.iter=10000)






