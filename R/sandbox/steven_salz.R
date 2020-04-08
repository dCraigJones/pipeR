# reference:
#   https://www.adsenv.com/sites/default/files/whitepapers/sg%2002%20paper%202008-07-31.pdf

load("C:/Users/Craig/Dropbox/R Library/pipeR/R/sandbox/sample_av_data.RData")

scattergraph()

draw_dm(0.33/100)



#draw_surcharge <- function() {}
scattergraph()

d <- 8
ddog <- 0
s <- 0.33/100
n <- 0.013

y <- seq(0,d,length.out=100)

r_e <- suppressWarnings(calc_hyd_radius_surcharged(y,d,ddog))


HC <- sqrt(s)/n
HC <- 5.188919

v_fps <- 1.486*HC*(r_e)^(2/3)

lines(v_fps, y, lty=2)




y <- data$Level..in.
v <- data$Velocity..f.s.

xx <- suppressWarnings(calc_hyd_radius_surcharged(y,d,ddog))
yy <- v

fit <- lm(yy~xx)
b <- coef(fit)[2]

c <- unname(b/1.486)
c

rmse <- sqrt(mean((1.486*c*(xx/12)^(2/3) - yy)^2, na.rm=TRUE))




max.iter <- 100

surcharge <- seq(0,d,length.out=(max.iter+1))
surcharge <- surcharge[1:max.iter]

export <- data.frame(ddog=NULL, rmse=NULL, hc=NULL)

for (i in seq_along(surcharge)) {
  ddog <- surcharge[i]
  y <- data$Level..in.
  v <- data$Velocity..f.s.
  
  xx <- suppressWarnings(calc_hyd_radius_surcharged(y,d,ddog))
  yy <- v
  
  fit <- lm(yy~xx)
  b <- coef(fit)[2]
  
  c <- unname(b/1.486)
  c
  
  rmse <- sqrt(mean((1.486*c*(xx/12)^(2/3) - yy)^2, na.rm=TRUE))
  
  export_ <- data.frame(ddog=ddog, rmse=rmse, hc=c)
  export <- rbind(export, export_)
  
}

i <- which.min(export$rmse)
ddog_fit <- export[i,1]
hc_fit <- export[i,3]


d <- 8
ddog <- ddog_fit
s <- 1
n <- hc_fit



y <- seq(0,d,length.out=100)

r_e <- suppressWarnings(calc_hyd_radius_surcharged(y,d,ddog))


HC <- sqrt(s)/n

v_fps <- 1.486*HC*(r_e/12)^(2/3)

lines(v_fps, y, lty=2)







draw_stevens_schultzbach <- function(y, v, ddog, d=8, s=0.33/100) {
    xx <- calc_hyd_radius(y-ddog, d, s)
    yy <- v
    
    fit <- lm(yy~xx)
    b <- coef(fit)[2]
    
    C <- b/1.486
    
    n <- sqrt(s)/C
    
    y <- seq(0,d,length.out=1000)
    
    y_ <- y-ddog
    theta <- 2*acos(1-2*y_/d)
    area <- d^2/8*(theta-sin(theta))
    perimeter <- 0.5*theta*d
    r <- area/perimeter
    
    q_gpm <- 448.8*1.49/n*(area/12^2)*(r/12)^(2/3)*sqrt(s)
    v_fps <- (q_gpm/448.8)/(area/144)
    
    lines(v_fps, y, lty=2)
}