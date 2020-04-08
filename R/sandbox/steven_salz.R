# reference:
#   https://www.adsenv.com/sites/default/files/whitepapers/sg%2002%20paper%202008-07-31.pdf

load("C:/Users/Craig/Dropbox/R Library/pipeR/R/sandbox/sample_av_data.RData")

scattergraph()

draw_dm(0.33/100)




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