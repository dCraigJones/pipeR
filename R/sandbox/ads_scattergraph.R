.libPaths("G:/Delivery/Shared/ACAD/JonesDC/_Support/RWD")

library(tidyverse); theme_set(theme_bw())
library(lubridate)
# utilities ---------------------------------------------------------------


draw_scattergraph <- function(D=8, max.v=5, step.v=1, max.d=16, step.d=2) {
  padding.v <- 0.15
  
  # Draw Scattergraph Template
  plot(0,0
       , xlim=c(-max.v/5,max.v*(1+padding.v))
       , ylim=c(0,max.d)
       , type="n"
       , axes=FALSE
       , xaxs="i"
       , yaxs="i"
       , xlab = "flow velocity (ft/sec)"
       , ylab = "flow depth (in)"
  )
  
  # Draw Scattergraph Gridlines
  for (i in seq(0,max.d,step.d)) {lines(c(0,max.v), c(i,i), col="grey75")}
  for (i in seq(0,max.v,step.v)) {lines(c(i,i), c(0,max.d), col="grey75")}
  
  
  # Calculate Pipe Cut-Away Section
  d <- D/2
  r <- d*D/3
  
  k <- d/2
  y <- seq(0,d, length.out=100)
  
  h <- -sqrt(r^2-d^2/4)
  cutaway.x <- sqrt(r^2-(y-k)^2)+h
  
  # Draw Pipe Cut-Away Sections
  lines(cutaway.x,y+d, lwd=2)
  lines(-cutaway.x,y+d, lwd=2)
  lines(-cutaway.x,y, lwd=2)
  lines(cutaway.x+max.v,y, lwd=2)
  lines(-cutaway.x+max.v,y, lwd=2)
  lines(cutaway.x+max.v,y+d, lwd=2)
  
  # Calculate Pipe Cross-section
  h <- 0
  k <- D/2
  b <- D/2
  a <- -max.v/5
  y <- seq(0,D,length.out=100)
  
  circle.x <- sqrt((1-(y-k)^2/b^2)*a^2)+h
  
  # Draw Pipe Cross-section
  lines(-circle.x,y, lwd=2)
  
  # Draw Pipeline
  
  lines(c(0,max.v), c(D,D), lwd=2)
  lines(c(0,max.v), c(0,0), lwd=2)
  
  # Label Axes
  axis(2, at=seq(0,max.d, step.d))
  axis(1, at=seq(0,max.v,step.v))
}

calc_hyd_radius <- function(y,d,s) {
  theta <- 2*acos(1-2*y/d)
  area <- d^2/8*(theta-sin(theta))
  perimeter <- 0.5*theta*d
  r <- (area/12^2)/(perimeter/12)
  
  return(r)
}


draw_lanfear_coll <- function(y, v, d=8, s=0.4/100) {
  xx <- calc_hyd_radius(y, d, s)
  yy <- v
  
  fit <- lm(yy~xx)
  b <- coef(fit)[2]
  
  C <- b/1.486
  
  n <- sqrt(s)/C
  
  draw_manning(d, n, s)
}

flow <- function(y, d, n, s) {
  
  # y <- seq(0,8,length.out=100)
  # d <- 8
  # n <- 0.013
  # s <- 0.4/100
  
  
  theta <- 2*acos(1-2*y/d)
  area <- d^2/8*(theta-sin(theta))
  perimeter <- 0.5*theta*d
  r <- area/perimeter
  
  q_gpm <- 448.8*1.49/n*(area/12^2)*(r/12)^(2/3)*sqrt(s)
  v_fps <- (q_gpm/448.8)/(area/144)
  
  #lines(v_fps, y)
  
  return(q_gpm)
}

draw_isolines <- function(d=8,q_gpm, max.v=5, max.d=16) {
  #d <- 8
  y <- seq(0,max.d,length.out=1000)
  
  area <- rep(3.1416*d^2/4, length(y))
  
  for (i in 1:length(y)) {
    if (y[i]<=d) {
      theta <- 2*acos(1-2*y[i]/d)
      area[i] <- d^2/8*(theta-sin(theta))
    }
  }
  
  #q_gpm <- 200
  v_fps <- (q_gpm/448.8)/(area/144)
  
  v_fps[v_fps>max.v]=NA
  
  lines(v_fps, y, col="#40AE49", lty=3)
  
  #text(min(v_fps, na.rm=TRUE), d, paste0(q_gpm, " gpm"))
}

draw_manning <- function(d=8, n=0.013, s=0.4/100) {
  y <- seq(0,d,length.out=100)
  
  theta <- 2*acos(1-2*y/d)
  area <- d^2/8*(theta-sin(theta))
  perimeter <- 0.5*theta*d
  r <- area/perimeter
  
  q_gpm <- 448.8*1.49/n*(area/12^2)*(r/12)^(2/3)*sqrt(s)
  v_fps <- (q_gpm/448.8)/(area/144)
  
  lines(v_fps, y, lty=2)
}


# Test Data ---------------------------------------------------------------

data <- read.csv("t:/DCJ/bla.csv", header=T, colClasses = c("character", "numeric", "numeric", "numeric"))

draw_scattergraph()

use <- data %>% 
  mutate(datetime=mdy_hm(Time.Stamp)) %>% 
  mutate(week=week(datetime))

points(data$Velocity..f.s., data$Level..in., col="red", cex=0.7)

draw_manning(8,0.01,0.33/100)

draw_lanfear_coll(data$Level..in.,data$Velocity..f.s., 8, 0.33/100)

draw_isolines(8, 100)
draw_isolines(8, 200)
draw_isolines(8, 300)
draw_isolines(8, 400)



# sandbox -----------------------------------------------------------------

  y <- use$Level..in.
  v <- use$Velocity..f.s.
  d <- 8
  s <- 0.33/100
  
  index <- 0
  curve <- matrix(rep(0, 2*50), ncol=2)
  
  for (i in seq(0,8,length.out=50)) {
    index <- index + 1
    xx <- calc_hyd_radius(y, d-i, s)
    yy <- v
    
    
    fit <- lm(yy~xx)
    curve[index,1] <- i
    curve[index,2] <- summary(fit)$r.squared #mean(resid(fit)^2)
  }
  curve[curve[,2]==0]=NA
  
  draw_scattergraph()
  points(use$Velocity..f.s., use$Level..in., col="red")
  draw_manning(8,0.01, 0.33/100)
  draw_lanfear_coll(use$Level..in., use$Velocity..f.s.)#, 8, 0.33/100)
  draw_isolines(8,50)
  draw_isolines(8,100)
  draw_isolines(8,200)
  draw_isolines(8,300)
  draw_isolines(8,400)
  
  #draw_stevens_schultzbach(use$Level..in., use$Velocity..f.s., 1)
  

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
  
  
  
  
  min.rmse <- min(curve[,2], na.rm=TRUE)
  index <- which(min.rmse==curve[,2])
  depth <- seq(0,8,length.out=50)[index]
  
  xx <- calc_hyd_radius(y, d-depth, s)
  yy <- v
  
  
  fit <- lm(yy~xx)
  
  b <- coef(fit)[2]
  
  C <- b/1.486
  
  n <- sqrt(s)/C
  
  draw_manning(d-depth, n, s)

  

  
  
  draw_week <- function(index, colval) {
  use <- data %>% 
    mutate(datetime=mdy_hm(Time.Stamp)) %>% 
    mutate(week=week(datetime)) %>%
    filter(week==index)
  
  points(use$Velocity..f.s., use$Level..in., col=colval)
  }
  

  data %>% 
    mutate(datetime=mdy_hm(Time.Stamp)) %>% 
    mutate(week=week(datetime)) %>% 
    mutate(x = calc_hyd_radius(Level..in., 8, 0.3/100)) %>%
    mutate(y= Velocity..f.s.) %>%
    group_by(week) %>%
    summarize(b=coef(lm(y~x))[2]) %>%
    mutate(c=b/1.486) %>%
    mutate(n=sqrt(0.3/100)/c) %>%
    ggplot(aes(x=week, y=n)) + geom_line(lwd=1)

  s <- 29:47
  n <- max(s)-min(s)
  pal <- topo.colors(n)
  
  draw_scattergraph()
  for (i in seq_along(s)) {
    draw_week(s[i], pal[i])
    
  }
  
  draw_hour <- function(index, colval) {
    use <- data %>% 
      mutate(datetime=mdy_hm(Time.Stamp)) %>% 
      mutate(week=week(datetime)) %>%
      filter(week>28) %>%
      filter(week<48) %>%
      mutate(hour=hour(datetime)) %>%
      filter(hour==index)
    
    points(use$Velocity..f.s., use$Level..in., col=colval)
  }
  
  data %>% 
    mutate(datetime=mdy_hm(Time.Stamp)) %>% 
    mutate(week=week(datetime)) %>% 
    filter(week>28) %>%
    filter(week<48) %>%
    mutate(hour=hour(datetime)) %>%
    mutate(x = calc_hyd_radius(Level..in., 8, 0.3/100)) %>%
    mutate(y= Velocity..f.s.) %>%
    group_by(hour) %>%
    summarize(b=coef(lm(y~x))[2]) %>%
    mutate(c=b/1.486) %>%
    mutate(n=sqrt(0.3/100)/c) %>%
    ggplot(aes(x=hour, y=n)) + geom_line(lwd=1)
  
  s <- 0:23
  n <- max(s)-min(s)
  pal <- topo.colors(n)
  
  
  draw_scattergraph()
  for (i in seq_along(s)) {
    draw_hour(s[i], pal[i])
    
  }
  
  draw_day <- function(index, colval) {
    use <- data %>% 
      mutate(datetime=mdy_hm(Time.Stamp)) %>% 
      mutate(week=week(datetime)) %>%
      filter(week>28) %>%
      filter(week<48) %>%
      mutate(day=wday(datetime)) %>%
      filter(day==index)
    
    points(use$Velocity..f.s., use$Level..in., col=colval)
  }
  
  data %>% 
    mutate(datetime=mdy_hm(Time.Stamp)) %>% 
    mutate(week=week(datetime)) %>% 
    filter(week>28) %>%
    filter(week<48) %>%
    mutate(day=wday(datetime)) %>%
    mutate(x = calc_hyd_radius(Level..in., 8, 0.3/100)) %>%
    mutate(y= Velocity..f.s.) %>%
    group_by(day) %>%
    summarize(b=coef(lm(y~x))[2]) %>%
    mutate(c=b/1.486) %>%
    mutate(n=sqrt(0.3/100)/c) %>%
    ggplot(aes(x=day, y=n)) + geom_line(lwd=1)
  
  s <- 1:7
  n <- max(s)-min(s)
  pal <- topo.colors(n)
  
  
  draw_scattergraph()
  for (i in seq_along(s)) {
    draw_day(s[i], pal[i])
    
  }
  
  
  data %>% 
    mutate(datetime=mdy_hm(Time.Stamp)) %>% 
    mutate(week=week(datetime)) %>%
    filter(week>28) %>%
    filter(week<48) %>%
    mutate(hour=hour(datetime)) %>%
    mutate(x = calc_hyd_radius(Level..in., 8, 0.3/100)) %>%
    mutate(y= Velocity..f.s.)
  
  %>%
    group_by(hour) %>%
    summarize(b=coef(lm(y~x))[2]) %>%
    mutate(c=b/1.486) %>%
    mutate(n=sqrt(0.3/100)/c) %>%
    ggplot(aes(x=week, y=n)) + geom_line(lwd=1)
  