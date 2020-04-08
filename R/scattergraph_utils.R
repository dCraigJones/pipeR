draw_pipe_cutaway <- function(D, max.v_fps) {
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
  lines(cutaway.x+max.v_fps,y, lwd=2)
  lines(-cutaway.x+max.v_fps,y, lwd=2)
  lines(cutaway.x+max.v_fps,y+d, lwd=2)
}

draw_pipe_xsection <- function(D, max.v_fps) {
  # Calculate Pipe Cross-section
  h <- 0
  k <- D/2
  b <- D/2
  a <- -max.v_fps/5
  y <- seq(0,D,length.out=100)
  
  circle.x <- sqrt((1-(y-k)^2/b^2)*a^2)+h
  
  # Draw Pipe Cross-section
  lines(-circle.x,y, lwd=2)
  
}

calc_hyd_radius <- function(y,d) {
  theta <- 2*acos(1-2*y/d)
  area <- d^2/8*(theta-sin(theta))
  perimeter <- 0.5*theta*d
  r <- (area/12^2)/(perimeter/12)
  
  return(r)
}

calc_hyd_radius_surcharged <- function(y,d,ddog) {
  theta <- 2*acos(1-2*y/d)
  perimeter <- 0.5*theta*d
  
  y_ <- y-ddog
  
  theta_ <- 2*acos(1-2*y_/d)
  area_ <- d^2/8*(theta_-sin(theta_))
  
  r_e <- (area_/12^2)/(perimeter/12)
  
  return(r_e)
}


fit_ddog_HC <- function(v, y, d, ddog) {
  xx <- suppressWarnings(calc_hyd_radius_surcharged(y,d,ddog))
  yy <- v
  
  fit <- lm(yy~xx)
  b <- coef(fit)[2]
  
  c <- unname(b/1.486)
  
  return(c)
}

fit_ddog <- function(v, y, d, max.iter=50) {
#  max.iter <- 100
  
  surcharge <- seq(0,d,length.out=(max.iter+1))
  surcharge <- surcharge[1:max.iter]
  
  export <- data.frame(ddog=NULL, rmse=NULL, hc=NULL)
  
  for (i in seq_along(surcharge)) {
    ddog <- surcharge[i]

    xx <- suppressWarnings(calc_hyd_radius_surcharged(y,d,ddog))
    yy <- v
    
    fit <- lm(yy~xx)
    b <- coef(fit)[2]
    
    c <- unname(b/1.486)
    c
    
    rmse <- suppressWarnings(
      sqrt(
        mean(
          (1.486*c*(xx/12)^(2/3) - yy)^2, na.rm=TRUE)))
    
    export_ <- data.frame(ddog=ddog, rmse=rmse, hc=c)
    export <- rbind(export, export_)
    
  }
  
  i <- which.min(export$rmse)
  ddog_fit <- export[i,1]
  
  return(ddog_fit)
}

fit_HC <- function(v,y,d) {
  xx <- suppressWarnings(calc_hyd_radius(y, d))
  yy <- v
  
  fit <- lm(yy~xx)
  b <- coef(fit)[2]
  
  HC <- b/1.486
  names(HC) <- "Hyd C"
  
  return(HC)
  
}