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