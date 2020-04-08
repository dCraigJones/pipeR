scattergraph <- function(D_in=8, max.d_in=D_in*2, max.v_fps=5, step.d_in=2, step.v_fps=1) {
  scattergraph_properties <<- NULL
  scattergraph_properties$D_in <<- D_in
  scattergraph_properties$max.d_in <<- max.d_in
  scattergraph_properties$max.v_fps <<- max.v_fps
  
  padding.v <- 0.15
  
  # Draw Scattergraph Template
  plot(0,0
       , xlim=c(-max.v_fps/5,max.v_fps*(1+padding.v))
       , ylim=c(0,max.d_in)
       , type="n"
       , axes=FALSE
       , xaxs="i"
       , yaxs="i"
       , xlab = "flow velocity (ft/sec)"
       , ylab = "flow depth (in)"
  )
  
  # Draw Scattergraph Gridlines
  for (i in seq(0,max.d_in,step.d_in)) {lines(c(0,max.v_fps), c(i,i), col="grey75")}
  for (i in seq(0,max.v_fps,step.v_fps)) {lines(c(i,i), c(0,max.d_in), col="grey75")}
  
  draw_pipe_cutaway(D_in, max.v_fps)
  draw_pipe_xsection(D_in, max.v_fps)
  
  # Draw Pipeline
    lines(c(0,max.v_fps), c(D_in,D_in), lwd=2)
  lines(c(0,max.v_fps), c(0,0), lwd=2)
  
  # Label Axes
  axis(2, at=seq(0,max.d_in, step.d_in))
  axis(1, at=seq(0,max.v_fps,step.v_fps))
}

draw_dm <- function(s, n=0.013) {
    scattergraph_properties$s <<- s
  
    d <- scattergraph_properties$D_in
    
    y <- seq(0,d,length.out=100)
    
    theta <- 2*acos(1-2*y/d)
    area <- d^2/8*(theta-sin(theta))
    perimeter <- 0.5*theta*d
    r <- area/perimeter
    
    q_gpm <- 448.8*1.49/n*(area/12^2)*(r/12)^(2/3)*sqrt(s)
    v_fps <- (q_gpm/448.8)/(area/144)
    
    lines(v_fps, y, lty=2)
}

draw_lc <- function(v_fps, y_in) {
    d <- scattergraph_properties$D_in
    y <- y_in
    v <- v_fps
    s <- 1 # arbitrary value
  
    xx <- suppressWarnings(calc_hyd_radius(y, d))
    yy <- v
    
    fit <- lm(yy~xx)
    b <- coef(fit)[2]
    
    C <- b/1.486
    
    n <- sqrt(s)/C
    
    HC <- sqrt(s)/n
    names(HC) <- "Hyd C"
    
    retVal <- HC
    
    if(!is.null(scattergraph_properties$s)) {

      n_fit <- sqrt(scattergraph_properties$s)/HC
      names(n_fit) <- "Fitted Manning Roughness"
      
      retVal <- n_fit
    }
  
    draw_dm(s, n)
    
    return(retVal)
  }
  
draw_ss <- function(v_fps, y_in, dd_in) {
  
  
}

draw_iso <- function(q_gpm, color="#40AE49") {
    d <- scattergraph_properties$D_in
    max.d <- scattergraph_properties$max.d_in
    max.v <- scattergraph_properties$max.v_fps
  
    y <- seq(0,max.d,length.out=1000)
    
    area <- rep(3.1416*d^2/4, length(y))
    
    for (i in 1:length(y)) {
      if (y[i]<=d) {
        theta <- 2*acos(1-2*y[i]/d)
        area[i] <- d^2/8*(theta-sin(theta))
      }
    }
    
    q_gpm <- 200
    v_fps <- (q_gpm/448.8)/(area/144)
    
    v_fps[v_fps>max.v]=NA
    
    lines(v_fps, y, col=color, lty=3)
  }