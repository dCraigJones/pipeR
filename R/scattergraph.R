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
  
  
}

draw_lc <- function(v_fps, y_in) {
  
  
}

draw_ss <- function(v_fps, y_in, dd_in) {
  
  
}

draw_iso <- function(q_gpm) {
  
}