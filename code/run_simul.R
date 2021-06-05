# Define Several potential outcome functions -----------------------------------

# truncated p score
ps <- function(x) {
  return(min(max(x,.01), .99))
}


glm <- function(x) {
  return(.75*x)
}


smooth <- function(x) {
  return(.05*sin(15*x)+.4*x)
}

nonlinear <- function(x) {
  return(ifelse(x %% .01 < .005,
                .9*x,
                .1*x))
}

