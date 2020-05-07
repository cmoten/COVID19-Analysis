## data cleaning and transformation
clean_data <- function(data) {
  ## Use Start date to make a day column
  start_date <- mdy("010120")
  ## convert from wide to long format
  data %<>% gather(key=date, value=count, -city)
  ## convert from character to date
  data %<>% mutate(date = date%>% mdy())
  ## convert count to integer
  data %<>% mutate(count = count %>% as.integer())
  na_rows <- which(is.na(data$count))
  data$count[na_rows] <- 0
  ## aggregate by country
  data %<>% as.data.frame()
  return(data)
}

download_data <- function(url, dest_file) {
  tryCatch(
    {
      download.file(url, destfile = dest_file)
    },
    
    error = function(cond) {
      stop("Downlaod failed")
    }
  )
  
  message("Download Complete")
}

#NLXB Function
nlxb_formula <- y ~ a * exp(-b * exp(-c * x))

model_nlxb <- function(formula, start_par, data) {
  tryCatch(
    error = function(cnd) "Model did not converge",
    nlxb(formula,
         start = start_par,
         trace = FALSE,
         data = data
    )
  )
}


compute_lam <- function(k, gamma = 1/4, range = seq(from = 0, to = 12, length.out = 1200)) {
  res <- k * exp(gamma * (range - 1))
  res
}


normalize_vec <- function(vec){
  res <- vec/sum(vec)
  res
}

HDIofGrid = function( probMassVec , credMass=0.95 ) {
  # Arguments:
  #   probMassVec is a vector of probability masses at each grid point.
  #   credMass is the desired mass of the HDI region.
  # Return value:
  #   A list with components:
  #   indices is a vector of indices that are in the HDI
  #   mass is the total mass of the included indices
  #   height is the smallest component probability mass in the HDI
  # Example of use: For determining HDI of a beta(30,12) distribution
  #   approximated on a grid:
  #   > probDensityVec = dbeta( seq(0,1,length=201) , 30 , 12 )
  #   > probMassVec = probDensityVec / sum( probDensityVec )
  #   > HDIinfo = HDIofGrid( probMassVec )
  #   > show( HDIinfo )
  sortedProbMass = sort( probMassVec , decreasing=TRUE )
  HDIheightIdx = min( which( cumsum( sortedProbMass ) >= credMass ) )
  HDIheight = sortedProbMass[ HDIheightIdx ]
  HDImass = sum( probMassVec[ probMassVec >= HDIheight ] )
  return( list( indices = which( probMassVec >= HDIheight ) ,
                mass = HDImass , height = HDIheight ) )
}


