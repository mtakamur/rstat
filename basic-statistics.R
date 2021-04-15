# Calculate average of given vector.
# If the parameter is not vector type returns NULL.
getAverage <- function(source) {
  if (!is.vector(source)) {
    print("The parameter \"source\" should be vector.")
    return(NULL)
  }
  sum <- 0
  numberOfElements <- length(source)
  for (i in 1:numberOfElements) {
    sum = sum + source[i]
  }
  return(sum / numberOfElements)
}

# Calculate variance of given vector from mean.
# If the parameter is not vector type returns NULL.
getVariance <- function(source, mean) {
  if (!is.vector(source)) {
    print("The parameter \"source\" should be vector.")
    return(NULL)
  }
  if (missing(mean)) {
    mean <- getAverage(source)
  }
  if (isSingleElement(source)) {
    print("Variance cannot be estimated for single element.")
    return(NULL)
  }
  numberOfElements <- length(source)
  variance <- 0
  for (i in 1:numberOfElements) {
    variance = variance + (source[i] - mean) ^ 2
  }
  return(variance / (numberOfElements - 1))
}

# Calculate standard diviation of given vector
# "soruce" is at least needed to obtain the result
getStandardDiviation <- function(source, variance, mean) {
  if (!is.vector(source)) {
    print("The parameter \"source\" should be vector.")
    return(NULL)
  }
  if (missing(variance)) {
    variance <- getVariance(source, mean)
  }
  return(sqrt(variance))
}

# Calculate standard error of given vector
# "soruce" is at least needed to obtain the result
getStandardError <- function(source, diviation, variance, mean) {
  if (!is.vector(source)) {
    print("The parameter \"source\" should be vector.")
    return(NULL)
  }
  if (missing(diviation)) {
    diviation <- getStandardDiviation(source, variance, mean)
  }
  return(diviation / sqrt(length(source)))
}

# Check if number of elements of given source is only one.
isSingleElement <- function(source) {
  return(length(source) == 1)
}