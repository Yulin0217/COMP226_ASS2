getOrders <- function(store, newRowList, currentPos, info, params) {

  ###########################################################################
  # You do not need to edit this next part of the code
  ###########################################################################
  allzero <- rep(0, length(newRowList)) # used for initializing vectors
  pos <- allzero

  if (is.null(store))
    store <- initStore(newRowList)
  else
    store <- updateStore(store, newRowList)
  ###########################################################################

  ###########################################################################
  # This next code section is the only one you
  # need to edit for getOrders
  #
  # The if condition is already correct:
  # you should only start computing the moving
  # averages when you have enough (close) prices
  # for the long moving average
  ###########################################################################
  if (store$iter > params$lookbacks$long) {
    #Only consider price in serise
    for (serie in params$series) {
      # Make sure current_close is numeric
      current_close <- as.numeric(newRowList[[serie]]$Close)
      # Make sure prices is an xts object!!
      prices <- as.xts(store$cl[[serie]])
      tma <- getTMA(prices, params$lookbacks)
      pos_sign <- getPosSignFromTMA(list(short = tma$short, medium = tma$medium, long = tma$long))
      pos_size <- getPosSize(current_close)
      pos[serie] <- pos_sign * pos_size
    }
  }
  ###########################################################################

  ###########################################################################
  # You do not need to edit the rest of this function
  ###########################################################################
  marketOrders <- -currentPos + pos

  return(list(store = store, marketOrders = marketOrders,
              limitOrders1 = allzero, limitPrices1 = allzero,
              limitOrders2 = allzero, limitPrices2 = allzero))
}

###############################################################################
checkE01 <- function(prices, lookbacks) {
  if (all(c("short", "medium", "long") %in% names(lookbacks))) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

checkE02 <- function(prices, lookbacks) {
  if (all(sapply(lookbacks, is.integer))) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

checkE03 <- function(prices, lookbacks) {
  if (lookbacks$short < lookbacks$medium & lookbacks$medium < lookbacks$long) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

checkE04 <- function(prices, lookbacks) {
  if ("xts" %in% class(prices)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

checkE05 <- function(prices, lookbacks) {
  # Using unlist() function converts the list to a vector.
  if (nrow(prices) >= max(unlist(lookbacks))) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

checkE06 <- function(prices, lookbacks) {
  if ("Close" %in% colnames(prices)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

###############################################################################
# You should not edit allChecks

atLeastOneError <- function(prices, lookbacks) {
  # return TRUE if any of the error checks return TRUE
  ret <- FALSE
  ret <- ret | checkE01(prices, lookbacks)
  ret <- ret | checkE02(prices, lookbacks)
  ret <- ret | checkE03(prices, lookbacks)
  ret <- ret | checkE04(prices, lookbacks)
  ret <- ret | checkE05(prices, lookbacks)
  ret <- ret | checkE06(prices, lookbacks)
  return(ret)
}

###############################################################################

getTMA <- function(prices, lookbacks, with_checks = FALSE) {

  # prices and lookbacks should pass (return FALSE) when used with
  # the 6 checks, as tested in the following call to allChecks that
  # you should not edit
  if (with_checks)
    if (atLeastOneError(close_prices, lookbacks))
      stop('At least one of the errors E01...E06 occured')

  # Calculate moving averages using close price
  MA_short <- SMA(prices$Close, n = lookbacks$short)
  MA_medium <- SMA(prices$Close, n = lookbacks$medium)
  MA_long <- SMA(prices$Close, n = lookbacks$long)

  # Get the last row of the MA
  last_MA_short <- as.numeric(tail(MA_short, 1))
  last_MA_medium <- as.numeric(tail(MA_medium, 1))
  last_MA_long <- as.numeric(tail(MA_long, 1))
  ret <- list(short = last_MA_short, medium = last_MA_medium, long = last_MA_long)
  return(ret)
}

getPosSignFromTMA <- function(tma_list) {
  if (tma_list$short < tma_list$medium && tma_list$medium < tma_list$long) {
    return(-1)
  } else if (tma_list$short > tma_list$medium && tma_list$medium > tma_list$long) {
    return(1)
  } else {
    return(0)
  }
  return(0)
}

getPosSize <- function(current_close, constant = 5000) {
  return(floor(constant / current_close))
}

###############################################################################
# The functions below do NOT need to be edited
###############################################################################
initClStore <- function(newRowList) {
  clStore <- lapply(newRowList, function(x) x$Close)
  return(clStore)
}

updateClStore <- function(clStore, newRowList) {
  clStore <- mapply(function(x, y) rbind(x, y$Close), clStore, newRowList, SIMPLIFY = FALSE)
  return(clStore)
}

initStore <- function(newRowList, series) {
  return(list(iter = 1, cl = initClStore(newRowList)))
}

updateStore <- function(store, newRowList) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl, newRowList)
  return(store)
}


#print(store$cl[params$series]$Close)
#print(newRowlist)