significant <- function (low, high)
{
  indicator = 0
  if (abs(low - high) < 0.01) { indicator = 3}
  else if ((low < 0) & (high < 0)) { indicator = 1}
  else if((low > 0) & (high >0)) {indicator = 2}
  return(as.numeric(indicator))
}
