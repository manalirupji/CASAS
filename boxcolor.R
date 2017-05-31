boxColor<- function (low, high, col_low, col_mid, col_high)
{
  color = col_mid
  if ((low < 0) & (high < 0)) { color = col_low}
  else if((low > 0) & (high >0)) {color = col_high}
  return(as.character(color))
}
