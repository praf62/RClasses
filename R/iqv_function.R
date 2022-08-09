#freq: the absolute frequency distribution
iqv=function(freq, varName){
  print(paste("***", varName, "***"))
  print(freq)
  k = length(freq)
  n = sum(freq)
  sum_n2 = sum(freq*freq)
  n2 = n^2
  ret = k*(n2-sum_n2)/(n2*(k-1))
  #print(ret)
  return(ret)
}
