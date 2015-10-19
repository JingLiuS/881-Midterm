setwd("E:\\MSSP\\881\\midterm\\rain gauge")
library(ggplot2)
library(stringr)

# Read the data
theFiles <- dir("raw data/", pattern = "\\.txt")
theFiles
for(a in theFiles)
{
  nameToUse <- str_sub(string = a, start = 1, end = 7)
  temp <- read.csv(file = file.path("raw data", a), skip = 2, stringsAsFactors = F)
  assign(x = nameToUse, value = temp)
}
L1 <- rbind(L0001.t, L0002.t, L0003.t, L0004.t, L0005.t, L0006.t, L0007.t, L0008.t, L0009.t, L0010.t, L0011.t, L0012.t,
            L0101.t, L0102.t, L0103.t, L0104.t, L0105.t, L0106.t, L0107.t, L0108.t, L0109.t, L0110.t, L0111.t, L0112.t,
            L0201.t, L0202.t, L0203.t, L0204.t, L0205.t, L0206.t, L0207.t, L0208.t, L0209.t, L0210.t, L0211.t, L0212.t,
            L0301.t, L0302.t, L0303.t, L0304.t, L0305.t, L0306.t, L0307.t, L0308.t, L0309.t, L0310.t, L0311.t, L0312.t,
            L0401.t, L0402.t, L0403.t, L0404.t, L0405.t, L0406.t, L0407.t, L0408.t, L0409.t, L0410.t, L0411.t, L0412.t)

# The fist column is sequence number, so we remove it
L1 <- L1[, 2:length(L1)]

colnames(L1) <- c(1:24)

# "T" represents trace but it's still in a storm, so we make it 0
# "----" represents no rain, so we make it NA
# We don't know what "M" means, so we also make it NA
L1[L1 == "T   "] <- 0
L1[L1 == "T"] <- 0
L1[L1 == "----"] <- NA
L1[L1 == "M   "] <- NA

# We now transfer it into matrix, remember to do it by row
rain <- as.numeric(t(as.matrix(L1)))
is.numeric(rain)

# Now we can calculate the storm
# We want to skip all the NAs and add up the rainfalls that are continual to form a storm
storm <- function(x)
{
  i = 1
  sum = NULL
  while(i <= length(x))
  {
    tmp = 0
    if(!is.na(x[i])) # you can't compare missing values (num == NA) and (!is.na(num) == FALSE) is silly
    {
      while((!is.na(x[i])) & (x[i] >= 0) & (i <= length(x))) # can't drop the (!is.na) part
      {
        tmp = tmp + x[i]
        i = i + 1
      }
      sum = c(sum, tmp)
    }else
      {
        i = i + 1
      }
  }
  return(sum)
}
rain1 <- storm(rain)

# Remove the storms that have 0 percipitation
rain2 <- rain1[rain1 != 0]

mean(rain2)
var(rain2)

# plot them
qplot(rain2, goem = "histogram", binwidth = 0.2)

# parameters estimation for gamma distribution using MME
alpha <- mean(rain2)^2/var(rain2)
lambda <- mean(rain2)/var(rain2)
alpha
lambda

# parameters estimation for gamma distribution using MLE
n <- length(rain2)
minus.likelihood <- function(theta) {-(n*theta[1]*log(theta[2])-n*lgamma(theta[1])+(theta[1]-1)*sum(log(rain2))-theta[2]*sum(rain2))}
max.likelihood <- nlminb(start=c(0.3613, 1.2762), obj = minus.likelihood)
max.likelihood$par
