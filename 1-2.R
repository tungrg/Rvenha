setwd('C:/Users/tungrg/Desktop/New folder')
getwd()
#######Bai1
mean_conf_interval= function(data, alpha, sigma = -1)
{
  x = mean(data)
  s = sqrt(var(data))
  e = 0
  if(length(data) >= 30)
  {
    z = qnorm(1-alpha/2)
    if(sigma == -1)
    {
      e = z*s/sqrt(length(data))
    }
    else
    {
      e = z*sigma/sqrt(length(data))
    }
  }
  else
  {
    t = qt(1-alpha/2, df = length(data) - 1)
    e = t*s/sqrt(length(data))
  }
  cat("upper = ", x-e, "lower = ",x+e)
}
############Bai2
prop_conf_interval = function(m,n,alpha)
{
  p =m/n
  if(n*p >= 5 && n*(p-1) >=5)
  {
    z = qnorm(1-alpha/2)
    e = z*sqrt(p*(1-p)/n)
    cat("upper = ", p-e, "lower = ",p+e)
  }
  else{
    cat ("khong ket luan duoc trong truong hop nay") 
  }
}
###########Bai3
mean_hypothesis_onesample = function(data, mu0, sigma, alpha, HA = c('two.side', 'greater', 'smaller'))
{
  mu = mean(data)
  t0 = (mu - mu0)*sqrt(length(data)/sigma)

  if(HA == "two.side")
  {
    t1 = qt(1-alpha/2, length(data)-1)
    if(t0 <= (-t1) || t0 >= t1)
    {
      cat("Bac bo H0")

    }
    else
    {
      cat("Khong du bang chung de ket luan")
    }
    pvalue = 2*pt(abs(t0), length(data)-1, lower.tail = FALSE)
    cat("p-value = " , pvalue)
  }
  else if(HA == "greater")
  {
    t1 = qt(1-alpha, length(data)-1)
    if(t0 >= t1)
    {
      cat("Bac bo H0")
      
    }
    else
    {
      cat("Khong du bang chung de ket luan")
    }
    pvalue = pt(t0, length(data)-1)
    cat("p-value = " , pvalue)
  }
  else if(HA == "smaller")
  {
    t1 = qt(1-alpha, length(data)-1)
    if(t0 <= (-t1))
    {
      cat("Bac bo H0")
      
    }
    else
    {
      cat("Khong du bang chung de ket luan")
    }
    pvalue = pt(t0, length(data)-1, lower.tail =  FALSE)
    cat("p-value = " , pvalue)
  }
  cat("Trung binh mau = ", mu)
}
#####################Bai4
mean_hypothesis_twosample = function(data1, data2,  mu0, sigma1, sigma2, alpha, HA = c('two.side', 'greater', 'smaller'))
{
  if(sigma1 == sigma2)
  {
    Sp = ((length(data1)-1)*sigma1^2 +(length(data2)-1)*sigma2^2)/(length(data1) + length(data2) -2)
    t0 = (mean(data1) - mean(data2))/(Sp*sqrt((1/length(data1)) + (1/length(data2))))
    if(HA == "two.side")
    {
      t1 = qt(1-alpha/2, length(data1)+ length(data2) -2)
      if(abs(t0) > t1)
      {
        cat("Bac bo H0")
        
      }
      else
      {
        cat("Khong du bang chung de ket luan")
      }
      pvalue = 2*pt(abs(t0), length(data1)+ length(data2) -2, lower.tail = FALSE)
      cat("p-value = " , pvalue)
      
    }
    else if(HA == "greater")
    {
      t1 = qt(1-alpha, length(data1)+ length(data2) -2)
      if(t0 > t1)
      {
        cat("Bac bo H0")
        
      }
      else
      {
        cat("Khong du bang chung de ket luan")
      }
      pvalue = pt(t0, length(data1)+ length(data2) -2)
      cat("p-value = " , pvalue)
    }
    else if(HA == "smaller")
    {
      t1 = qt(1-alpha, length(data1)+ length(data2) -2)
      if(t0 < -t1)
      {
        cat("Bac bo H0")
        
      }
      else
      {
        cat("Khong du bang chung de ket luan")
      }
      pvalue = pt(t0, length(data1)+ length(data2) -2)
      cat("p-value = " , pvalue)
    }
  }
  else
  {
    t0 = (mean(data1) - mean(data2))/(sqrt((sigma1^2/length(data1)) +(sigma2^2/length(data2))))
    df = ((sigma1^2/length(data1)) +(sigma2^2/length(data2)))/((sigma1^2/length(data1)^2/(length(data1)-1)) + (sigma2^2/length(data2)^2/(length(data2)-1)))
    if(HA == "two.side")
    {
      t1 = qt(1-alpha/2, df)
      if(abs(t0) > t1)
      {
        cat("Bac bo H0")
        
      }
      else
      {
        cat("Khong du bang chung de ket luan")
      }
      pvalue = 2*pt(t0, df, lower.tail = FALSE)
      cat("p-value = " , pvalue)
      
    }
    else if(HA == "greater")
    {
      t1 = qt(1-alpha, df)
      if(t0 > t1)
      {
        cat("Bac bo H0")
        
      }
      else
      {
        cat("Khong du bang chung de ket luan")
      }
      pvalue = pt(t0, df, lower.tail = FALSE)
      cat("p-value = " , pvalue)
    }
    else if(HA == "smaller")
    {
      t1 = qt(1-alpha, df)
      if(t0 < (-t1))
      {
        cat("Bac bo H0")
        
      }
      else
      {
        cat("Khong du bang chung de ket luan")
      }
      pvalue = pt(t0, df)
      cat("p-value = " , pvalue)
    }
  }
}
data<-read.csv("volume.csv", header = TRUE)
attach(data)
x<-data$machine1
y<-data$machine2
mean_hypothesis_twosample(data$machine1,data$machine2,0, sqrt(var(data$machine1)),sqrt(var(data$machine2)), 0.05, "two.side")
mean_hypothesis_onesample(data$machine1, 12,sqrt(var(data$machine1)),0.05,"two.side")