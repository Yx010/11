#The list of the csv is group and features' name
m <- read.csv("D:/data/mydata1.csv",header = T)

x = scale(m[-1],center = F, scale = TRUE)#z-score
m2 = data.frame(x)
m3 = data.frame(m[1])
m4 = cbind(m3,m2)
write.csv(m4,"D:/data/mydata2.csv")


