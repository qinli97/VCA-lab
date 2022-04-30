# Center = (512,384), radius = 200
# example using the output of the last partipant
x = c(630,702,702,630,512,394,322,322,394,512) # x coord of ROI 1 --10
y  = c(222,322,446,546,584,546,446,322,222,184) # y coord of ROI 1--10

ROI_Freq = read.csv("~/Desktop/VCALab/res3_ROI_Frequency.csv", sep =",", header=TRUE, stringsAsFactors = FALSE)
ROI_Freq[,"max_slope"] = "" # get a new column recording the maximum slope for each subject
ROI_Freq[,"alpha"] = ""  # get a new column recording the degree of each subject preferred for the first fixation on the annulus
ROI_Freq[,"x_loc"] = ""  # where the maximum slope is on the annulus, x coord
ROI_Freq[,"y_loc"] = ""  # where the maximum slope is on the annulus, y coord
ROI_freq = as.matrix(setNames(dff[1:dim(dff)[1],],NULL))

library(scatterplot3d)

# sample 100 points on the circumference of the circle (on the annulus)
samp_xy = function(n,rad = 200, center = c(512,384)){
  x0 = center[1]
  y0 = center[2]
  u = 2*pi*runif(n)
  cbind(x=rad*cos(u)+x0, y = rad*sin(u)+y0)
}


Slope_Angle = function(ROI_freq){
  for (i in 1:dim(ROI_freq)[1]){
    z = ROI_freq[i,] # prequency of the ROI for each subject
    df = cbind.data.frame(x,y,z)
    p1 = scatterplot3d(x,y,z)
    
    # # optimization to find regression coefs
    # ls = function(df,par){with(dfmsum((z-par[1]-par[2]*x-par[3]*y)^2))}
    # rlt = optim(par = c(0,0,0))
    # coef = rlt$par
    # coef
    
    p1 = scatterplot3d(x,y,z, angle = 70, pch = 16, color = "red", main = "Regression plane", type = "h")
    # angle can be changed to get a better view of the 3d plot
    
    text(p1$xyz.convert(df), labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9,10),cex= 2, col = "black")
    lm1 = lm(z~x+y, data = df)
    p1$plane3d(lm1, lty.box = "solid") # will get a 3d regression plane
    
    reg_fit= function(x,y){summary(lm1)$coef[1]+summary(lm1)$coef[2]*x + summary(lm1)$coef[3]*y}
    #reg_fit(x,y) # estimated z value for each ROI location based on the regression
    
    
    
    rand_pts = samp_xy(100, rad = 200, center = c(512,384)) # sample 100 points on the annulus
    
    O_x = 512 # center of the circle, x coord
    O_y = 384 # center of the circle, y coord
    O_z = reg_fit(O_x,O_y) 
    rand_z = reg_fit(rand_pts[,1],rand_pts[,2])
    rad = 200
    tangt = (rand_z-O_z)/rad
    
    max_slope_ind = which.max(tangt)
    max_slope_loc = rand_pts[max_slope_ind,] # (x,y)
    max_slope = tangt[max_slope_ind]
    # c_sq = (max_slope_loc[1]-O_x)^2 + (max_slope_loc[2]-O_y)^2 # r^2
    # alpha = acos(1-c_sq/(2*(rad^2))) 
    alpha = atan2(max_slope_loc[2]-O_y,max_slope_loc[1]-O_x)*180/pi 
    # arctan(y,x), the angle relative to x = 512
    if (alpha <= 0){
      alpha = alpha +360
    }
    x_slope = rand_pts[max_slope_ind,][1]
    y_slope = rand_pts[max_slope_ind,][2]
    ROI_Freq[i,]$max_slope = max_slope
    ROI_Freq[i,]$alpha = alpha
    ROI_Freq[i,]$x_loc = x_slope
    ROI_Freq[i,]$y_loc = y_slope
    
  }
  return(ROI_Freq)
}


sa = Slope_Angle(ROI_freq)
ROI_Frequency <- write.csv(sa, file = "res3_ROI_Frequency_AngleSlope.csv")

p2 = plot(150:750,150:750, type = "n")
#points(rand_pts[,1],rand_pts[,2])
points(O_x,O_y,col = "red")

points(x,y,col = "green", pch = 10)
points(sa$x_loc,sa$y_loc,col = "blue", pch = 20)
text(x,y, labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9,10),cex= 2, col = "steelblue")
polar.plot(roi[1], polar.pos=NULL,labels,label.pos=NULL,
           start=0,clockwise=FALSE,rp.type="r")
points(rand_pts[,1], rand_pts[,2], col = "yellow", pch = 10)



