#WORKSHOP 3


#------------------------------------PART 1------------------------------------------
rm(list=ls())
DiffusionModel <- function(DiffExp, TimeExp, total_time) {
  #^show product growth by defining parameters within the diffusion model
  D<-10^-6        # Diffusion coefficient (in m^2/s)
  dt<-10^-2         # Time step (in seconds)
  total_time<-100
  TimeSteps <- total_time/dt  # Calculates number of steps given dt and Total Time
  
  dots_x<-rep(0,100)   # Creates 100 molecules to follow (x component of position)
  dots_y<-rep(0,100)   # Creates 100 molecules to follow (y component of position)
  
  dist<-sqrt(2*D*dt)   # Calculates the root mean squared distance that a molecule should travel based on D.
  
  distx<-rnorm(length(dots_x),mean=dist,sd=0.341*dist) # Do not change this line
  disty<-rnorm(length(dots_y),mean=dist,sd=0.341*dist) # Do not change this line
  
  anglex<-sample(seq(0,2*pi,by=0.001*pi),length(dots_x),replace=TRUE) # Do not change this line
  angley<-sample(seq(0,2*pi,by=0.001*pi),length(dots_y),replace=TRUE) # Do not change this line
  
  dots_x<-dots_x+distx*cos(anglex)   # Moves the molecules (x component)
  dots_y<-dots_y+disty*sin(angley)   # Moves the molecules (y component)
  
  plot(dots_x,dots_y)  # Plots the positions of the molecules
}

DiffusionModel(-6, -2, 100) #You can change the dt(-2) to any other number to test the dt changing with total time
#The diffusion model shows how these variables change within each other 
#As you manipulate the 2nd variable = dt, then you can see how it changes with time
#As dt increases (but negatively) the time step is getting smaller so get a more accurate view on the time relative to the step

png("DiffusionModel.png") #To export model created via png file


#----------------------------------------PART 2-----------------------------------------
setwd("~/Desktop")

#FOR THE FIRST D
DiffusionModel <- function(DiffExp, TimeExp, total_time) {
  
  D<-10^-6        # Diffusion coefficient (in m^2/s)
  dt<-10^-2         # Time step (in seconds)
  total_time <- 10
  TimeSteps <- total_time/dt  # Calculates number of steps given dt and Total Time
  
  dots_x<-rep(0,100)   # Creates 100 molecules to follow (x component of position)
  dots_y<-rep(0,100)   # Creates 100 molecules to follow (y component of position)
  
  dots_x_tracking <- dots_x #To track each dot individually
  dots_y_tracking <- dots_y
  
  dist<-sqrt(2*D*dt)   # Calculates the root mean squared distance that a molecule should travel based on D.
  
  for (i in 1:TimeSteps) {
    distx<-rnorm(length(dots_x),mean=dist,sd=0.341*dist) # Do not change this line
    disty<-rnorm(length(dots_y),mean=dist,sd=0.341*dist) # Do not change this line
    
    anglex<-sample(seq(0,2*pi,by=0.001*pi),length(dots_x),replace=TRUE) # Do not change this line
    angley<-sample(seq(0,2*pi,by=0.001*pi),length(dots_y),replace=TRUE) # Do not change this line
    
    dots_x<-dots_x+distx*cos(anglex)   # Moves the molecules (x component)
    dots_y<-dots_y+disty*sin(angley)   # Moves the molecules (y component)
    
    dots_x_tracking <- cbind(dots_x_tracking, dots_x)
    dots_y_tracking <- cbind(dots_y_tracking, dots_y)
  }
  
  qplot(dots_x,dots_y, main="Post-Diffusion Coordinates of 100 Molecules Starting at the Origin",xlab="Final X Coordinate", ylab="Final Y Coordinate")  # Plots the positions of the molecules
}

DiffusionModel(-6, -2, 10)
png("DiffusionModel.png")
#or
plot(DiffusionModel(-6, -2, 10))
png("plot(DiffusionModel).png")


#FOR THE SECOND D
DiffusionModel <- function(DiffExp, TimeExp, total_time) {
  
  D<-10^-2        # Diffusion coefficient (in m^2/s)
  dt<-10^-2         # Time step (in seconds)
  total_time <- 10
  TimeSteps <- total_time/dt  # Calculates number of steps given dt and Total Time
  
  dots_x<-rep(0,100)   # Creates 100 molecules to follow (x component of position)
  dots_y<-rep(0,100)   # Creates 100 molecules to follow (y component of position)
  
  dots_x_tracking <- dots_x #To track each dot individually
  dots_y_tracking <- dots_y
  
  dist<-sqrt(2*D*dt)   # Calculates the root mean squared distance that a molecule should travel based on D.
  
  for (i in 1:TimeSteps) {
    distx<-rnorm(length(dots_x),mean=dist,sd=0.341*dist) # Do not change this line
    disty<-rnorm(length(dots_y),mean=dist,sd=0.341*dist) # Do not change this line
    
    anglex<-sample(seq(0,2*pi,by=0.001*pi),length(dots_x),replace=TRUE) # Do not change this line
    angley<-sample(seq(0,2*pi,by=0.001*pi),length(dots_y),replace=TRUE) # Do not change this line
    
    dots_x<-dots_x+distx*cos(anglex)   # Moves the molecules (x component)
    dots_y<-dots_y+disty*sin(angley)   # Moves the molecules (y component)
    
    dots_x_tracking <- cbind(dots_x_tracking, dots_x)
    dots_y_tracking <- cbind(dots_y_tracking, dots_y)
  }
  
  qplot(dots_x,dots_y, main="Post-Diffusion Coordinates of 100 Molecules Starting at the Origin", xlab="Final X Coordinate", ylab="Final Y Coordinate")  # Plots the positions of the molecules
}

DiffusionModel(-2, -2, 10)
png("DiffusionModel.png")
#or
plot(DiffusionModel(-2, -2, 10))
png("plot(DiffusionModel).png")


#FOR THE THIRD D
DiffusionModel <- function(DiffExp, TimeExp, total_time) {
  
  D<-10^-8        # Diffusion coefficient (in m^2/s)
  dt<-10^-2         # Time step (in seconds)
  total_time <- 10
  TimeSteps <- total_time/dt  # Calculates number of steps given dt and Total Time
  
  dots_x<-rep(0,100)   # Creates 100 molecules to follow (x component of position)
  dots_y<-rep(0,100)   # Creates 100 molecules to follow (y component of position)
  
  dots_x_tracking <- dots_x #To track each dot individually
  dots_y_tracking <- dots_y
  
  dist<-sqrt(2*D*dt)   # Calculates the root mean squared distance that a molecule should travel based on D.
  
  for (i in 1:TimeSteps) {
    distx<-rnorm(length(dots_x),mean=dist,sd=0.341*dist) # Do not change this line
    disty<-rnorm(length(dots_y),mean=dist,sd=0.341*dist) # Do not change this line
    
    anglex<-sample(seq(0,2*pi,by=0.001*pi),length(dots_x),replace=TRUE) # Do not change this line
    angley<-sample(seq(0,2*pi,by=0.001*pi),length(dots_y),replace=TRUE) # Do not change this line
    
    dots_x<-dots_x+distx*cos(anglex)   # Moves the molecules (x component)
    dots_y<-dots_y+disty*sin(angley)   # Moves the molecules (y component)
    
    dots_x_tracking <- cbind(dots_x_tracking, dots_x)
    dots_y_tracking <- cbind(dots_y_tracking, dots_y)
  }
  
  qplot(dots_x,dots_y, main="Post-Diffusion Coordinates of 100 Molecules Starting at the Origin", xlab="Final X Coordinate", ylab="Final Y Coordinate")  # Plots the positions of the molecules
}

DiffusionModel(-8, -2, 10)
png("DiffusionModel.png")
#or
plot(DiffusionModel(-8, -2, 10))
png("plot(DiffusionModel).png")


#FOR THE FOURTH D
DiffusionModel <- function(DiffExp, TimeExp, total_time) {
  
  D<-10^-3       # Diffusion coefficient (in m^2/s)
  dt<-10^-2         # Time step (in seconds)
  total_time <- 10
  TimeSteps <- total_time/dt  # Calculates number of steps given dt and Total Time
  
  dots_x<-rep(0,100)   # Creates 100 molecules to follow (x component of position)
  dots_y<-rep(0,100)   # Creates 100 molecules to follow (y component of position)
  
  dots_x_tracking <- dots_x #To track each dot individually
  dots_y_tracking <- dots_y
  
  dist<-sqrt(2*D*dt)   # Calculates the root mean squared distance that a molecule should travel based on D.
  
  for (i in 1:TimeSteps) {
    distx<-rnorm(length(dots_x),mean=dist,sd=0.341*dist) # Do not change this line
    disty<-rnorm(length(dots_y),mean=dist,sd=0.341*dist) # Do not change this line
    
    anglex<-sample(seq(0,2*pi,by=0.001*pi),length(dots_x),replace=TRUE) # Do not change this line
    angley<-sample(seq(0,2*pi,by=0.001*pi),length(dots_y),replace=TRUE) # Do not change this line
    
    dots_x<-dots_x+distx*cos(anglex)   # Moves the molecules (x component)
    dots_y<-dots_y+disty*sin(angley)   # Moves the molecules (y component)
    
    dots_x_tracking <- cbind(dots_x_tracking, dots_x)
    dots_y_tracking <- cbind(dots_y_tracking, dots_y)
  }
  
  qplot(dots_x,dots_y, main="Post-Diffusion Coordinates of 100 Molecules Starting at the Origin", xlab="Final X Coordinate", ylab="Final Y Coordinate")  # Plots the positions of the molecules
}

DiffusionModel(-3, -2, 10)
png("DiffusionModel.png")
#or
plot(DiffusionModel(-3, -2, 10))
png("plot(DiffusionModel).png")


#FOR THE FIFTH D
DiffusionModel <- function(DiffExp, TimeExp, total_time) {
  
  D<-10^-10        # Diffusion coefficient (in m^2/s)
  dt<-10^-2         # Time step (in seconds)
  total_time <- 10
  TimeSteps <- total_time/dt  # Calculates number of steps given dt and Total Time
  
  dots_x<-rep(0,100)   # Creates 100 molecules to follow (x component of position)
  dots_y<-rep(0,100)   # Creates 100 molecules to follow (y component of position)
  
  dots_x_tracking <- dots_x #To track each dot individually
  dots_y_tracking <- dots_y
  
  dist<-sqrt(2*D*dt)   # Calculates the root mean squared distance that a molecule should travel based on D.
  
  for (i in 1:TimeSteps) {
    distx<-rnorm(length(dots_x),mean=dist,sd=0.341*dist) # Do not change this line
    disty<-rnorm(length(dots_y),mean=dist,sd=0.341*dist) # Do not change this line
    
    anglex<-sample(seq(0,2*pi,by=0.001*pi),length(dots_x),replace=TRUE) # Do not change this line
    angley<-sample(seq(0,2*pi,by=0.001*pi),length(dots_y),replace=TRUE) # Do not change this line
    
    dots_x<-dots_x+distx*cos(anglex)   # Moves the molecules (x component)
    dots_y<-dots_y+disty*sin(angley)   # Moves the molecules (y component)
    
    dots_x_tracking <- cbind(dots_x_tracking, dots_x)
    dots_y_tracking <- cbind(dots_y_tracking, dots_y)
  }
  
  qplot(dots_x,dots_y, main="Post-Diffusion Coordinates of 100 Molecules Starting at the Origin", xlab="Final X Coordinate", ylab="Final Y Coordinate")  # Plots the positions of the molecules
}

DiffusionModel(-10, -2, 10)
png("DiffusionModel.png")
#or
plot(DiffusionModel(-10, -2, 10))
png("plot(DiffusionModel).png")


#------------------------------------PART 3-------------------------------------------
DiffusionModel <- function(DiffExp, TimeExp, total_time, Molecules=100, Track=FALSE) {
  
  D<-10^-6        # Diffusion coefficient (in m^2/s)
  dt<-10^-2         # Time step (in seconds)
  total_time <- 10
  TimeSteps <- total_time/dt  # Calculates number of steps given dt and Total Time
  
  dots_x<-rep(0,Molecules)   # Creates 100 molecules to follow (x component of position)
  dots_y<-rep(0,Molecules)   # Creates 100 molecules to follow (y component of position)
  
  dots_x_tracking <- dots_x
  dots_y_tracking <- dots_y
  
  dist<-sqrt(2*D*dt)   # Calculates the root mean squared distance that a molecule should travel based on D.
  
  for (i in 1:TimeSteps) {
    distx<-rnorm(length(dots_x),mean=dist,sd=0.341*dist) # Do not change this line
    disty<-rnorm(length(dots_y),mean=dist,sd=0.341*dist) # Do not change this line
    
    anglex<-sample(seq(0,2*pi,by=0.001*pi),length(dots_x),replace=TRUE) # Do not change this line
    angley<-sample(seq(0,2*pi,by=0.001*pi),length(dots_y),replace=TRUE) # Do not change this line
    
    dots_x<-dots_x+distx*cos(anglex)   # Moves the molecules (x component)
    dots_y<-dots_y+disty*sin(angley)   # Moves the molecules (y component)
    
    dots_x_tracking <- cbind(dots_x_tracking, dots_x)
    dots_y_tracking <- cbind(dots_y_tracking, dots_y)
  }
  if (Track == TRUE) {
    plot(dots_x,dots_y, main="Post-Diffusion Coordinates of Molecules Starting at the Origin", xlab="Final X Coordinate", ylab="Final Y Coordinate")  # Plots the positions of the molecules
    
    for (i in 1:Molecules) {
      for (j in 1:TimeSteps) {
        segments(dots_x_tracking[i,j],dots_y_tracking[i,j],dots_x_tracking[i,j+1],dots_y_tracking[i,j+1])
      }
    }
  } else {
    library("ggplot2")
    qplot(dots_x,dots_y, main="Post-Diffusion Coordinates of Molecules Starting at the Origin", xlab="Final X Coordinate", ylab="Final Y Coordinate")  # Plots the positions of the molecules
  }}

DiffusionModel(-6,-2, 10, 100, Track = TRUE)

png("DiffusionModel.png")
