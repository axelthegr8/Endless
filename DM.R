#!/usr/bin/Rscript
install.packages("combinat")
install.packages("~/DM/DeliveryMan_1.1.0.tar", repos = NULL)

library("combinat", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")
library("DeliveryMan", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

source('~/DM/DM.R')

testDM(myFunction, verbose = 0, returnVec = FALSE, n = 10, 
       seed = 21, timeLimit = 250)

testDM <- function(myFunction, verbose = 0, returnVec = FALSE, n = 100, 
          seed = 21, timeLimit = 250) 
{
  if (!is.na(seed)) 
    set.seed(seed)
  seeds = sample(1:25000, n)
  startTime = Sys.time()
  aStar = sapply(seeds, function(s) {
    midTime = Sys.time()
    if (as.numeric(midTime) - as.numeric(startTime) > timeLimit) {
      cat("\nRun terminated due to slowness.")
      return(NA)
    }
    set.seed(s)
    if (verbose == 2) 
      cat("\nNew game, seed", s)
    runDeliveryMan(myFunction, doPlot = F, pause = 0, verbose = verbose == 
                     2)
  })
  endTime = Sys.time()
  if (verbose >= 1) {
    cat("\nMean:", mean(aStar))
    cat("\nStd Dev:", sd(aStar))
    cat("\nTime taken:", as.numeric(endTime) - as.numeric(startTime), 
        "seconds.")
  }
  if (returnVec) 
    return(aStar)
  else return(mean(aStar))
}

myFunction=function(roads,car,packages) {
  
  nextMove=0
  toGo=0
  offset=0
  #If the car is missing load, execute planning,decide on package, send the load in to the aStar algorithm 
  if (car$load==0) {
    packages = planning(packages,car,roads)
    #print(packages)
    load=which(packages[,5]==0)[1]
    #print("load")
    
    #print(load)
    toGo=aStar(roads,car,packages,toGo, load, offset)
  } 
  else {
    offset=2
    toGo=aStar(roads,car,packages,toGo, car$load, offset)[1]
  }
  #If functions for deciding in what direction to make a move according to the aStar algorithm
  if (car$x<toGo[[1]]$x) {nextMove=6}
  else if (car$x>toGo[[1]]$x) {nextMove=4}
  else if (car$y<toGo[[1]]$y) {nextMove=8}
  else if (car$y>toGo[[1]]$y) {nextMove=2}
  else {nextMove=5}
  car$nextMove=nextMove
  car$mem=list()
  return (car)
}

planning = function(packages,car,roads){
  start = list(x=1,y=1)
  packageA = list(x=packages[1,1],y=packages[1,2])
  endA = list(x=packages[1,3],y=packages[1,4])
  packageB = list(x=packages[2,1],y=packages[2,2])
  endB = list(x=packages[2,3],y=packages[2,4])
  packageC = list(x=packages[3,1],y=packages[3,2])
  endC = list(x=packages[3,3],y=packages[3,4])
  packageD = list(x=packages[4,1],y=packages[4,2])
  endD = list(x=packages[4,3],y=packages[4,4])
  packageE = list(x=packages[5,1],y=packages[5,2])
  endE = list(x=packages[5,3],y=packages[5,4])
  
  nodesList = list(list(start=packageA,end=endA),list(start=packageB,end=endB),
                   list(start=packageC,end=endC), list(start=packageD, end = endD), list(start=packageE, end = endE))
  #print(nodesList)
  
  x <- permn(1:length(nodesList))
  #print(x)
  diffLen = list()
  for(i in 1:length(x)){
    #print("i")
    #print(i)
    
    temp_tot = calcManhattanDistance(start,nodesList[[x[[i]][1]]]$start)[1]
    #print("temptot")
    #print(temp_tot)
    
    for(j in 1:4){
      temp_tot = temp_tot + 
        calcManhattanDistance(nodesList[[x[[i]][j]]]$start,nodesList[[x[[i]][j]]]$end)[1] + 
        calcManhattanDistance(nodesList[[x[[i]][j]]]$end,nodesList[[x[[i]][j+1]]]$start)[1]
    }
    temp_tot = temp_tot +
      calcManhattanDistance(nodesList[[x[[i]][5]]]$start,nodesList[[x[[i]][5]]]$end)[1] 
    diffLen[[length(diffLen)+1]] <- temp_tot
  }
  new_packages = matrix(nrow=5,ncol=5)
  for(i in 1:5){
    new_packages[i,] = packages[x[which.min(diffLen)][[1]][i],]
  }
  packages = new_packages
  return (packages = new_packages)
}
calcManhattanDistance=function(loc, target) {
  
  xDist=loc$x-target$x
  yDist=loc$y-target$y
  return (abs(xDist)+abs(yDist))
}

aStar=function(roads, car, packages, toGo, load, offset) {
  
  start = list(x=car$x,y=car$y)
  goal = list(x=packages[load, offset+1], y=packages[load, offset+2])
  closedSet = list()           
  openSet = list(start)         
  cameFrom = matrix(data=list(),ncol=10, nrow=10)
  gScore = matrix(ncol=10, nrow=10)
  fScore = matrix(ncol=10, nrow=10)
  fScore[start$x,start$y] = calcManhattanDistance(start,goal) 
  gScore[start$x,start$y] = 0
  #print(closedSet)
  
  while (length(openSet)>0){
    
    temp_current = arrayInd(which.min(fScore), dim(fScore))
    current_node = list(x=temp_current[1], y = temp_current[2])
    if(current_node$x == goal$x & current_node$y == goal$y){
      return (createPath(cameFrom,current_node))
    }
    
    closedSet[[length(closedSet)+1]] <- current_node
    fScore[current_node$x,current_node$y] = 1000
    for(i in 1:length(openSet)){
      if (openSet[[i]]$x == current_node$x & openSet[[i]]$y == current_node$y){
        openSet[[i]] <- NULL
        break
      }
    }
    
    neighbor_list = list()
    if(current_node$y+1 <= 10){neighbor_list[[length(neighbor_list)+1]] <- list(x=current_node$x,y=current_node$y+1)}
    if(current_node$y-1 >= 1){neighbor_list[[length(neighbor_list)+1]] <- list(x=current_node$x,y=current_node$y-1)}
    if(current_node$x+1 <= 10){neighbor_list[[length(neighbor_list)+1]] <- list(x=current_node$x+1,y=current_node$y)}
    if(current_node$x-1 >= 1){neighbor_list[[length(neighbor_list)+1]] <- list(x=current_node$x-1,y=current_node$y)}
    
    for (i in 1:length(neighbor_list)){
      
      
      if(neighbor_list[i] %in% closedSet){next}
      
      trafic = getTrafic(current_node, neighbor_list[i], roads$hroads, roads$vroads)
      tentative_gScore = gScore[current_node$x,current_node$y] + trafic 
      
      if(neighbor_list[i] %in% openSet){
        if(tentative_gScore >= gScore[neighbor_list[[i]]$x,neighbor_list[[i]]$y]){next}
        else{
          cameFrom[[neighbor_list[[i]]$x,neighbor_list[[i]]$y]] = current_node
          gScore[neighbor_list[[i]]$x,neighbor_list[[i]]$y] = tentative_gScore
          fScore[neighbor_list[[i]]$x,neighbor_list[[i]]$y] = tentative_gScore+calcManhattanDistance(neighbor_list[[i]], goal)
          next
        }
      }
      
      openSet[length(openSet)+1] = neighbor_list[i]
      cameFrom[[neighbor_list[[i]]$x,neighbor_list[[i]]$y]] = current_node
      gScore[neighbor_list[[i]]$x,neighbor_list[[i]]$y] = tentative_gScore
      fScore[neighbor_list[[i]]$x,neighbor_list[[i]]$y] = tentative_gScore+calcManhattanDistance(neighbor_list[[i]], goal)
    }
  } 
}

getTrafic = function(current_node, neighbor_node, hroads, vroads){
  #print(hroads)
  #print(hroads[2,2])
  #print(vroads)
  # print("*******")
  # print(current_node$y)
  # #print(current_node$x)
  # print(".......")
  # print(neighbor_node[[1]]$y)
  # print("--------")
  
  if(neighbor_node[[1]]$x-current_node$x>0){
    #print("Forsta")
    return(hroads[current_node$x,current_node$y])
  }
  else if(neighbor_node[[1]]$x-current_node$x<0){
    #print("Andra")
    return(hroads[neighbor_node[[1]]$x,neighbor_node[[1]]$y])
  }
  else if(neighbor_node[[1]]$y-current_node$y>0){
    #print("Tredje")
    return(vroads[current_node$x,current_node$y])
  }
  else if(neighbor_node[[1]]$y-current_node$y<0){
    #print("Fjarde")
    return(vroads[neighbor_node[[1]]$x,neighbor_node[[1]]$y])
  }
  
  else{
    print("Oops! Something went wrong here!")
  }
}

createPath=function(cameFrom,current_node){
  
  total_path = list(current_node)
  
  while(!is.null(current_node)){
    current_node=cameFrom[[current_node$x,current_node$y]]
    total_path[[length(total_path)+1]] <- list(x=current_node$x,y=current_node$y)
  }
  total_path = rev(total_path)
  if(length(total_path)>2) {
    total_path = total_path[-c(1,2)]
  }
  else {
    total_path = total_path[-c(1)]
  }
  return (total_path)
}

basicDM =
function (roads, car, packages) 
{
  nextMove = 0
  toGo = 0
  offset = 0
  if (car$load == 0) {
    toGo = which(packages[, 5] == 0)[1]
  }
  else {
    toGo = car$load
    offset = 2
  }
  if (car$x < packages[toGo, 1 + offset]) {
    nextMove = 6
  }
  else if (car$x > packages[toGo, 1 + offset]) {
    nextMove = 4
  }
  else if (car$y < packages[toGo, 2 + offset]) {
    nextMove = 8
  }
  else if (car$y > packages[toGo, 2 + offset]) {
    nextMove = 2
  }
  else {
    nextMove = 5
  }
  car$nextMove = nextMove
  car$mem = list()
  return(car)
}
dumbDM =
function (roads, car, packages) 
{
  car$nextMove = sample(c(2, 4, 6, 8), 1)
  return(car)
}
makeDotGrid =
function (n, i) 
{
  plot(rep(seq(1, n), each = n), rep(seq(1, n), n), xlab = "X", 
       ylab = "Y", main = paste("Delivery Man. Turn ", i, ".", 
                                sep = ""))
}
makeRoadMatrices = function (n) 
{
  hroads = matrix(rep(1, n * (n - 1)), nrow = n - 1)
  vroads = matrix(rep(1, (n - 1) * n), nrow = n)
  list(hroads = hroads, vroads = vroads)
}
manualDM = 
function (roads, car, packages) 
{
  if (car$load > 0) {
    print(paste("Current load:", car$load))
    print(paste("Destination: X", packages[car$load, 3], 
                "Y", packages[car$load, 4]))
  }
  car$nextMove = readline("Enter next move. Valid moves are 2,4,6,8,0 (directions as on keypad) or q for quit.")
  if (car$nextMove == "q") {
    stop("Game terminated on user request.")
  }
  return(car)
}
packageOn =
function (x, y, packages) 
{
  notpickedup = which(packages[, 5] == 0)
  onX = which(packages[, 1] == x)
  onY = which(packages[, 2] == y)
  available = intersect(notpickedup, intersect(onX, onY))
  if (length(available) != 0) {
    return(available[1])
  }
  return(0)
}
plotPackages = 
function (packages) 
{
  notpickedup = which(packages[, 5] == 0)
  notdelivered = which(packages[, 5] != 2)
  points(packages[notpickedup, 1], packages[notpickedup, 2], 
         col = "green", pch = 18, cex = 3)
  points(packages[notdelivered, 3], packages[notdelivered, 
                                             4], col = "red", pch = 18, cex = 3)
}
plotRoads = 
function (hroads, vroads) 
{
  for (row in 1:nrow(hroads)) {
    for (col in 1:ncol(hroads)) {
      lines(c(row, row + 1), c(col, col), col = hroads[row, 
                                                       col])
    }
  }
  for (row in 1:nrow(vroads)) {
    for (col in 1:ncol(vroads)) {
      lines(c(row, row), c(col, col + 1), col = vroads[row, 
                                                       col])
    }
  }
}
processNextMove = 
function (car, roads, dim) 
{
  nextMove = car$nextMove
  if (nextMove == 8) {
    if (car$y != dim) {
      car$wait = roads$vroads[car$x, car$y]
      car$y = car$y + 1
    }
    else {
      warning(paste("Cannot move up from y-position", car$y))
    }
  }
  else if (nextMove == 2) {
    if (car$y != 1) {
      car$y = car$y - 1
      
      car$wait = roads$vroads[car$x, car$y]
    }
    else {
      warning(paste("Cannot move down from y-position", 
                    car$y))
    }
  }
  else if (nextMove == 4) {
    if (car$x != 1) {
      car$x = car$x - 1
      car$wait = roads$hroads[car$x, car$y]
    }
    else {
      warning(paste("Cannot move left from x-position", 
                    car$x))
    }
  }
  else if (nextMove == 6) {
    if (car$x != dim) {
      car$wait = roads$hroads[car$x, car$y]
      car$x = car$x + 1
    }
    else {
      warning(paste("Cannot move right from x-position", 
                    car$x))
    }
  }
  else if (nextMove != 5) {
    warning("Invalid move. No move made. Use 5 for deliberate no move.")
  }
  car$nextMove = NA
  return(car)
}
updateRoads = 
function (hroads, vroads) 
{
  r1 = runif(length(hroads))
  r2 = runif(length(hroads))
  for (i in 1:length(hroads)) {
    h = hroads[i]
    if (h == 1) {
      if (r1[i] < 0.05) {
        hroads[i] = 2
      }
    }
    else {
      if (r1[i] < 0.05) {
        hroads[i] = h - 1
      }
      else if (r1[i] < 0.1) {
        hroads[i] = h + 1
      }
    }
    v = vroads[i]
    if (v == 1) {
      if (r2[i] < 0.05) {
        vroads[i] = 2
      }
    }
    else {
      if (r2[i] < 0.05) {
        vroads[i] = v - 1
      }
      else if (r2[i] < 0.1) {
        vroads[i] = v + 1
      }
    }
  }
  list(hroads = hroads, vroads = vroads)
}
runDeliveryMan = 
function (carReady = manualDM, dim = 10, turns = 2000, doPlot = T, 
          pause = 0.1, del = 5, verbose = T) 
{
  roads = makeRoadMatrices(dim)
  car = list(x = 1, y = 1, wait = 0, load = 0, nextMove = NA, 
             mem = list())
  packages = matrix(sample(1:dim, replace = T, 5 * del), ncol = 5)
  packages[, 5] = rep(0, del)
  for (i in 1:turns) {
    roads = updateRoads(roads$hroads, roads$vroads)
    if (doPlot) {
      makeDotGrid(dim, i)
      plotRoads(roads$hroads, roads$vroads)
      points(car$x, car$y, pch = 16, col = "blue", cex = 3)
      plotPackages(packages)
    }
    if (car$wait == 0) {
      if (car$load == 0) {
        on = packageOn(car$x, car$y, packages)
        if (on != 0) {
          packages[on, 5] = 1
          car$load = on
        }
      }
      else if (packages[car$load, 3] == car$x && packages[car$load, 
                                                          4] == car$y) {
        packages[car$load, 5] = 2
        car$load = 0
        if (sum(packages[, 5]) == 2 * nrow(packages)) {
          if (verbose) 
            cat("\nCongratulations! You suceeded in", 
                i, "turns!")
          return(i)
        }
      }
      car = carReady(roads, car, packages)
      car = processNextMove(car, roads, dim)
    }
    else {
      car$wait = car$wait - 1
    }
    if (pause > 0) 
      Sys.sleep(pause)
  }
  cat("\nYou failed to complete the task. Try again.")
  return(NA)
}

findNext=function(packageLocation, packages, i){
  closestPackage=0
  closestDist=10000
  closest=list(closestPackage=closestPackage, dist=closestDist)
  if(length(which(packages[,5]==0))>1){
    #print("----------------")
    #print(length(which(packages[,5]==0)))
    toDestination = calcManhattanDistance(packageLocation,list(x=packages[i,3], y=packages[i,4]))
    for (j in 1:length(packages[,1])) {
      if(packages[j,5]==0 & i!=j){
        target=list(x=packages[j,1], y=packages[j,2])
        manhattanDist=calcManhattanDistance(list(x=packages[i,3], y=packages[i,4]), target)
        if(manhattanDist < closest$dist) {
          closest=list(closestPackage=j, dist=manhattanDist)
        }
      }
    }
    return(toDestination+closest$dist)
  }
  else{return(0)}
  
}