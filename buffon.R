gap=1
length=2
success=0
trials=100000

for (iteration in 1:trials) {
  if((runif(1,0,length)+1*sin(runif(1,0,pi))) >= gap)
    success = success+1
}

success/trials