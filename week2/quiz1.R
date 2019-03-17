ssum <- function(arg1, arg2, arg3){
  if (round(arg1) == arg1 && round(arg2) == arg2 && round(arg3) == arg3 && arg3 >= 1){
    if (arg1 < 0 && arg2 < 0){
      num <- seq(arg1, arg2, -arg3)
      sq_sum <- sum(num^2)
    }
    else{
      num <- seq(arg1, arg2, arg3)
      sq_sum <- sum(num^2)
    }
    return(sq_sum)  
  }
  else{
    print("wrong inputs")
  }
}

ssum(-1,8,2)
