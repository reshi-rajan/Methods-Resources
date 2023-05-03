### Code for Loops

## Load the Tidyverse
library(tidyverse)

# Create a function

square_root <- function(x){
  
  sq_rt <- x^(0.5)
  
  return(sq_rt)
}

# Generate a list
map(1:100, ~square_root(.))

# Make dataframe with rows 
# map_dfc is for columns
map_dfr(1:100, ~square_root)


# For Loop

for(x in 1:100){
  
  print(x^2)
  
}

# While Loop

x = 1

while(x <= 100){ 
  
  print(x^2 -4)
  x = x + 1
}

# Repeat loop

x = 1 

repeat{
  print(x)
  
  x = x + 0.3141

  if(x > 9){
    break
  }
  
}

# Using the next statement

for (x in 1:28){
  if (x == 3 | x == 23 | x == 19){
    next
  }
 print(x)
}

# The key difference between break and next is
# break ends the loop and next skips to the next step