getAllPureStrategyNE <- function(game) 
{
  
  wyplaty_graczy <- list()
  
  for (i in 1:length(game))
  {
    wyplaty_graczy <- c(wyplaty_graczy, list(1:dim(game[[i]])[i]))
  }
  
  #stworzenie macierzy zawierajacej wszystkie kombinacje strategii 
  strategie <- do.call(expand.grid, wyplaty_graczy)
  strategie <- as.matrix(strategie)
  
  # inicjacja wynikowej listy
  nash_list = list()
  
  # sprawdzanie czy dana strategia jest NE
  for (i in 1:nrow(strategie)) 
  {
    
    # inicjacja wektora zawierajacego dana kombinacje strategii
    a <- c()
    
    for (t in 1:length(strategie[i, ]))
    {
      a <- c(a, strategie[i, t])
    }
    
    # wektor pomocniczy do sprawdzania strategii
    test_vec <- a
    
    a_matrix <- matrix(a, ncol = length(game), byrow = TRUE)
    
    is_nash <- TRUE
    
    for (player in 1:length(game)) # by players 
    {
      test_vec <- a
      for (strategy in 1:dim(game[[player]])[player]) # by players strategy
      {
        test_vec[player] <- strategy
        test_matrix <- matrix(test_vec, ncol = length(game), byrow = TRUE)
        
        # wyplata gracza przy badanej kombinacji strategii
        old_val <- game[[player]][a_matrix]
        
        # wyplata gracza przy nowej kombinacji
        cur_val <- game[[player]][test_matrix]
        
        if (old_val < cur_val)
        {
          is_nash <- FALSE
          break
        }
      }
    }
      
    if(is_nash)
    {
      new_name <- paste(a, collapse = '')
      nash_list[[new_name]] <- unname(a)
    }
  }
  
  print(nash_list)
}

##########################################
############## PRZYKLADY #################
##########################################

game <- list( "player1" = array(c(5, 10, 1, 2), dim = c(2, 2)),
              "player2" = array(c(5, 1, 10, 2), dim = c(2, 2)))

# game <- list(
#   "player1" = array(c(1, 0, 0, 1), dim = c(2, 2)),
#   "player2" = array(c(1, 0, 0, 1), dim = c(2, 2)))
# 
# game <- list(
#   "player1" = array(c(3, 2, 0, 2), dim = c(2, 2)),
#   "player2" = array(c(3, 0, 2, 2), dim = c(2, 2)))
# 
# game <- list(
#   "player1" = array(c(0, 1, 1, 0), dim = c(2, 2)),
#   "player2" = array(c(0, 1, 1, 0), dim = c(2, 2)))

# game <- list( "player1" = array(c(0, 1, 1, 1, 1, 1, 1, 0), dim = c(2, 2, 2)),
#               "player2" = array(c(0, 1, 1, 1, 1, 1, 1, 0), dim = c(2, 2, 2)),
#               "player3" = array(c(0, 1, 1, 1, 1, 1, 1, 0), dim = c(2, 2, 2)))

# game <- list( "player1" = array(c(1, 0, 0, 0, 0, 0, 0, 1), dim = c(2, 2, 2)),
#               "player2" = array(c(1, 0, 0, 0, 0, 0, 0, 1), dim = c(2, 2, 2)),
#               "player3" = array(c(1, 0, 0, 0, 0, 0, 0, 1), dim = c(2, 2, 2)))

# game <- list( "player1" = array(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), dim = c(2, 2, 2, 2)),
#               "player2" = array(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), dim = c(2, 2, 2, 2)),
#               "player3" = array(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), dim = c(2, 2, 2, 2)),
#               "player4" = array(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), dim = c(2, 2, 2, 2)))

# game <- list( "player1" = array(c(1,1,2,1,1,1,0,1,1), dim = c(3, 3)),
#               "player2" = array(c(1,1,0,1,1,1,2,1,1), dim = c(3, 3)))

getAllPureStrategyNE(game)






