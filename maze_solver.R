#Algorytm wyszukiwania wszytskich sasiadow danego punktu - funkcja zwraca wektor 4 elementowy.

search_for_neighbours <- function(maze, row, col)
{
  num_rows <- nrow(maze)
  num_cols <- ncol(maze)
  
  neigbhours <- c(0,0,0,0) #lewo, gora, prawo, dol 
  
  if (row > 1)
  {
    neigbhours[2] <- maze[row - 1, col]
  }
  if (row < num_rows)
  {
    neigbhours[4] <- maze[row + 1, col]
  }
  
  if (col > 1)
  {
    neigbhours[1] <- maze[row, col - 1]
  }
  if (col < num_cols)
  {
    neigbhours[3] <- maze[row, col + 1]
  }
  
  return(neigbhours)
}

# Algorytm wyszukiwania sciezki w labieryncie. Dzialanie labirytnu polega na tworzeniu dwoch list:
# - lista 1 zawierajaca punkt startowy,
# - lista 2 pusta

# Algorytm szuka sasiadow wszytskich punktow w liscie 1, a nastepnie umieszcza ich w liscie 2 oraz usuwa zawartosc listy 1,
# Nastepnie algorytm szuka sasiadow wszytskich punktow w liscie 2, a nastepnie umieszcza ich w liscie 1 oraz usuwa zawartosc listy 2.
# Te kroki sa wykonane dopoki lista 1 jest pusta.

pathQ <- function(maze, starting_point, ending_region)
{
  # funkcja sprawdzajaca czy dany obszar znajduje sie w przeszukiwanym labiryncie.
  
  is_in_region <- function(ending_x_min, ending_x_max, ending_y_min, ending_y_max, point_x, point_y)
  {
    if (ending_x_min <= point_x && point_x <= ending_x_max && ending_y_min <= point_y && point_y <= ending_y_max)
    {
      outcome <<- TRUE
    }
  }
  
  num_rows <- nrow(maze)
  num_cols <- ncol(maze)
  zero_matrix <- matrix(0, nrow = num_rows, ncol = num_cols)
  outcome <- FALSE
  
  ending_x_min = min(ending_region$x)
  ending_x_max = max(ending_region$x)
  
  ending_y_min = min(ending_region$y)
  ending_y_max = max(ending_region$y)
  
  #usuwanie otoczenia labiryntu 
  col_to_drop = c()
  row_to_drop = c()
  for (i in 1:num_cols)
  {
    if (sum(maze[, i]) == 0)
    {
      col_to_drop = c(col_to_drop, i)
    }
  }
  
  for (i in 1:num_rows)
  {
    if (sum(maze[i, ]) == 0)
    {
      row_to_drop = c(row_to_drop, i)
    }
  }
  
  if (length(row_to_drop) != 0)
  {
    maze = maze[-row_to_drop,]
  }
  if (length(col_to_drop) != 0)
  {
    maze = maze[, -col_to_drop]
  }
  
  # Macierz zawierajaca informacje o punktach, w ktorych algorytm juz byl 
  zero_matrix[starting_point[1], starting_point[2]] <- 1
  
  # Inicjacja sciezek
  path_1 <- list(starting_point)
  path_2 <- list()
  
  counter <- 0
  while (length(path_1) != 0)
  {
    for (point in path_1)
    {
      neighbours <- search_for_neighbours(maze, point[1], point[2])
      
      if (neighbours[1] == 1 && zero_matrix[point[1], point[2] - 1] == 0)
      {
        is_in_region(ending_x_min, ending_x_max, ending_y_min, ending_y_max, point[1], point[2] - 1)

        path_2 <- c(path_2, list(c(point[1], point[2] - 1)))
        zero_matrix[point[1], point[2] - 1] <- 1
      }
  
      if (neighbours[2] == 1 && zero_matrix[point[1] - 1, point[2]] == 0)
      {
        is_in_region(ending_x_min, ending_x_max, ending_y_min, ending_y_max, point[1] - 1, point[2])
        
        path_2 <- c(path_2, list(c(point[1] - 1, point[2])))
        zero_matrix[point[1] - 1, point[2]] <- 1
      }
      
      if (neighbours[3] == 1 && zero_matrix[point[1], point[2] + 1] == 0)
      {
        is_in_region(ending_x_min, ending_x_max, ending_y_min, ending_y_max, point[1], point[2] + 1)
        
        path_2 <- c(path_2, list(c(point[1], point[2] + 1)))
        zero_matrix[point[1], point[2] + 1] <- 1
      }
      
      if (neighbours[4] == 1 && zero_matrix[point[1] + 1, point[2]] == 0)
      {
        is_in_region(ending_x_min, ending_x_max, ending_y_min, ending_y_max, point[1] + 1, point[2])
        
        path_2 <- c(path_2, list(c(point[1] + 1, point[2])))
        zero_matrix[point[1] + 1, point[2]] <- 1
      }
      
    }
    
    if (outcome == TRUE)
    {
      break
    }
    path_1 <- list()
    
    
    
    for (point in path_2)
    {
      neighbours <- search_for_neighbours(maze, point[1], point[2])
      
      if (neighbours[1] == 1 && zero_matrix[point[1], point[2] - 1] == 0)
      {
        is_in_region(ending_x_min, ending_x_max, ending_y_min, ending_y_max, point[1], point[2] - 1)
        
        path_1 <- c(path_1, list(c(point[1], point[2] - 1)))
        zero_matrix[point[1], point[2] - 1] <- 1
      }
      
      if (neighbours[2] == 1 && zero_matrix[point[1] - 1, point[2]] == 0)
      {
        is_in_region(ending_x_min, ending_x_max, ending_y_min, ending_y_max, point[1] - 1, point[2])
        
        path_1 <- c(path_1, list(c(point[1] - 1, point[2])))
        zero_matrix[point[1] - 1, point[2]] <- 1
      }
      
      if (neighbours[3] == 1 && zero_matrix[point[1], point[2] + 1] == 0)
      {
        is_in_region(ending_x_min, ending_x_max, ending_y_min, ending_y_max, point[1], point[2] + 1)
        
        path_1 <- c(path_1, list(c(point[1], point[2] + 1)))
        zero_matrix[point[1], point[2] + 1] <- 1
      }
      
      if (neighbours[4] == 1 && zero_matrix[point[1] + 1, point[2]] == 0)
      {
        is_in_region(ending_x_min, ending_x_max, ending_y_min, ending_y_max, point[1] + 1, point[2])
        
        path_1 <- c(path_1, list(c(point[1] + 1, point[2])))
        zero_matrix[point[1] + 1, point[2]] <- 1
      }
      
    }
    if (outcome == TRUE)
    {
      break
    }
    path_2 <- list()
    counter <- counter + 1
  }

  return(outcome)
  
}



##########################################
############## PRZYKLAD 1#################
##########################################

maze_matrix <- matrix(c(
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1,1, 1, 1, 1, 0,
  0, 1, 1, 0, 1, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 1, 1, 1, 0, 1, 0,
  0, 0, 0, 0, 1, 0, 0, 0, 1, 0,
  0, 1, 1, 1, 1, 1, 1, 1, 1, 0,
  1, 1, 0, 0, 0, 0, 1, 0, 1, 0,
  0, 1, 1, 1, 1, 0, 1, 0, 1, 0,
  0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
  0, 1, 1, 1, 1, 1, 1, 1, 1, 1
), nrow = 10, byrow = TRUE)

# wizualizacja
image(maze_matrix, col = c("black", "white"), main = "Maze Visualization", axes = TRUE)

#Punkt startowy - postac wektor
start_point <- c(1, 1)

#punkt koncowy do ktorego mozna dojsc - postac lista
end_point <- list(x = 10, y = 10)

#punkt koncowy do ktorego nie mozna dojsc - postac lista
#end_point <- list(x = 6, y = 10)

#wywolanie funkcji
pathQ(maze_matrix, start_point, end_point)




##########################################
############## PRZYKLAD 2#################
##########################################

maze_matrix <- matrix(c(
  1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1,
  1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1,
  1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1,
  1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1,
  1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1,
  1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1,
  1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1,
  1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1
), ncol = 20, byrow = TRUE)

# wizualizacja
image(maze_matrix, col = c("black", "white"), main = "Maze Visualization", axes = TRUE)

#Punkt startowy - postac wektor
start_point <- c(1, 1)

#punkt koncowy do ktorego mozna dojsc - postac lista
end_point <- list(x = 8, y = 6)

#punkt koncowy do ktorego nie mozna dojsc - postac lista
#end_point <- list(x = 10, y = 15)

#wywolanie funkcji
pathQ(maze_matrix, start_point, end_point)





##########################################
############## PRZYKLAD 3#################
##########################################
#przeszukiwanie labirytnu z tresci zadania

#setwd(path)
d0 <- readRDS(file = "./maze.RDS")

#Punkt startowy - postac wektor
startPoint <- c(1, 1)

#punkt koncowy do ktorego mozna dojsc - postac lista - czas wykonania ok.5s
logoPosition <- list(x = 387:413, y = 322:348)

#punkt koncowy do ktorego nie mozna dojsc - postac lista - czas wykonania ok. 7s
#logoPosition <- list(x = 220:230, y = 325:335)

#wywolanie funkcji
pathQ(d0, startPoint, logoPosition)








