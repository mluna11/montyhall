#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
  a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
  return( a.game )
} 



#' @title
#'   Select initial door
#'   
#' @description
#'   `select_door()` randomly selects and opens the first door in the 
#'   game
#'   
#' @details
#'   This is the first door the contestant chooses in the game. 
#'   
#' @param 
#'   No arguments are used by the function
#'   
#' @return 
#'   The function returns a length 1 numerical vector indicating
#'   the first door in play. 
#'   
#' @examples
#'   select_door()
#'   
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Open goat door
#'   
#' @description
#'   `open_goat_door()` selects the second door to open in the game
#'    
#' @details
#'   This is the door the game host opens.The host will always open a door
#'   with a goat behind it. 
#'   
#' @param
#'   The second door in play can not be the initial door the contestant 
#'   selected nor can it be the door with the car behind it.
#'    
#' @return 
#'   The function returns three vectors. The first is a length 3
#'   character vector indicating the positions of goats and the car. 
#'   The second is a length 1 numerical vector indicating the initial
#'   door selected by the contestant. The third is a length
#'   1 numerical vector indicating the door selected by the host. 
#'   
#' @examples
#'   this.game <- create_game()
#'   this.game
#'   my.initial.pick <- select_door()
#'   my.initial.pick
#'   open_goat_door( this.game, my.initial.pick )
#' 
#' @export
open_goat_door <- function( game, a.pick )
{
  doors <- c(1,2,3)
  # if contestant selected car,
  # randomly select one of two goats 
  if( game[ a.pick ] == "car" )
  { 
    goat.doors <- doors[ game != "car" ] 
    opened.door <- sample( goat.doors, size=1 )
  }
  if( game[ a.pick ] == "goat" )
  { 
    opened.door <- doors[ game != "car" & doors != a.pick ] 
  }
  return( opened.door ) # number between 1 and 3
}



#' @title
#'   Change door
#'   
#' @description
#'   `change_door()` selects the final unopened door in the game.
#' 
#' @details
#'   The contestant is given the option to exchange their initial 
#'   selection for the remaining unopened door.
#' 
#' @param
#'   If the contestant decides to stay with their initial selection,
#'   then that door is the final selected door and not the other unopened door.
#'   If the contestant decides to choose the other unopened door, 
#'   then that door is selected and made the final selected door in the game.  
#'    
#' @return
#'   The function returns five vectors. The first and fourth are a length 1 numerical
#'   vector indicating the initial door selected by the contestant.  
#'   The second is a length 1 numerical vector indicating the 
#'   the final unopened door should the contestant choose to exchange it
#'   for their initial selection.The third is a length 3
#'   character vector indicating the positions of goats and the car.
#'   The fifth is a length 1 numerical vector indicating the final selected door
#'   in the game. 
#'  
#' @examples
#'   opened.door <- open_goat_door( this.game, my.initial.pick )
#'   change_door( stay=T, opened.door=opened.door, a.pick=my.initial.pick )
#'   change_door( stay=F, opened.door=opened.door, a.pick=my.initial.pick )
#'   my.final.pick <- change_door( stay=F, opened.door=opened.door, a.pick=my.initial.pick )
#'   this.game
#'   my.initial.pick
#'   my.final.pick 
#' 
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
  doors <- c(1,2,3) 
  
  if( stay )
  {
    final.pick <- a.pick
  }
  if( ! stay )
  {
    final.pick <- doors[ doors != opened.door & doors != a.pick ] 
  }
  
  return( final.pick )  # number between 1 and 3
}



#' @title
#'   Determine winner
#' 
#' @description
#'   `determine_winner()`determines the game outcome. 
#' 
#' @details
#'   After the contestant has made their final selection, the game outcome is
#'   determined.
#' 
#' @param
#'   If the final picked door has the car, then the contestant wins.
#'   If the final picked door has a goat, then the contestant loses. 
#'  
#' @return
#'   The function returns four vectors. The first is a length 3
#'   character vector indicating the positions of goats and the car.
#'   The second is a length 1 numerical vector indicating the initial 
#'   door selected by the contestant. The third is a length 1 character 
#'   vector indicating the game outcome if the contestant choose to 
#'   stay with their initial selected door. The third is a length 1 
#'   character vector indicating the game outcome if the contestant 
#'   choose to exchange their initial selected door for the 
#'   final unopened door.
#'  
#' @examples
#'   this.game
#'   my.initial.pick
#'   my.final.pick <- change_door( stay=T, opened.door=opened.door, a.pick=my.initial.pick )
#'   determine_winner( final.pick=my.final.pick, game=this.game )
#'   my.final.pick <- change_door( stay=F, opened.door=opened.door, a.pick=my.initial.pick )
#'   determine_winner( final.pick=my.final.pick, game=this.game )
#' 
#' @export
determine_winner <- function( final.pick, game )
{
  if( game[ final.pick ] == "car" )
  {
    return( "WIN" )
  }
  if( game[ final.pick ] == "goat" )
  {
    return( "LOSE" )
  }
}





#' @title
#'   Play game
#' 
#' @description
#'   `play_game()`plays a full round of the game and returns the game outcome. 
#' 
#' @details
#'   This function plays a full round of the game and returns the game outcome. 
#' 
#' @param
#'   fill in
#'
#' @return
#'   fill in
#'  
#' @examples
#'   fill in
#' 
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )
  
  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )
  
  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'   Play game loop
#' 
#' @description
#'   `play_n_games( n= )` plays the game on a loop for n amount of times. 
#' 
#' @details
#'   fill in
#' 
#' @param
#'   fill in
#'  
#' @return
#'  fill in
#'  
#' @examples
#'   fill in
#' 
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1
  
  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )
  
  table( results.df ) %>% 
    prop.table( margin=1 ) %>%  # row proportions
    round( 2 ) %>% 
    print()
  
  return( results.df )
  
}
