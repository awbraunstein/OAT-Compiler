We made Connect4 in OAT.

The goal is to drop pieces into the grid to make 4 in a row, column, or diagonal.
Players alternate turns until there is a winner or the grid is full.

Our game supports 2 human players or one human and one AI.
The way the AI works is it looks for chances to win, and if one doesn't exist,
it looks to block the opponent from winning.

Otherwise, it moves to a random column.

After the game, it displays the winner and gives the option for a new game.
Pieces are colored and differentiated for clarity.

The important files are:

ourOat/main.oat
lib/math.oat
lib/runtime.c
lib/console.c
lib/console.h
lib/console.oat