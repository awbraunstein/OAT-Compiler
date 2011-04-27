We made Connect4 in OAT.

The goal is to drop pieces into the grid to make 4 in a row, column, or diagonal.
Players alternate turns until there is a winner or the grid is full.

Our game supports 2 human players or one human and one AI.
The way the AI works is it looks for all possible chances for either player to win.
Then it moves into the open space to either block ot win.
It prioritizes winning.

Otherwise, it moves to a random column.

The AI is not perfect, but makes for a decent single player experience.

After the game, it displays the winner and gives the option for a new game.
Pieces are colored and differentiated for clarity.

The important files are:

ourOat/main.oat
lib/math.oat
lib/runtime.c
lib/console.c
lib/console.h
lib/console.oat