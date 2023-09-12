# UNO

## Project: 
- A card game based on UNO
- Develped using OCaml

## Structure:
- uni.ml: game interface dealing with the data structure of the json file
- command.ml: handle user inputs
- state.ml: keep track of game states and determine the result of upcoming actions


## Key Features: 
1. Basic rules based on official UNO guide
2. Choose between 2 to 10 players (One user played by user command, and all others played by computer)
3. Winner is the player who empties the cards first
4. Introduce four user command (All other commands will be processed as invalid and the user will be prompted to make another one) 
   - "take": add one random card to the user's inventory
   - "use + color + number / functional cards": drop the card if it follows the game rule
   - "uno": call when there is only one card left
   - "quit": exit the game 
All other commands will be processed as invalid and the user will be prompted to make another one. 
5.  Keeping track of the real-time status of the game: the number of cards held by each player, the current card held by each player.

