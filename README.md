# Azul
Implementation of the Azul board game for B351

# Playing
To get a game going:
1. clone this repo then cd into src.
2. run `racket -i`
3. enter `(require "main.rkt")`
4. enter `(azul)`


===============================

To make a move, input a list of three things: a factory number, a color label and a staging line number. 

Example: `(1 blue 1)` takes all the blue tiles from factory f-1 and places them in staging line 1. 

===============================


To take from the middle factory, enter factory number `-1`. 

To send tiles directly to your overflow (i.e. no staging line can host them), enter line number 6. 

Example:  `(-1 black 6)` takes all the black tiles from the middle factory and places them in the overflow row.

===============================

# Colors
In your console/terminal/REPL, some of the colors may look out of place from those in the given list of color options.

If the color of the tile you'd like looks purple, the color option for you is _lightblue_. 

If the color of the tile you'd like looks green, the color option for you is _black_.

If the color of the tile you'd like looks grey, the color option for you is _black_.


Ideas:
[Online implemetation of Azul](http://boardwebgames.com/rojo/)
* This version has an AI which kind of does some analysis and picks the move that
  seems to be the best. Doesn't really use minimax.
* We can maybe use this AI's ideas to implement our heuristic function
