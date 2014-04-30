README: Bayesian PokerBot

===Compilation and Run ===
To make, cd into /code and make. 
make clean works as expected. 
make cleanup will remove everything but the actual built program. 
./pokabot will run the program. Nothing else is needed. 

===Instructions for Use===
Follow the instructions that will appear on the screen. Enter prompted responses in the command line. Notes: when prompted for a move, type the move in the command line ("bet" "call" "check" "raise" or "fold"). You do not need to type it in all caps. The first time you play the game, the controller will let you select whether you want to play first. For each following iteration of the game, the controller will alternate the order of play between you and the computer player. 

At the end of each iteration of the game, the interface will display who has won, what cards they won with and the size of the pot. 

===Instructions for using Debugging Output===

Debugging output is helpful because it allows one to get a sense of some of what's happening inside the program. 
To turn debugging output on, set debug_messages (the only value in Switches.ml) to true, then make clean and make. 
When using debugging output, it may prove helpful to first "mkfifo pokerror" in the program's directory, then call the program with "./pokabot 2> pokerror" and in a separate terminal window (in the same directory) do "less -f pokerror". This will put debug messages in one terminal and program user I/O in another, which is a pleasant way to test. 