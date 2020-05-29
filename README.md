# csci-241-ch10-1
Drunkards_Walk_Enhanced

Rewrite Walk.asm named as Walk2.asm with several enhancements to make it work like this: (Try to run Walk2.exe)
How many steps the Drunkard to move: 23
(39,10) (38,10) (37,10) (38,10) (38,11) (39,11) (38,11) (38,10) (38,11) (37,11)
(37,10) (37,11) (37,10) (37,9) (37,10) (37,11) (37,12) (36,12) (35,12) (36,12) (
37,12) (37,13) (36,13)
Press any key to continue . . .

                                    *
                                    **O
                                    ***
                                  ***
                                   **

Press any key to continue . . .
To do this, see the requirements and suggestions below:
Part 1
Let the user enter the number of steps that the Drunkard will move. Save it in aWalk.pathsUsed, before calling TakeDrunkenWalk2
Modify the DisplayPosition procedure to output parenthesis coordinates horizontally
Let the start point be (39,10) with steps going one of four directions. The Drunkard's current (X,Y) can't go back to (39,10), but can repeat others
Notice that a diagonal move should not occur. One suggestion is to modify the loop Again like this:
Again:
   ; Insert current valid location X and Y in array.
   ; Use DisplayPosition2 to display position X and Y

 Gen:
   ; Choose a direction (0-3)
   ; Update X or Y for North, South, West, or East

   ; Compare the generated (X, Y) to the origin (39, 10)
   ; if equal
		; From the previous (X, Y)
		; Go back to Gen to re-generate a valid (X, Y)

   ; Point to the next COORD address
loop    Again
Try to use registers without LOCAL variables currX and currY
Part 2
Create the ShowPath procedure to output each step the Drunkard walked with O as the start and * for all others
Call ShowPath immediately after calling TakeDrunkenWalk2, since all the coordinates filled in the structure
Set reversed colors with delay in ShowPath to have a walking animation effect on any color combinations like black on white
So the main can be implemented in the following steps:
Receive the number of steps (Test entering a number less than 30, also make WalkMax = 30 or more)
Required: Call TakeDrunkenWalk2 that in turn, call your new DisplayPosition2
Optional: Call ShowPath to draw paths generated by TakeDrunkenWalk2
Note: You can use .IF directives as you want. Don't make things too complicated. If you fail in assembling error:
1>Walk2.asm(97): error A2075: jump destination too far : by nn byte(s)... ...
it means your loop body too big. You should reduce the loop instructions or create a subroutine to call in the loop. Try it to have fun!
