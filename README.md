# Boggle

Boggle is a boardgame designed by Alan Turoff. In Boggle the players first assemble a random 4 by 4 grid of letters and then try to find as many words as possible hidden in this grid. The words are formed by tracing paths through adjacent letters without revisiting the same letter. Adjacency can be either horizontal, vertical or diagonal.

![Boggle](http://functional-programming.it.jyu.fi/images/boggle.png)

In this project you need to build a computer opponent for this game. The computer opponent should find as many words as possible in the given boggle grid. To recognize words, you can use this list of 2000 most common words.

## Planning

Like many Haskell programs, the parts doing IO can be separated from the game logic. Also, the actual logic of finding the words is best subdivided into several smaller functions to make the implementation easier. Before implementing your game:

- Identify the parts doing IO and model them as separate operations.
- Think which subtasks you model as helper functions. Last years successful Boggle games were usually divided into 3-7 functions.
- Pay attention to the types you are using: solving problems like this often boils down to identifying the proper data structures. The actual implementation often is just a set of transformations between these structures. Also, you can't use just lists and expect the program to be fast enough to execute.
- Use datatypes from modules such as Data.Set, Data.Map and Data.Vector.
Finally, describe how your parts work together

## Implementation

Implement each function in your plan by using the design recipe given earlier. That is, you should write the type of the function and some examples of how it should work before implementing it. Lastly you should test your functions: check that your examples work as intended and write QuickCheckable properties.

Lastly, compose the parts in to a functioning game. Do not be discouraged if you need to revise your plan or helper functions at this point.
