import random
import math
# solved arrangement
#   ------
# | 0 1 2 |
# | 3 4 5 |
# | 6 7 8 |
#   ------

# representation [ 0,1,2,3,4,5,6,7,8 ]
#     up || position - board_size
#   down || position + board_size
#   left || position - 1
#  right || position + 1
#  edges || if position is less than board size, there is no space up
#        || if the position is more than one board size away from the end of the list, there is no down


class ftpuzzle:
    def __init__(self, bs): # get the size of the board
        self.board_size = bs 
        
    def available_moves(self, board):   # finds the nodes that can be opened
        space = board.index(0) # find the space
        moves = [] # make a list of moves

        # -- logic to find available moves --

        if space < self.board_size: # no up/up
            pass
        else:
            moves.append(space-self.board_size)

        if space > len(board)-self.board_size: # no down/down
            pass
        else:
            moves.append(space+self.board_size)

        if space%self.board_size == self.board_size-1: # no right/right
            pass
        else:
            moves.append(space+1)

        if (space%self.board_size) == 0:
            pass
        else:
            moves.append(space-1)
        # ---------------------------------
        return moves

    def manhat_sum(self, board): # find how far a from home a number is.
        i = 0 # the number
        sum = 0 # the total
        for space in board:
            if space == i:
                pass # print "space " + str(space) + " is in the right place"
            else:
                start = board.index(i)
                hor = abs((start%self.board_size) - (i%self.board_size))
                vert = abs(i//self.board_size - start//self.board_size)
                sum +=(hor+vert)
            i += 1
        print "looks like we have a heuristic of " +str(sum)
        return sum
# depricate
    def find_row(self, loc):
        if loc < self.board_size:
            return 0
        else:
            find_row(loc-self.board_size) +1
                
    def swap(self, swap_node, board): # give it a node and it will be swaped with the space
        space = board.index(0)
        cboard = list(board)
        print "trying to swap " + str(space) + " " + str(swap_node)
        self.print_b(board)
        cboard[space] = cboard[swap_node]
        cboard[swap_node] = 0
        return cboard # potential next game state

    def print_b(self, board): # print the board out so it looks like you would expect.
        start = 0
        end = self.board_size
        while end<=len(board):
            print board[start:end]
            start += self.board_size
            end += self.board_size
