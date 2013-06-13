from random import choice
from ftpuzzle import *

# The functionality of this class is copied to board_state
 
class pot_solution:
    def __init__(self, new_board=range(9), moves_so_far=0, heuristic=None, shuffle_moves=0):
        self.board = new_board # a list of integer
        if shuffle_moves != 0:
            self.shuffle_board(shuffle_moves)
        self.moves = moves_so_far # integer
        if heuristic==None:
            self.heur = manhat_sum(self.board)  # number
        else:
            self.heur = heuristic

    def shuffle_board(self, num_moves):
        i = 0
        while i < num_moves:
            moves = available_moves(self.board)
            random_move = choice(moves)
            self.board = swap(random_move, self.board)
            i += 1
        #print "after making " + str(i) + " moves your random board is: "
        self.heur = 1
        self.moves = 0
        #self.print_bs()
                
    def next_board(self, move):
        new_board = swap(move, self.board)
        return pot_solution(new_board, self.moves+1, manhat_sum(new_board))

    def expand(self):
        """return a list of all states reachable from this current state"""
        return map(self.next_board, available_moves(self.board))

    def is_goal_state(self):
        """returns True if this current state is a qualified goal state"""
        return self.get_heur() == 0

    def get_heur(self):
        return self.heur
    def set_heur(self, new_heur):
        self.heur = new_heur

    def get_moves(self):
        return self.moves
    def set_moves(self):
        self.moves = new_moves

    def inc(self):
        self.moves += 1

    def get_board(self):
        return self.board
    def set_board(self, new_board):
        self.board = new_board

    def print_bs(self): # print the board out so it looks like you would expect.
        start = 0
        end = board_size
        while end<=len(self.board):
            print self.board[start:end],
            if start == 0:
                print "  Prior moves : " + str(self.moves)
            elif start == board_size:
                print "  Estimate yet: " + str(self.heur)
            else:
                print "\n"
            start += board_size
            end += board_size
