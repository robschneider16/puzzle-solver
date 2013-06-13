from random import choice
from ftpuzzle import *
 
class pot_solution:
    def __init__(self, new_board, new_moves, heuristic):
        self.moves = new_moves # integer
        self.heur = heuristic  # number
        self.board = new_board # a list of integer

    def shuffle_board(self, num_moves):
        i = 0
        while i<=num_moves:
            moves = available_moves(self.board)
            random_move = choice(moves)
            self.board = swap(random_move, self.board)
            i += 1
        print "after making " + str(i) + " moves your random board is: "
        self.heur = 1
        self.moves = 0
        print_b(self.board)
                
    def next_board(self, move):
        new_board = swap(move, self.board)
        return pot_solution(new_board, self.moves+1, manhat_sum(new_board))

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
