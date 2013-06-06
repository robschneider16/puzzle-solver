import ftpuzzle
import random
import pot_solution
# how to solve:
# open nodes
# calculate huristic + what has happend so far.
# choose best solution

class ftsolver:
    def __init__(self, board_size): # make a puzzle for it to solve, create a board that becomes part of a potential_solution
        self.puzzle = ftpuzzle.ftpuzzle(board_size) # make a puzzle
#        self.start_board = range(0,board_size*board_size) # create a board
#        random.shuffle(self.start_board) # this doesn't work...
        self.start_board = [1,2,5,3,4,0,6,7,8]
        ps = pot_solution.pot_solution(self.start_board, 0, 1)
        self.pot_sols = [ps] # initialize a potential solution --- need to add heuristic here.

    def open_nodes(self):
        print self.pot_sols[0].get_board()
        nodes = self.puzzle.available_moves(self.pot_sols[0].get_board()) # find what moves you can make
        for node in nodes: 
            potential_board = self.puzzle.swap(node, self.pot_sols[0].get_board()) # make the move
            heur = self.puzzle.manhat_sum(potential_board) # see if it was a good move
            self.pot_sols.append(pot_solution.pot_solution(potential_board, self.pot_sols[0].get_moves()+1, heur)) # add it to the list of moves.

    def sort_sol(self):
        self.pot_sols = sorted(self.pot_sols, key=lambda ps: ps.moves + ps.heur)
        for sol in self.pot_sols:
            print sol.get_heur()
