from ftpuzzle import *
import random
import pot_solution
# how to solve:
# open nodes
# calculate huristic + what has happend so far.
# choose best solution

class ftsolver:
    def __init__(self, shuffle_moves=5): # make a puzzle to solve, create a board that becomes part of a potential_solution
        self.pot_sols = [pot_solution.pot_solution(moves_so_far=0,shuffle_moves=shuffle_moves)]

# We're not making puzzle instances now (ftpuzzle.py does not have a class).
# Solver probably shouldn't be a class either.
# Create a global open_nodes and closed_nods, both of which get manipulated
# by a recursive A* function.

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
