import ftpuzzle
import ftsolver
import random

class main:
    game = ftsolver.ftsolver(3)

    while game.pot_sols[0].get_heur() != 0:
        game.open_nodes()
        game.pot_sols.pop(0)
        game.sort_sol()
    print "ok, there is a solution. it took " + str(game.pot_sols[0].get_moves())
    game.puzzle.print_b(game.pot_sols[0].get_board())
