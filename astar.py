from ftpuzzle import *
import random
from  pot_solution import *

closed = []
open = []
def astar_search(state, depth):
    print "Closed has " + str(len(closed)) + " and Open has " + str(len(open))
    if state.is_goal_state():
        print "Found solution: "
        print state.get_board()
        print "at depth " + str(state.get_moves())
        return state
    else:
        expansions = state.expand() 
        # need to filter expansions wrt closed
        closed_boards = map(lambda s: s.get_board(), closed)
        filtered_expansions = filter(lambda s: s.get_board not in closed_boards, expansions)
        if len(expansions) > len(filtered_expansions):
            print "FILTERING " + str(len(expansions)) + " to " + str(len(filter(lambda s: s not in closed, expansions)))
        open.extend(filter(lambda s: s not in closed, expansions))
        sorted(open, key=lambda ps: ps.moves + ps.heur)
        next_state = open.pop(0)
        closed.append(next_state)
        astar_search(next_state, next_state.get_moves())

start_state = pot_solution(shuffle_moves=10)
print start_state.get_board()
astar_search(start_state, 0)
