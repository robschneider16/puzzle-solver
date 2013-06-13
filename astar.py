from ftpuzzle import *
import random
from  pot_solution import *

closed_list = []
states = []
def astar_search(state, depth):
    if state.is_goal_state():
        print "Found solution: "
        print state.get_board()
        print "at depth " + str(state.get_moves())
        return state

    else:
        states.extend(state.expand())
        closed_list.append(states.pop(0))
        sorted(states, key=lambda ps: ps.moves + ps.heur)
        astar_search(states[0], depth+1)

start_state = pot_solution(shuffle_moves=3)
print start_state.get_board()
astar_search(start_state, 0)
