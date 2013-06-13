from ftpuzzle import *
import random
from  pot_solution import *

#closed = []
#open = []

def astar_search(start):
    closed = []
    open = [start]
    while open:
        current_node = open.pop(0)
        # move first open to closed
        closed.append(current_node)
        # check if that was the goal and break if so
        if current_node.is_goal_state():
            print "Found the goal"
            current_node.print_bs()
            break
        else:
            # expand that node
            expansions = current_node.expand()
            # filter expansions against closed
            closed_boards = map(lambda s: s.get_board(), closed) # compare only boards, since moves could differ
            filtered_expansions = filter(lambda s: s.get_board() not in closed_boards, expansions)
            # add filtered expansions to open
            open.extend(filtered_expansions)
            # sort open
            open = sorted(open, key=lambda s: s.get_moves() + s.get_heur())
            # continue looping
    print "At end, Closed has " + str(len(closed)) + " and Open has " + str(len(open))


start_state = pot_solution(shuffle_moves=30)
print "Starting with:"
start_state.print_bs()
astar_search(start_state)

