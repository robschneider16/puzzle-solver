from ftpuzzle import *
import random
from  pot_solution import *

#closed = []
#open = []

def astar_search(start):
    closed = []
    open = [start]
    while open:
        print "Closed has " + str(len(closed)) + " and Open has " + str(len(open))
        current_node = open.pop(0)
        print "continuing with "
        print_b(current_node.get_board())
        print "\n"
        # move first open to closed
        closed.append(current_node)
        # check if that was the goal and break if so
        if current_node.is_goal_state():
            print "Found the goal"
            print_b(current_node.get_board())
            break
        else:
            # expand that node
            expansions = current_node.expand()
            print "____ EXPANSIONS ____"
            for s in expansions:
                s.print_bs()
                print "\n"
            print "____________________"
            # filter expansions against closed
            closed_boards = map(lambda s: s.get_board(), closed)
            filtered_expansions = filter(lambda s: s.get_board() not in closed_boards, expansions)
            if len(expansions) > len(filtered_expansions):
                print "FILTERING " + str(len(expansions)) + " to " + str(len(filtered_expansions))
            # add filtered expansions to open
            open.extend(filtered_expansions)
            # sort open
            open = sorted(open, key=lambda s: s.get_moves() + s.get_heur())
            # continue looping




start_state = pot_solution(shuffle_moves=30)
#open.append(start_state)
astar_search(start_state)

