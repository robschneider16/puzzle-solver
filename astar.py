#from ftpuzzle import *
#import random
#from  pot_solution import *
#from gstate import *
#from board10state import *

#closed = []
#open = []

def astar_search(start):
    closed = []
    open = [start]
    while open:
        current_node = open.pop(0)
        # move first open to closed
        closed.append(current_node)
        #print current state
        current_node.print_bs()
        # check if that was the goal and break if so
        if current_node.is_goal_state():
            print "Found the goal"
            current_node.print_bs()
            break
        else:
            print "astar: Expanding"
            # expand that node
            expansions = current_node.expand()
            print "astar: Found " + str(len(expansions)) + " child nodes"
            #for s in expansions:
            #    s.print_bs()
            # filter expansions against closed AND open list
            closed_boards = map(lambda s: s.get_board(), closed) # compare only boards, since moves could differ
            all_boards = map(lambda s: s.get_board(), open)
            all_boards.extend(closed_boards)
            filtered_expansions = filter(lambda s: s.get_board() not in all_boards, expansions)
            # add filtered expansions to open
            open.extend(filtered_expansions)
            # sort open
            open = sorted(open, key=lambda s: s.get_moves() + s.get_heur())
            # continue looping
            print "Closed has " + str(len(closed)) + " and Open has " + str(len(open))
    print "At end, Closed has " + str(len(closed)) + " and Open has " + str(len(open))

"""
# Board10::Test
layout = {}
layout[0] = Piece(4, l_tuples, (2,2))
layout[1] = Piece(10, tile_tuples, (1,1))
layout[2] = Piece(7, line_tuples, (2,1))
layout[3] = Piece(12, tile_tuples, (1,1))
layout[4] = Piece(13, sqr_tuples, (2,2))
layout[5] = Piece(15, tile_tuples, (1,1))
layout[6] = Piece(16, line_tuples, (2,1))
layout[7] = Piece(21, tile_tuples, (1,1))
layout[8] = Piece(18, bwl_tuples, (2,2))
bs = Board10State(layout)
astar_search(bs)"""
"""gs = GState()
#start_state = pot_solution(shuffle_moves=50)
#start_state = pot_solution([4,5,3,1,0,2,6,7,8])
#start_state = pot_solution([4,1,10,3,5,6,15,0,8,2,14,7,12,9,13,11])
print "Starting with:"
#gs.print_bs()
#start_state.print_bs()
astar_search(gs)
#gs.print_bs()"""
