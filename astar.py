from heapq import *


def astar_search(start):
    # closed is dictionary mapping boards from get_board() to states
    closed = {}
    open = []
    heappush(open, (start.get_f(), start))
    dcount = 0
    path_state = None
    while open and dcount >= 0:
        dcount += 1
        # set current to best of open
        current_node = heappop(open)[1]
        # if previously considered and this doesn't look to be any better, skip to next on open
        if current_node.get_board() in closed:
            #print "board in closed, ... checking get_f()"
            if current_node.get_g() >= closed[current_node.get_board()].get_g():
                print "continuing: skiping state ..."
                current_node.print_bs()
                continue
        # otherwise, add current to closed
        print "Depth " + str(dcount) + ": processing state: " + current_node.get_board()
        closed[current_node.get_board()] = current_node
        #print current state
        #print "Current:"
        #current_node.print_bs()
        # check if that was the goal and break if so
        if current_node.is_goal_state():
            print "Found the goal"
            current_node.print_bs()
            path_state = current_node
            break
        else:
            #print "astar: Expanding"
            # expand that node
            expansions = current_node.expand()
            #print "Expanded " + str(len(expansions)) + " child nodes"
            #for s in expansions:
            #    s.print_bs()
            # add filtered expansions to open
            for expansion in expansions:
                if expansion.get_board() not in closed:
                    heappush(open, (expansion.get_f(), expansion))
            # continue looping
            #print "Closed has " + str(len(closed)) + " and Open has " + str(len(open))
            #print ""
    print "At end, Closed has " + str(len(closed)) + " and Open has " + str(len(open))
    while path_state != None:
        path_state.print_bs()
        path_state = path_state.prev_state

def fringe_search(start):
    """
    As a first cut, this implement a fringe-like breadth-first search.
    No sorting, expanding evenly across all states of a given number of prior moves
    """
    prev_fringe = {}
    current_fringe = {start.get_board(): start}
    fringe_depth = 0
    goal_node = False
    while not goal_node and current_fringe != []:
        fringe_depth += 1
        print "Fringe " + str(fringe_depth) + ": holding " + str(len(current_fringe)) + " search nodes"
        next_fringe = {}
        for bs, node in current_fringe.iteritems():
            #print "Fringe " + str(fringe_depth) + ": processing state: " + node.get_board()
            if node.is_goal_state():
                goal_node = node
                break
            children = node.expand()
            for child in children:
                child_bs = child.get_board()
                if ((child_bs not in prev_fringe) and (child_bs not in current_fringe) and (child_bs not in next_fringe)):
                    next_fringe[child_bs] = child
        prev_fringe = current_fringe
        current_fringe = next_fringe
    if goal_node:
        print "Found the goal"
        goal_node.print_bs()
    else:
        print "No solution found"

