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
        if dcount%10000 == 0:
            print str(dcount) + " times through loop, open has " + str(len(open)) + " and closed " + str(len(closed))
        # set current to best of open
        current_node = heappop(open)[1]
        # if previously considered and this doesn't look to be any better, skip to next on open
        if current_node.get_board() in closed:
            #print "board in closed, ... checking get_f()"
            if current_node.get_g() >= closed[current_node.get_board()].get_g():
                #print "continuing: skiping state " + current_node.get_board()
                continue
        # otherwise, add current to closed
        closed[current_node.get_board()] = current_node
        #print "Current:" + current_node.get_board()
        if current_node.is_goal_state():
            print "Found the goal"
            current_node.print_bs()
            path_state = current_node
            break
        else:
            #print "astar: Expanding"
            expansions = current_node.expand()
            #print "found " + str(len(expansions)) + " children"
            for expansion in expansions:
                if expansion.get_board() not in closed:
                    heappush(open, (expansion.get_f(), expansion))
            #print "Closed has " + str(len(closed)) + " and Open has " + str(len(open))
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
    while not goal_node and current_fringe != [] and fringe_depth < 8:
        fringe_depth += 1
        print "Fringe " + str(fringe_depth) + ": holding " + str(len(current_fringe)) + " search nodes"
        #print "Node nmoves: " + str(map(lambda n: n[1].nmoves, current_fringe.items()))
        next_fringe = {}
        for bs, node in current_fringe.iteritems():
            #print "Fringe " + str(fringe_depth) + ": processing state: " + node.get_board()
            if node.nmoves != fringe_depth - 1:
                raise Exception("move count error")
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

