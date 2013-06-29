from heapq import *


def astar_search(start):
    # closed is dictionary mapping boards from get_board() to states
    closed = {}
    open = []
    heappush(open, (start.get_f(), start))
    while open:
        # set current to best of open
        current_node = heappop(open)[1]
        # if previously considered and this doesn't look to be any better, skip to next on open
        if current_node.get_board() in closed:
            if current_node.get_f() >= closed[current_node.get_board()].get_f():
                continue
        # otherwise, add current to closed
        closed[current_node.get_board()] = current_node
        #print current state
        #current_node.print_bs()
        # check if that was the goal and break if so
        if current_node.is_goal_state():
            print "Found the goal"
            current_node.print_bs()
            break
        else:
            #print "astar: Expanding"
            # expand that node
            expansions = current_node.expand()
            #print "astar: Found " + str(len(expansions)) + " child nodes"
            #for s in expansions:
            #    s.print_bs()
            # add filtered expansions to open
            for expansion in expansions:
                heappush(open, (expansion.get_f(), expansion))
            # continue looping
            #print "Closed has " + str(len(closed)) + " and Open has " + str(len(open))
    print "At end, Closed has " + str(len(closed)) + " and Open has " + str(len(open))

def fringe_search(start):
    """
    As a first cut, this implement a fringe-like breadth-first search.
    No sorting, expanding evenly across all states of a given number of prior moves
    """
    prev_fringe = {}
    current_fringe = {hash(start.get_board()): start}
    fringe_count = 0
    goal_node = False
    while not goal_node and current_fringe != []:
        fringe_count += 1
        print "Fringe " + str(fringe_count) + ": holding " + str(len(current_fringe)) + " search nodes"
        next_fringe = {}
        for bshsh, node in current_fringe.iteritems():
            if node.is_goal_state():
                goal_node = node
                break
            children = node.expand()
            for child in children:
                child_bshsh = hash(child.get_board())
                if ((child_bshsh not in prev_fringe) and (child_bshsh not in current_fringe) and (child_bshsh not in next_fringe)):
                    next_fringe[child_bshsh] = child
        prev_fringe = current_fringe
        current_fringe = next_fringe
    if goal_node:
        print "Found the goal"
        goal_node.print_bs()
    else:
        print "No solution found"

