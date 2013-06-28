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

"""
def fringe_sort(start):
    fringe = [start]
    threshold = start.get_h()
    found = False
    while not found and now != []:  # some test
        for node in now:
        children = node.expand()
        for child in children:
            if child.get_f() < threshold:
                heappush(now, (child.get_f(), child))
            else:
                heappush(later, (child.get_f(), child))
        try:
            head = heappop(now)
        except:
            threshold = 
"""

