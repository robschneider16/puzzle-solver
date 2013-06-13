from ftpuzzle import *
#import random
from pot_solution import *


# ID-DFS: Iterative-Deepening Depth-First Search 
# semi-generic implementation of IDDFS for use in our sliding-tile puzzle explorations
# wfi: 6/12/2013

def iddfs(state, max_depth):
    """iddfs: consumes a board-state and max_depth integer, returns either solution state OR false"""
    for d in range(1,max_depth+1):
        maybe_solution = dfs(state, 0, d)
        if maybe_solution:
            print "Found solution: "
            print maybe_solution.get_board()
            print " at depth " + str(d)
            return maybe_solution
    print "No solution found at max depth " + str(max_depth)


def dfs(state, depth, max_depth):
    # if state is goal, report success
    if is_goal_state(state):
        return state
    # if depth is max_depth, report non-success
    elif depth >= max_depth:
        return False
    # else expand state and recursively dfs on each one
    else:
        next_states = expand(state) # returns a list of children of current state
        for a_state in next_states:
            maybe_solution = dfs(a_state, depth+1, max_depth)
            if maybe_solution:
                return maybe_solution
    return False


def is_goal_state(state): # state is a pot_solution (for now)
    return state.get_heur() == 0

def expand(state): # state is a pot_solution (for now)
    return map(state.next_board, available_moves(state.get_board()))

start_state = pot_solution(range(9), 0, 1)
print start_state.get_board()
print available_moves(start_state.get_board())
start_state.shuffle_board(20)
iddfs(start_state, 10)
