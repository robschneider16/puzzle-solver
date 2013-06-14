from ftpuzzle import *
from pot_solution import *

# ID-DFS: Iterative-Deepening Depth-First Search 
# semi-generic implementation of IDDFS for use in our sliding-tile puzzle explorations
# wfi: 6/12/2013

def iddfs(state, max_depth):
    """iddfs: consumes a board-state and max_depth integer, returns either solution state OR false"""
    for d in range(1,max_depth+1):
        maybe_solution = dfs(state, 0, d, [])
        if maybe_solution:
            print "Found solution: "
            maybe_solution.print_bs()
            return maybe_solution
    print "No solution found at max depth " + str(max_depth)


def dfs(state, depth, max_depth, path_so_far):
    """dfs: standard depth-limited depth-first search consumes state, current-depth and max-depth"""
    # if state is goal, report success
    if state.is_goal_state():
        return state
    # if depth is max_depth, report non-success
    elif depth >= max_depth:
        return False
    # else expand state and recursively dfs on each one
    else:
        expansions = state.expand() # returns a list of children of current state
        # filter expansions wrt path so-far
        path_boards = map(lambda s: s.get_board(), path_so_far)
        filtered_expansions = filter(lambda s: s.get_board() not in path_boards, expansions)
        for a_state in filtered_expansions:
            new_path = list(path_so_far)
            new_path.append(state)
            maybe_solution = dfs(a_state, depth+1, max_depth, new_path)
            if maybe_solution:
                return maybe_solution
    return False


start_state = pot_solution(shuffle_moves=50)
#start_state = pot_solution([4,5,3,1,0,2,6,7,8])
start_state.print_bs()
iddfs(start_state, 25)
