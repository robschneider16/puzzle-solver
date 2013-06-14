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
    """dfs: standard depth-limited depth-first search consumes state, current-depth and max-depth
     but orders exploration of children by the heuristic function"""
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
        filtered_expansions = sorted(filter(lambda s: s.get_board() not in path_boards, expansions),
                                     key=lambda s: s.get_moves() + s.get_heur())
        for a_state in filtered_expansions:
            new_path = list(path_so_far)
            new_path.append(state)
            maybe_solution = dfs(a_state, depth+1, max_depth, new_path)
            if maybe_solution:
                return maybe_solution
    return False

def beam_search(state, beam_width):
    closed = []
    open_beam = [state]
    while open_beam:
        current_node = open_beam.pop(0)
        closed.append(current_node)
        if current_node.is_goal_state():
            print "Found goal"
            current_node.print_bs()
            break
        else:
            expansions = current_node.expand()
            closed_boards = map(lambda s: s.get_board(), closed) # compare only boards, since moves could differ
            filtered_expansions = filter(lambda s: s.get_board() not in closed_boards, expansions)
            open_beam.extend(filtered_expansions)
            open_beam = sorted(open_beam, key=lambda s: s.get_moves() + s.get_heur())[0:beam_width]
    print "At end, Closed has " + str(len(closed)) + " and Open has " + str(len(open_beam))
            

start_state = pot_solution(shuffle_moves=50)
#start_state = pot_solution([4,5,3,1,0,2,6,7,8])
#start_state = pot_solution([4,1,10,3,5,6,15,0,8,2,14,7,12,9,13,11])
start_state.print_bs()
#iddfs(start_state, 25)
beam_search(start_state, 100)
