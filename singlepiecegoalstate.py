import cProfile

from gstate import *
from astar import *
from drawer import artist


# declare the goal_state of the board
goal_state = {"gpc":[1]} # for Board 10, Variants 11 and 12
#goal_state = {"gpc":[2]} # for Climb 12, Variant 1

class SinglePieceGoalState(GState):

    # finds the heuristic of the 2x2 to the spaces on the board
    def get_space_distance(self):
        # remaps the ref_point of the 2x2 to the center for the 
        # purposes of finding the distance between all the spaces
        goal_piece = self.piece_positions["gpc"][0]
        augmented_row    = goal_piece.ref_point/self.bw + (goal_piece.shape[0]/2.0 - 0.5)
        augmented_column = goal_piece.ref_point%self.bw + (goal_piece.shape[1]/2.0 - 0.5)
        s = 0
        for i in self.spaces:
            s += abs(augmented_row-(i/self.bw))      # distance between rows and space
            s += abs(augmented_column-(i%self.bw))   # distance between cols and space
        return s

    def get_h(self):
        # the heuristic of units between the goal_piece block and the spaces
        some_sum = self.get_space_distance()/(len(self.spaces)*(self.bw + self.bh))# confident h is consistent
        # the number of horizontal spaces between the goal_piece and the goal_state
        some_sum += abs(self.piece_positions["gpc"][0].ref_point%self.bw - goal_state["gpc"][0]%self.bw) 
        # the number of vertical spaces between the goal_piece and the goal_state
        some_sum += abs(self.piece_positions["gpc"][0].ref_point/self.bw - goal_state["gpc"][0]/self.bw)
        return some_sum # confident h is admissible (and consistent)

    def is_goal_state(self):
        #my_artist.draw_state(self.piece_positions)
        return self.piece_positions["gpc"][0].ref_point == goal_state["gpc"][0]


b1x1 = [([0],[0]),
        ([0],[0]),
        ([0],[0]),
        ([0],[0])]
b2x2 = [([0,1],[2,3]),
        ([1,3],[0,2]),
        ([2,3],[0,1]),
        ([0,2],[1,3])]
b2x1 = [([0],[1]),
        ([0,1],[0,1]),
        ([1],[0]),
        ([0,1],[0,1])]
b1x2 = [([0,1],[0,1]),
        ([1],[0]),
        ([0,1],[0,1]),
        ([0],[1])]
fwL = [([0,3],[2,3]),
       ([0,3],[0,2]),
       ([2,3],[0,3]),
       ([0,2],[0,3])]
ifL = [([0,1],[1,2]),
       ([1,2],[0,2]),
       ([1,2],[0,1]),
       ([0,2],[1,2])]
bwL = [([1,2],[2,3]),
       ([1,3],[1,2]),
       ([2,3],[1,2]),
       ([1,2],[1,3])]
ibL = [([0,1],[0,3]),
       ([1,3],[0,3]),
       ([0,3],[0,1]),
       ([0,3],[1,3])]
ivT = [([1,3,5],[3,4,5]),
       ([1,5],[1,3]),
       ([3,4,5],[1,3,5]),
       ([1,3],[1,5])]

v11_layout = {}
v11_layout["gpc"] = [Piece(13, b2x2, (2,2), pid=0)]
v11_layout["fwL"] = [Piece(4, fwL, (2,2), pid=1)]
v11_layout["1x1"] = [Piece(10, b1x1, (1,1), pid=2),
                     Piece(12, b1x1, (1,1), pid=3),
                     Piece(15, b1x1, (1,1), pid=4),
                     Piece(21, b1x1, (1,1), pid=5)]
v11_layout["2x1"] = [Piece(7, b2x1, (2,1), pid=6),
                     Piece(16, b2x1, (2,1), pid=7)]
v11_layout["bwL"] = [Piece(18, bwL, (2,2), pid=8)]

v12_layout = {}
v12_layout["gpc"] = [Piece(17, b2x2, (2,2), pid=0)]
v12_layout["ifL"] = [Piece(12, ifL, (2,2), pid=1)]
v12_layout["1x1"] = [Piece(9, b1x1, (1,1), pid=2),
                     Piece(14, b1x1, (1,1), pid=3),
                     Piece(20, b1x1, (1,1), pid=4),
                     Piece(23, b1x1, (1,1), pid=5)]
v12_layout["2x1"] = [Piece(4, b2x1, (2,1), pid=6),
                     Piece(15, b2x1, (2,1), pid=7)]
v12_layout["bwL"] = [Piece(6, bwL, (2,2), pid=8)]

climb12_layout = { # on a 6x5 board, with 0,1,3, and 4 blocked off in the first row
    "gpc": [Piece(21, ivT, (2,3), pid=0)],
    "ifL": [Piece(11, ifL, (2,2), pid=1)],
    "1x1": [Piece(15, b1x1, (1,1), pid=2),
            Piece(19, b1x1, (1,1), pid=3),
            Piece(25, b1x1, (1,1), pid=4),
            Piece(29, b1x1, (1,1), pid=5)],
    "2x1": [Piece(5, b2x1, (2,1), pid=6),
            Piece(9, b2x1, (2,1), pid=7)],
    "bwL": [Piece(12, bwL, (2,2), pid=8)],
    "1x2": [Piece(20, b1x2, (1,2), pid=9),
            Piece(23, b1x2, (1,2), pid=10)]
    }


# DON'T FORGET TO CHANGE goal_state WHEN CHANGING PROBLEM

bs = SinglePieceGoalState(v12_layout, space_positions=[1,2,5,6], board_width=4, board_height=6)
#bs = SinglePieceGoalState(climb12_layout, space_positions=[2,6,7,8], board_width=5, board_height=6)
#my_artist = artist(bs)
#my_artist.draw_state(bs)
astar_search(bs)
#cProfile.run('fringe_search(bs)')

