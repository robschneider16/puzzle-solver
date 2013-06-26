from BitVector import *
from gstate import *
from astar import *

global tile_tuples # still for 1x1 blocks/tiles
#
#---
#| |
#---
#
tile_tuples = [ ( BitVector(bitstring = '1'), BitVector(bitstring = '1') ),
                ( BitVector(bitstring = '1'), BitVector(bitstring = '1') ),
                ( BitVector(bitstring = '1'), BitVector(bitstring = '1') ),
                ( BitVector(bitstring = '1'), BitVector(bitstring = '1') )
                ]
#
#---
#| |
#---
# backward-'L'
bwl_tuples = [ ( BitVector(bitstring = '0110'), BitVector(bitstring = '0011') ),
               ( BitVector(bitstring = '0101'), BitVector(bitstring = '0110') ),
               ( BitVector(bitstring = '0011'), BitVector(bitstring = '0110') ),
               ( BitVector(bitstring = '0110'), BitVector(bitstring = '0101') )
               ]
#
#---
#| |
#---
# forward-'L'
l_tuples = [ ( BitVector(bitstring = '1001'), BitVector(bitstring = '0011') ), 
             ( BitVector(bitstring = '1001'), BitVector(bitstring = '1010') ),
             ( BitVector(bitstring = '0011'), BitVector(bitstring = '1001') ),
             ( BitVector(bitstring = '1010'), BitVector(bitstring = '1001') )
             ]

# inverted-(forward)-'L'
ifl_tuples = [ ( BitVector(bitstring = '1100'), BitVector(bitstring = '0110') ), 
                ( BitVector(bitstring = '0110'), BitVector(bitstring = '1010') ),
                ( BitVector(bitstring = '0110'), BitVector(bitstring = '1100') ),
                ( BitVector(bitstring = '1010'), BitVector(bitstring = '0110') )
                ]
#
#---
#| |
#---
# 2x2 square block
sqr_tuples = [ ( BitVector(bitstring = '1100'), BitVector(bitstring = '0011') ),
                ( BitVector(bitstring = '0101'), BitVector(bitstring = '1010') ),
                ( BitVector(bitstring = '0011'), BitVector(bitstring = '1100') ),
                ( BitVector(bitstring = '1010'), BitVector(bitstring = '0101') )
                ]
#
#---
#| |
#---
# vertical 2x1 block
line_tuples = [ ( BitVector(bitstring = '10'), BitVector(bitstring = '01') ),
                ( BitVector(bitstring = '11'), BitVector(bitstring = '11') ),
                ( BitVector(bitstring = '01'), BitVector(bitstring = '10') ),
                ( BitVector(bitstring = '11'), BitVector(bitstring = '11') )
                ]


# declare the goal_state of the board
global goal_state
goal_state = {"2x2":1}

class Board10State(GState):

    # finds the heuristic of the 2x2 to the spaces on the board
    def get_space_distance(self):
        # remaps the ref_point of the 2x2 to the center for the 
        # purposes of finding the distance between all the spaces
        augmented_row    = self.piece_positions["2x2"][0].ref_point/self.bw + .5
        augmented_column = self.piece_positions["2x2"][0].ref_point%self.bw + .5
        s = 0
        for i in self.spaces:
            s += abs(augmented_row-(i/self.bw))      # distance between rows and space
            s += abs(augmented_column-(i%self.bw))   # distance between cols and space
        return s

    def get_h(self):
        return (
            # the number of horizontal spaces between the 2x2 and the goal_state
            abs(self.piece_positions["2x2"][0].ref_point%self.bw - goal_state["2x2"]%self.bw) 
            # the number of vertical spaces between the 2x2 and the goal_state
            + abs(self.piece_positions["2x2"][0].ref_point/self.bw - goal_state["2x2"]/self.bw)
            # the heuristic of units between the 2x2 block and the space
            + (self.get_space_distance()/13.0)
            )

    def is_goal_state(self):
        return self.piece_positions["2x2"][0].ref_point == goal_state["2x2"]


v11_layout = {}
v11_layout["fwL"] = [Piece(4, l_tuples, (2,2))]
v11_layout["1x1"] = [Piece(10, tile_tuples, (1,1)),
                 Piece(12, tile_tuples, (1,1)),
                 Piece(15, tile_tuples, (1,1)),
                 Piece(21, tile_tuples, (1,1)) ]
v11_layout["2x1"] = [Piece(7, line_tuples, (2,1)),
                Piece(16, line_tuples, (2,1)) ]
v11_layout["2x2"] = [Piece(13, sqr_tuples, (2,2))]
v11_layout["bwL"] = [Piece(18, bwl_tuples, (2,2))]

v12_layout = {}
v12_layout["ifL"] = [Piece(12, ifl_tuples, (2,2))]
v12_layout["1x1"] = [Piece(9, tile_tuples, (1,1)),
                 Piece(14, tile_tuples, (1,1)),
                 Piece(20, tile_tuples, (1,1)),
                 Piece(23, tile_tuples, (1,1)) ]
v12_layout["2x1"] = [Piece(4, line_tuples, (2,1)),
                Piece(15, line_tuples, (2,1)) ]
v12_layout["2x2"] = [Piece(17, sqr_tuples, (2,2))]
v12_layout["bwL"] = [Piece(6, bwl_tuples, (2,2))]

bs = Board10State(v12_layout, space_positions=[1,2,5,6], board_width=4, board_height=6)
astar_search(bs)
#bs.move_up(bs.piece_positions["1x1"][0])
#bs.move_down(bs.piece_positions["1x1"][0])
#print bs.can_move_down(bs.piece_positions["1x1"][0])
#bs.move_down(bs.piece_positions["1x1"][0]) # FAIL
#bs.print_bs()
#bs.piece_positions[2] = Piece(1, (2,2))
