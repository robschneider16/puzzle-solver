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
#
bwl_tuples = [ ( BitVector(bitstring = '0110'), BitVector(bitstring = '0011') ),
                ( BitVector(bitstring = '0101'), BitVector(bitstring = '0110') ),
                ( BitVector(bitstring = '0011'), BitVector(bitstring = '0110') ),
                ( BitVector(bitstring = '0110'), BitVector(bitstring = '0101') )
                ]
#
#---
#| |
#---
#
l_tuples = [ ( BitVector(bitstring = '1001'), BitVector(bitstring = '0011') ), 
                ( BitVector(bitstring = '1001'), BitVector(bitstring = '1010') ),
                ( BitVector(bitstring = '0011'), BitVector(bitstring = '1001') ),
                ( BitVector(bitstring = '1010'), BitVector(bitstring = '1001') )
                ]
#
#---
#| |
#---
#
sqr_tuples = [ ( BitVector(bitstring = '1100'), BitVector(bitstring = '0011') ),
                ( BitVector(bitstring = '0101'), BitVector(bitstring = '1010') ),
                ( BitVector(bitstring = '0011'), BitVector(bitstring = '1100') ),
                ( BitVector(bitstring = '1010'), BitVector(bitstring = '0101') )
                ]
#
#---
#| |
#---
#
line_tuples = [ ( BitVector(bitstring = '100'), BitVector(bitstring = '001') ),
                ( BitVector(bitstring = '0101'), BitVector(bitstring = '1010') ),
                ( BitVector(bitstring = '001'), BitVector(bitstring = '100') ),
                ( BitVector(bitstring = '1010'), BitVector(bitstring = '0101') )
                ]


# declare the board shape
# NOTE: for ClimbPro boards the upper left and right hand corners should have immovable blocks
global board_width
board_width = 4
global board_height
board_height = 6
global board_size
board_size = board_height*board_width
global goal_state
goal_state = {"2x2":1}

class Board10State(GState):

    def get_heur(self):
        return (abs(self.piece_positions["2x2"][0].ref_point%board_width - goal_state["2x2"]%board_width) 
            + abs(self.piece_positions["2x2"][0].ref_point/board_width - goal_state["2x2"]/board_width))



layout = {}
layout["fl"] = [Piece(4, l_tuples, (2,2))]
layout["1x1"] = [ Piece(10, tile_tuples, (1,1)),
                Piece(12, tile_tuples, (1,1)),
                Piece(15, tile_tuples, (1,1)),
                Piece(21, tile_tuples, (1,1)) ]
layout["2x1"] = [Piece(7, line_tuples, (2,1)),
                Piece(16, line_tuples, (2,1)) ]
layout["2x2"] = [Piece(13, sqr_tuples, (2,2))]
layout["bwl"] = [Piece(18, bwl_tuples, (2,2))]
bs = Board10State(layout, space_positions=[1,2,5,6])
astar_search(bs)
#bs.move_up(bs.piece_positions["1x1"][0])
#bs.move_down(bs.piece_positions["1x1"][0])
#print bs.can_move_down(bs.piece_positions["1x1"][0])
#bs.move_down(bs.piece_positions["1x1"][0]) # FAIL
#bs.print_bs()
#bs.piece_positions[2] = Piece(1, (2,2))
