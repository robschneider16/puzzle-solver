from BitVector import *
import gstate

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
                ( BitVector(bitstring = '1010'), BitVector(bitstring = '`1001') )
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
                ( BitVector(bitstring = '010101'), BitVector(bitstring = '101010') ),
                ( BitVector(bitstring = '001'), BitVector(bitstring = '100') ),
                ( BitVector(bitstring = '101010'), BitVector(bitstring = '010101') )
                ]


# declare the board shape
# NOTE: for ClimbPro boards the upper left and right hand corners should have immovable blocks
global board_width
board_width = 4
global board_size
board_size = board_height*board_width

class Board10State(GState):

	def __init__(self):
        if not positions:
            layout = {}
            for i in range(1,board_size):
                layout[i] = Piece(i, tile_tuples, label=str(i))
            self.piece_positions = layout
        else:
            self.piece_positions = positions
        self.spaces = [4] # for board10
        self.nmoves = prior_moves
	
