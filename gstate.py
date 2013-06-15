# Using BitVector-3.3
from BitVector import *

# Static information for piece-types and their movement properties
# for the 8- and 15-puzzles, each block/tile is distinct because of its number
# thus we have one spaces (1x1) and 15 1x1 block-types

# each tuple (for up, right, down and left, respectively) has a precondition for the space's location
# and a post condition for where the space ends up (given the current ref_point
global tile_tuples
tile_tuples = [ ( BitVector(bitstring = '1'), BitVector(bitstring = '1') ),
                ( BitVector(bitstring = '1'), BitVector(bitstring = '1') ),
                ( BitVector(bitstring = '1'), BitVector(bitstring = '1') ),
                ( BitVector(bitstring = '1'), BitVector(bitstring = '1') ) ]

# A generic-state (gstate) supports the representation of puzzle boards
# having pieces of arbitrary shape.

global board_size
board_size = 9
global board_width
board_width = 3

class GState:

    def __init__(self):
        # need an array of distinct elements:
        self.piece_positions = [ Piece(0, tile_tuples),
                                 Piece(1, tile_tuples),
                                 Piece(2, tile_tuples),
                                 Piece(3, tile_tuples),
                                 Piece(4, tile_tuples),
                                 Piece(5, tile_tuples),
                                 Piece(6, tile_tuples),
                                 Piece(7, tile_tuples),
                                 Piece(8, tile_tuples) ]
        self.spaces = [0] # only one space for 8- and 15-puzzles

    # *** THIS NEEDS EXTENSION TO USE TUPLES
    def make_bit_string(self, piece_shape, mask, ref_point, increment=0):
        # SHOULD CHECK TO MAKE SURE piece IS ON BOARD
        bv = BitVector(size=board_size)
        # use piece ref_point and tuple to construct bit vector
        for i in range(piece_shape[0]):
            leftslice = ref_point+(i*board_width)
            bv[leftslice:leftslice+piece_shape[1]] = mask[i:i+piece_shape[1]]
        return bv

    def make_space_bit_string(self, space_locs):
        bv = BitVector(size=board_size)
        for space in space_locs:
            bv[space] = 1

    def move_left(self, p1):
        if p1.ref_point > 0 and (self.make_bit_string(p1.shape, p1.move_tups[3][0], p1.ref_point-1)
                                 & self.make_space_bit_string(self.spaces)):
            print 'Valid move'
        else:
            print 'Invalid move'


class Piece:
    # a reference location for where this piece is located on the board
    #self.ref_point
    # a pointer to the move tuples for this type
    #self.move_tups
    # a tuple with the row and column dimensions
    #self.shape

    def __init__(self, reference_point, move_tuples=None, block_shape=(1,1)):
        self.ref_point = reference_point
        self.move_tups = move_tuples
        self.shape = block_shape


gs = GState()
gs.move_left(gs.piece_positions[1])
gs.move_left(gs.piece_positions[2])
gs.move_left(gs.piece_positions[3])
