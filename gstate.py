# Using BitVector-3.3
from BitVector import *


# Static information for piece-types and their movement properties
# for the 8- and 15-puzzles, each block/tile is distinct because of its number
# thus we have one spaces (1x1) and 15 1x1 block-types

# each tuple (for up, right, down and left, respectively) has a precondition for the space's location
# and a post condition for where the space ends up (given the current ref_point
global tile_tuples = [ ( BitVector(bitstring = '1'), BitVector(bitstring = '1') ),
                       ( BitVector(bitstring = '1'), BitVector(bitstring = '1') ),
                       ( BitVector(bitstring = '1'), BitVector(bitstring = '1') ),
                       ( BitVector(bitstring = '1'), BitVector(bitstring = '1') ) ]

# A generic-state (gstate) supports the representation of puzzle boards
# having pieces of arbitrary shape.

global eight_puzzle_goal
eight_puzzle_goal = map(lambda b: [b], range(9))

global board_size
board_size = 9

class GState:
    # need an array of distinct elements:
    self.piece_positions = [ 'punt'
                             Piece(1, tile_tuples),
                             Piece(2, tile_tuples),
                             Piece(3, tile_tuples),
                             Piece(4, tile_tuples),
                             Piece(5, tile_tuples),
                             Piece(6, tile_tuples),
                             Piece(7, tile_tuples),
                             Piece(8, tile_tuples) ]

    def __init__(self):
        pass

    # *** THIS NEEDS EXTENSION TO USE TUPLES
    def make_bit_string(self, index):
        bv = BitVector(size=board_size)
        bv[index] = 1
        return bv

    def move_left(self, p1):
        if p1.ref_point > 0 and and(make_bit_string(p1.ref_point-1),
                                    make_bit_string(space_ref)):
            print 'Valid move'
        else:
            print 'Invalid move'
    


class Piece:
    # a reference location for where this piece is located on the board
    self.ref_point
    # a pointer to the move tuples for this type
    self.move_tups

    def __init__(self, reference_point, move_tuples):
        self.ref_point = reference_point
        self.move_tups = move_tuples


    
        

