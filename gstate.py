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

    def __init__(self, positions=None):
        if not positions:
            layout = {}
            for i in range(1,board_size):
                layout[i] = Piece(i, tile_tuples)
            self.piece_positions = layout
        else:
            self.piece_positions = positions
        self.spaces = [0] # only one space for 8- and 15-puzzles

    def make_bit_string(self, piece_shape, mask, ref_point, increment=0):
        # SHOULD CHECK TO MAKE SURE piece IS ON BOARD AND THROW ERROR IF NOT
        bv = BitVector(size = board_size) # make full board bv
        # use piece ref_point and tuple to overlay the mask from the piece
        for i in range(piece_shape[0]):
            leftslice = ref_point+(i*board_width) # left edge placement of the Piece mask 
            bv[leftslice:leftslice+piece_shape[1]] = mask[i:i+piece_shape[1]]
        return bv

    # Only spaces move, thus spaces are not represented as Pieces
    def make_space_bit_string(self, space_locs):
        bv = BitVector(size = board_size)
        for space in space_locs:
            bv[space] = 1
        return bv

    # NOW NEED TO ACTUALLY MOVE BLOCK AND SPACE
    def move_left(self, p1):
        # If valid move
        if (p1.ref_point%board_width > 0 # if true, p1.ref_point-1 below will not wrap
            and 0 < int(self.make_bit_string(p1.shape, p1.move_tups[3][0], p1.ref_point-1)
                        & self.make_space_bit_string(self.spaces))):
            self.swap(p1, -1, p1.move_tups[3])
        else:
            raise Exception('Invalid move')

    def swap(self, p, delta, which_move_tups):
        """Swap all the spaces from spots to be occupied as given in the precondition BitVector
         to the corresponding spots in the postcondition BitVector"""
        # Need to do this for each part of the piece mask
        for space_target_base in range(p.shape[0]*p.shape[1]):
            if which_move_tups[1][space_target_base] == 1: # need to move the appropriate space
                
                i = self.base_to_ref(p.ref_point, delta, space_target_base)
                space_index = self.spaces.index(i)
                self.spaces[space_index] = 

                space_index = self.spaces.index(p.ref_point+delta)
                self.spaces[space_index] = p.ref_point
                p.ref_point = p.ref_point+delta


class Piece:
    #self.ref_point # a reference location for where this piece is located on the board
    #self.move_tups # a pointer to the move tuples for this type
    #self.shape # a tuple with the row and column dimensions

    def __init__(self, reference_point, move_tuples=None, block_shape=(1,1)):
        self.ref_point = reference_point
        self.move_tups = move_tuples
        self.shape = block_shape


gs = GState()
gs.move_left(gs.piece_positions[1])
gs.move_left(gs.piece_positions[2])
for k,v in gs.piece_positions.iteritems():
    print str(k) + ' ' + str(v.ref_point)
print gs.spaces
gs.move_left(gs.piece_positions[3])
