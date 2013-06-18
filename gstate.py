# Using BitVector-3.3
from BitVector import *

# Static information for piece-types and their movement properties
# for the 8- and 15-puzzles, each block/tile is distinct because of its number
# thus we have one spaces (1x1) and 15 1x1 block-types

# each tuple (for up, right, down and left, respectively) has a precondition for the space's location
# relative to the direction of the intended move (i.e., where the space must be if this block moves)
# and a post condition for where the space ends up (given the CURRENT ref_point of the block prior to move)
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
            # Need to ensure the bitwise 'and' with spaces is same as precondition
            and 0 < int(self.make_bit_string(p1.shape, p1.move_tups[3][0], p1.ref_point-1) 
                & self.make_space_bit_string(self.spaces))):
            self.swap(p1, -1, p1.move_tups[3])
        else:
            raise Exception('Invalid move left')

    def move_right(self, p1):
        if ((p1.ref_point+p1.ncols())%board_width > 0 # not in danger of going off the board
            and 0 < int(self.make_bit_string(p1.shape, p1.move_tups[3][0], p1.ref_point+1)
                & self.make_space_bit_string(self.spaces))):
            self.swap(p1, +1, p1.move_tups[3])
        else:
            raise Exception('Invalid move right')

    # Keep in mind that when moving up or down, we may be forced to add a variable
    # 'board_hieght' for boards such as ClimbPro in place of board_width in this context -Mason
    def move_up(self, p1):
        if (p1.ref_point%board_width > 0
            and 0 < int(self.make_bit_string(p1.shape, p1.move_tups[3][0], p1.ref_point-board_width)
                & self.make_space_bit_string(self.spaces))):
            self.swap(p1, -board_width, p1.move_tups[3])
        else:
            raise Exception('Invalid move up')

    def move_down(self, p1):
        if ((p1.ref_point)%board_width > 0
            and 0 < int(self.make_bit_string(p1.shape, p1.move_tups[3][0], p1.ref_point+board_width)
                & self.make_space_bit_string(self.spaces))):
            self.swap(p1, +board_width, p1.move_tups[3])
        else:
            raise Exception('Invalid move down')

    # s is the space
    # p is a piece
    # d is either -1 for left, +1 for right, -board_width for up, and +board_width for down
    def base_to_ref(self, s, d, p):
        # returns a number of the desired space's location
        return (s/p.nrows() * board_width) + s%p.ncols() + d + p.ref_point

    # p is a Piece; delta is either -1 for left, +1 for right, -board_width for up,
    # and +board_width for down; which_move_tups is the bitmask tuple for the appropriae move direction
    def swap(self, p, delta, which_move_tups):
        """Swap all the spaces from spots to be occupied as given in the precondition BitVector
        to the corresponding spots in the postcondition BitVector"""

        # list of bits
        prespaces = []
        for i in range(p.nrows()*p.ncols()):
            #print which_move_tups[0][i]
            if which_move_tups[0][i] == 1:
                prespaces.append(i)

        postspaces = []
        for i in range(p.nrows()*p.ncols()):
            if which_move_tups[1][i] == 1:
                postspaces.append(i)

        space_tuples = zip(prespaces, postspaces)
        
        # Need to do this for each part of the piece mask
        for space_tuple in space_tuples:
            prelocation = self.base_to_ref(space_tuple[0], delta, p)
            postlocation = self.base_to_ref(space_tuple[1], 0, p)
            space_index = self.spaces.index(prelocation)
            self.spaces[space_index] = postlocation

        p.ref_point += delta


class Piece:
    #self.ref_point # a reference location for where this piece is located on the board
    #self.move_tups # a pointer to the move tuples for this type
    #self.shape # a tuple with the row and column dimensions

    def __init__(self, reference_point, move_tuples=None, block_shape=(1,1)):
        self.ref_point = reference_point
        self.move_tups = move_tuples
        self.shape = block_shape

    # number of rows in a piece
    def nrows(self):
        return self.shape[0]

    # number of colomns in a piece
    def ncols(self):
        return self.shape[1]


# print the state of the test boards
def printb(board):
    for k,v in board.piece_positions.iteritems():
        print str(k) + ' ' + str(v.ref_point)
    print board.spaces
    print "\n"

gs = GState()
#gs.piece_positions[2] = Piece(1, ['foo','foo','foo',(BitVector(bitstring='0110'), BitVector(bitstring='0101'))], (2,2))
# gs.spaces.extend([1,3])
""" Some tests may not pop any errors, HOWEVER I don't belive that they are 100 percent correct. - Mason"""

printb(gs)
gs.move_left(gs.piece_positions[1])
printb(gs)
gs.move_left(gs.piece_positions[2])
printb(gs)
gs.move_up(gs.piece_positions[5])
printb(gs)
gs.move_up(gs.piece_positions[8])
printb(gs)
#gs.move_down(gs.piece_positions[5]) # SHOULD FAIL
gs.move_down(gs.piece_positions[8])
gs.move_down(gs.piece_positions[5])
printb(gs)


