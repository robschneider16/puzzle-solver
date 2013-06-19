# Using BitVector-3.3
from BitVector import *
import copy

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

global goal_board
#goal_board = range(board_size)
goal_board= [3, 1, 0, 8, 2, 7, 5, 4, 6]

class GState:

    def __init__(self, positions=None, prior_moves=0):
        if not positions:
            layout = {}
            for i in range(1,board_size):
                layout[i] = Piece(i, tile_tuples, label=str(i))
            self.piece_positions = layout
        else:
            self.piece_positions = positions
        self.spaces = [0] # only one space for 8- and 15-puzzles
        self.nmoves = prior_moves

    def get_moves(self):
        return self.nmoves

    def get_heur(self):
        return self.manhat_sum()

    def get_board(self):
        board = range(board_size)
        for k,v in self.piece_positions.iteritems():
            board[v.ref_point] = k
        # This is not general beyond 8- and 15-puzzle at the moment
        board[self.spaces[0]] = 0
        return board

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

    def can_move_left(self, p1):
        return (p1.ref_point%board_width > 0 # if true, p1.ref_point-1 below will not wrap
                # Need to ensure the bitwise 'and' with spaces is same as precondition
                and 0 < int(self.make_bit_string(p1.shape, p1.move_tups[3][0], p1.ref_point-1) 
                            & self.make_space_bit_string(self.spaces)))

    def move_left(self, p1):
        self.swap(p1, -1, p1.move_tups[3])
        self.nmoves += 1

    def can_move_right(self, p1):
        return ((p1.ref_point%board_width < board_width-p1.ncols()) # not in danger of going off the board
                and 0 < int(self.make_bit_string(p1.shape, p1.move_tups[3][0], p1.ref_point+1)
                            & self.make_space_bit_string(self.spaces)))

    def move_right(self, p1):
        self.swap(p1, +1, p1.move_tups[3])
        self.nmoves += 1

    # Keep in mind that when moving up or down, we may be forced to add a variable
    # 'board_height' for boards such as ClimbPro in place of board_width in this context -Mason
    def can_move_up(self, p1):
        return (p1.ref_point >= board_width 
                and 0 < int(self.make_bit_string(p1.shape, p1.move_tups[3][0], p1.ref_point-board_width)
                            & self.make_space_bit_string(self.spaces)))

    def move_up(self, p1):
        self.swap(p1, -board_width, p1.move_tups[3])
        self.nmoves += 1

    def can_move_down(self, p1):
        return ((p1.ref_point) < board_size - (board_width * p1.nrows())
                and 0 < int(self.make_bit_string(p1.shape, p1.move_tups[3][0], p1.ref_point+board_width)
                            & self.make_space_bit_string(self.spaces)))

    def move_down(self, p1):
        self.swap(p1, +board_width, p1.move_tups[3])
        self.nmoves += 1

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
        postspaces = []
        for i in range(p.nrows()*p.ncols()):
            #print which_move_tups[0][i]
            if which_move_tups[0][i] == 1:
                prespaces.append(i)
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

    def available_moves(self):
        possible_moves = []
        for k, p in self.piece_positions.iteritems():
            if self.can_move_up(p):
                ns = copy.deepcopy(self)
                ns.move_up(p)
                possible_moves.append(ns)
            if self.can_move_right(p):
                ns = copy.deepcopy(self)
                ns.move_right(p)
                possible_moves.append(ns)
            if self.can_move_down(p):
                ns = copy.deepcopy(self)
                ns.move_down(p)
                possible_moves.append(ns)
            if self.can_move_left(p):
                ns = copy.deepcopy(self)
                ns.move_left(p)
                possible_moves.append(ns)
        return possible_moves

    def expand(self):
        return self.available_moves()

    def is_goal_state(self):
        return self.manhat_sum() == 0

    def manhat_sum(self):
        sum = 0
        for k in range(board_size):
            if goal_board[k] != 0:
                hor = abs(k%board_width - self.piece_positions[goal_board[k]].ref_point%board_width)
                vert = abs(k/board_width - self.piece_positions[goal_board[k]].ref_point/board_width)
                sum += hor + vert
        return sum

    # print the state of the test boards
    def printb(self):
        # *** THIS ONLY WORKS FOR 1x1 PUZZLES (e.g., 15-puzzle) WITH ONE SPACE
        a = range(board_size)
        for k,v in self.piece_positions.iteritems():
            #print str(k) + ' ' + str(v.ref_point)
            a[v.ref_point] = k
        #print self.spaces
        a[self.spaces[0]] = 0
        # NOW PRINT THE BOARD
        start = 0
        end = board_width
        print '-' * (board_size + 3)
        while end<=board_size:
            print '|',
            for x in a[start:end]:
                print '{0:{width}}'.format(x,width=board_width-1),
            if start == 0:
                print "|  Prior moves : " + str(self.nmoves)
            elif start == board_size:
                print "|  Estimate yet: " + str(self.get_heur())
            else:
                print "|"
            start += board_width
            end += board_width
        print '-' * (board_size + 3)



class Piece:
    #self.ref_point # a reference location for where this piece is located on the board
    #self.move_tups # a pointer to the move tuples for this type
    #self.shape # a tuple with the row and column dimensions

    def __init__(self, reference_point, move_tuples=None, block_shape=(1,1), label=None):
        self.ref_point = reference_point
        self.move_tups = move_tuples
        self.shape = block_shape

    # number of rows in a piece
    def nrows(self):
        return self.shape[0]

    # number of colomns in a piece
    def ncols(self):
        return self.shape[1]



#gs = GState()
#gs.piece_positions[2] = Piece(1, ['foo','foo','foo',(BitVector(bitstring='0110'), BitVector(bitstring='0101'))], (2,2))
# gs.spaces.extend([1,3])
""" Some tests may not pop any errors, HOWEVER I don't belive that they are 100 percent correct. - Mason"""


"""gs.printb()
gs.move_left(gs.piece_positions[1])
gs.printb()
gs.move_left(gs.piece_positions[2])
gs.printb()
gs.move_up(gs.piece_positions[5])
gs.printb()
gs.move_up(gs.piece_positions[8])
gs.printb()
#gs.move_down(gs.piece_positions[5]) # SHOULD FAIL
gs.move_right(gs.piece_positions[7])
gs.printb()
gs.move_down(gs.piece_positions[4])
gs.printb()"""


