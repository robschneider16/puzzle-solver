# Using BitVector-3.3
from BitVector import *
import copy

# Static information for piece-types and their movement properties
# for the 8- and 15-puzzles, each block/tile is distinct because of its number
# thus we have one spaces (1x1) and 15 1x1 block-types

# each tuple (for up, right, down and left, respectively) has a precondition for the space's location
# relative to the direction of the intended move (i.e., where the space must be if this block moves)
# and a post condition for where the space ends up (given the CURRENT ref_point of the block prior to move)
global tile_tuples # for 1x1 blocks/tiles
tile_tuples = [ ( BitVector(bitstring = '1'), BitVector(bitstring = '1') ),
                ( BitVector(bitstring = '1'), BitVector(bitstring = '1') ),
                ( BitVector(bitstring = '1'), BitVector(bitstring = '1') ),
                ( BitVector(bitstring = '1'), BitVector(bitstring = '1') ) ]

# A generic-state (gstate) supports the representation of puzzle boards
# having pieces of arbitrary shape.


class GState:

    def __init__(self, positions=None, prior_moves=0, space_positions=[0],
                 board_width=3, board_height=3):
        #last_moved_piece = 
        self.bw = board_width
        self.bh = board_height
        self.bsz = board_width*board_height
        if not positions:
            layout = {}
            for i in range(1,self.bsz):
                layout[str(i)] = [Piece(i, tile_tuples)]
            self.piece_positions = layout
        else:
            self.piece_positions = positions
        self.spaces = space_positions
        self.nmoves = prior_moves

    def get_moves(self):
        return self.nmoves

    def get_heur(self):
        return self.manhat_sum()

    def get_board(self):
        """return a representation of the board that is appropriate for a search methode
        to compare nodes on closed and open lists"""
        # **** we use this result to check if a board(state) is "in" the closed list
        board = {}
        for k,v in self.piece_positions.iteritems():
            board[k] = []
            #print "in first loop"
            for p in sorted(v, key=(lambda k: k.ref_point)):
                board[k].append(p.ref_point)
                #print "in second loop"
            #print "out of the SECOND loop!!!!"
            #board[k] = sorted(board[k])
        #print "out of the FIRST loop!!!!"
        board["spaces"] = sorted(self.spaces)
        return board

    def make_bit_string(self, piece_shape, mask, ref_point, increment=0):
        # SHOULD CHECK TO MAKE SURE piece IS ON BOARD AND THROW ERROR IF NOT
        bv = BitVector(size = self.bsz) # make full board bv
        # use piece ref_point and tuple to overlay the mask from the piece
        for i in range(piece_shape[0]):
            leftslice = ref_point+(i*self.bw) # left edge placement of the Piece mask
            bv[leftslice:leftslice+piece_shape[1]] = mask[i*piece_shape[1]:(i*piece_shape[1])+piece_shape[1]]
        #print "make_bit_string: for pc at " + str(ref_point) + " with bw=" + str(self.bw) + ", resulting bv = " + str(bv)
        return bv

    # Only spaces move, thus spaces are not represented as Pieces
    def make_space_bit_string(self):
        bv = BitVector(size = self.bsz)
        for space in self.spaces:
            bv[space] = 1
        return bv

    def can_move_left(self, p1):
        return (p1.ref_point%self.bw > 0 # if true, p1.ref_point-1 below will not wrap
                and (self.make_bit_string(p1.shape, p1.move_tups[3][0], p1.ref_point-1)
                     == (self.make_bit_string(p1.shape, p1.move_tups[3][0], p1.ref_point-1)
                         & self.make_space_bit_string())))

    def move_left(self, p1):
        self.swap(p1, -1, p1.move_tups[3])
        self.nmoves += 1

    def can_move_right(self, p1):
        return ((p1.ref_point%self.bw < self.bw-p1.ncols()) # not in danger of going off the board
                and (self.make_bit_string(p1.shape, p1.move_tups[1][0], p1.ref_point+1)
                     == (self.make_bit_string(p1.shape, p1.move_tups[1][0], p1.ref_point+1)
                         & self.make_space_bit_string())))

    def move_right(self, p1):
        self.swap(p1, +1, p1.move_tups[1])
        self.nmoves += 1

    # Keep in mind that when moving up or down, we may be forced to add a variable
    # 'board_height' for boards such as ClimbPro in place of board_width in this context -Mason
    def can_move_up(self, p1):
        return (p1.ref_point >= self.bw
                and (self.make_bit_string(p1.shape, p1.move_tups[0][0], p1.ref_point-self.bw)
                     == (self.make_bit_string(p1.shape, p1.move_tups[0][0], p1.ref_point-self.bw)
                         & self.make_space_bit_string())))

    def move_up(self, p1):
        self.swap(p1, -self.bw, p1.move_tups[0])
        self.nmoves += 1

    def can_move_down(self, p1):
        return ((p1.ref_point) < self.bsz - (self.bw * p1.nrows())
                and (self.make_bit_string(p1.shape, p1.move_tups[2][0], p1.ref_point+self.bw)
                     == (self.make_bit_string(p1.shape, p1.move_tups[2][0], p1.ref_point+self.bw)
                         & self.make_space_bit_string())))

    def move_down(self, p1):
        self.swap(p1, +self.bw, p1.move_tups[2])
        self.nmoves += 1

    # s is the space
    # p is a piece
    # d is either -1 for left, +1 for right, -board_width for up, and +board_width for down
    def base_to_ref(self, s, d, p):
        # returns a number of the desired space's location
        return (s/p.ncols() * self.bw) + s%p.ncols() + d + p.ref_point

    # p is a Piece; delta is either -1 for left, +1 for right, -board_width for up,
    # and +board_width for down; which_move_tups is the bitmask tuple for the appropriae move direction
    def swap(self, p, delta, which_move_tups):
        """Swap all the spaces from spots to be occupied as given in the precondition BitVector
        to the corresponding spots in the postcondition BitVector"""
        # list of bits
        prespaces = []
        postspaces = []
        for i in range(p.nrows()*p.ncols()):
            if which_move_tups[0][i] == 1:
                prespaces.append(i)
            if which_move_tups[1][i] == 1:
                postspaces.append(i)
        space_tuples = zip(prespaces, postspaces)
        #print "swap space_tuples: " + str(space_tuples)
        # Need to do this for each part of the piece mask
        for space_tuple in space_tuples:
            prelocation = self.base_to_ref(space_tuple[0], delta, p)
            postlocation = self.base_to_ref(space_tuple[1], 0, p)
            #print "swap: preloc = " + str(prelocation) + ", postloc = " + str(postlocation)
            space_index = self.spaces.index(prelocation)
            self.spaces[space_index] = postlocation
        self.spaces = sorted(self.spaces)  # to support canonical representations
        p.ref_point += delta

    def custom_copy(self, piece_type, piece):
        new_state = copy.copy(self)
        new_piece = copy.copy(piece)
        new_positions = copy.copy(self.piece_positions)
        new_positions[piece_type] = copy.copy(self.piece_positions[piece_type])
        new_positions[piece_type][new_positions[piece_type].index(piece)] = new_piece
        new_state.piece_positions = new_positions
        new_spaces = copy.copy(self.spaces)
        new_state.spaces = new_spaces
        return (new_state, new_piece)

    # how to pass a method name to a funtion and have the function apply that name onto an object
    def all_available_moves(self):
        possible_moves = []
        # for all the piece types in the puzzle ...
        for k, v in self.piece_positions.iteritems():
            #print "all_available_moves: processing piece type: " + k
            # for each of the pieces of a particular type ...
            for p in v:
                #print "all_available_moves: processing piece at ref_point: " + str(p.ref_point)
                # for any valid move, we will also need to repeat the check on the _moved_ piece
                # in order to gather up the possibility of sliding a piece more than 1 space and even up-and-over
                if self.can_move_up(p):
                    #print "can move up " + k + ":" + str(p.ref_point) + " in board:"
                    #self.print_bs()
                    ns = copy.deepcopy(self)
                    ns.move_up(ns.piece_positions[k][self.piece_positions[k].index(p)])
                    possible_moves.append(ns)
                if self.can_move_right(p):
                    #print "can move right " + k + ":" + str(p.ref_point) + " in board:"
                    #self.print_bs()
                    ns = copy.deepcopy(self)
                    ns.move_right(ns.piece_positions[k][self.piece_positions[k].index(p)])
                    possible_moves.append(ns)
                if self.can_move_down(p):
                    #print "can move down " + k + ":" + str(p.ref_point) + " in board:"
                    #self.print_bs()
                    ns = copy.deepcopy(self)
                    ns.move_down(ns.piece_positions[k][self.piece_positions[k].index(p)])
                    possible_moves.append(ns)
                if self.can_move_left(p):
                    #print "can move left " + k + ":" + str(p.ref_point) + " in board:"
                    #self.print_bs()
                    ns = copy.deepcopy(self)
                    ns.move_left(ns.piece_positions[k][self.piece_positions[k].index(p)])
                    possible_moves.append(ns)
        return possible_moves

    def expand(self):
        return self.all_available_moves()

    def is_goal_state(self):
        return self.get_heur() == 0

    def manhat_sum(self):
        sum = 0
        """for k, v in goal_state.iteritems():
            for p in v:
                if goal_board[k] != 0:
                    hor = abs(k%self.bw - self.piece_positions[goal_board[k]].ref_point%self.bw)
                    vert = abs(k/self.bw - self.piece_positions[goal_board[k]].ref_point/self.bw)
                    sum += hor + vert"""
        return sum

    # print the state of the test boards
    def print_bs(self):
        print self.get_board()
        print "moves: " + str(self.nmoves) + ", h-estimate: " + str(self.get_heur()) + "\n"


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
# For backward 'L' block:
#  ___
#  | |
#--  |
#|   |
#-----
#gs.piece_positions[2] = Piece(1, ['foo',(BitVector(bitstring='0101'), BitVector(bitstring='0110')),'foo',(BitVector(bitstring='0110'), BitVector(bitstring='0101'))], (2,2))
# gs.spaces.extend([1,3])
""" Some tests may not pop any errors, HOWEVER I don't belive that they are 100 percent correct. - Mason"""


"""gs.print_bs()
gs.move_left(gs.piece_positions[1])
gs.print_bs()
gs.move_left(gs.piece_positions[2])
gs.print_bs()
gs.move_up(gs.piece_positions[5])
gs.print_bs()
gs.move_up(gs.piece_positions[8])
gs.print_bs()
#gs.move_down(gs.piece_positions[5]) # SHOULD FAIL
gs.move_right(gs.piece_positions[7])
gs.print_bs()
gs.move_down(gs.piece_positions[4])
gs.print_bs()"""


