# Using BitVector-3.3
from BitVector import *
import copy


# each tuple (for up, right, down and left, respectively) has a precondition for the space's location
# relative to the direction of the intended move (i.e., where the space must be if this block moves)
# and a post condition for where the space ends up (given the CURRENT ref_point of the block prior to move)
global tile_tuples # for 1x1 blocks/tiles
tile_tuples = [ ( BitVector(bitstring = '1'), BitVector(bitstring = '1') ),
                ( BitVector(bitstring = '1'), BitVector(bitstring = '1') ),
                ( BitVector(bitstring = '1'), BitVector(bitstring = '1') ),
                ( BitVector(bitstring = '1'), BitVector(bitstring = '1') ) ]


# A generic-state (gstate) supports the representation of puzzle boards
# having pieces of arbitrary shape and the movement of those pieces.
class GState:

    def __init__(self, positions=None, prior_moves=0, space_positions=[0],
                 board_width=3, board_height=3):
        #last_moved_piece = 
        self.bw = board_width
        self.bh = board_height
        self.bsz = board_width*board_height
        if not positions:
            layout = {}
            # default: N-by-N puzzle
            for i in range(1,self.bsz):
                layout[str(i)] = [Piece(i, tile_tuples)]
            self.piece_positions = layout
        else:
            self.piece_positions = positions
        self.spaces = space_positions
        self.nmoves = prior_moves

    def get_g(self):
        return self.nmoves

    def get_h(self):
        return self.manhat_sum()

    def get_f(self):
        return self.get_g() + self.get_h()

    def get_board(self):
        """return a string representation of the board that is appropriate for a search algorithm
        to index states by board on closed and open lists"""
        board = ""
        for k,v in self.piece_positions.iteritems():
            board += "[" + k + ":"
            for p in sorted(v, key=(lambda k: k.ref_point)):
                board += " " + str(p.ref_point) 
            board += "]"
        board += "[spaces:"
        for space in sorted(self.spaces):
            board += " " + str(space)
        board += "]"
        return board

    def make_bit_string(self, piece_shape, mask, ref_point, increment=0):
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
        p.ref_point += delta

    def custom_copy(self, piece_type, piece):
        new_state = copy.copy(self)
        new_piece = copy.copy(piece)
        new_positions = copy.copy(self.piece_positions)
        new_positions[piece_type] = copy.copy(self.piece_positions[piece_type])
        new_positions[piece_type][new_positions[piece_type].index(piece)] = new_piece
        new_state.piece_positions = new_positions
        new_state.spaces = copy.copy(self.spaces)
        return new_state

    def one_piece_one_step_moves(self,k,p):
        part_possible_moves = []
        #print "all_available_moves: processing piece at ref_point: " + str(p.ref_point)
        if self.can_move_up(p):
            #print "can move up " + k + ":" + str(p.ref_point) + " in board:"
            #self.print_bs()
            ns = self.custom_copy(k,p)
            ns.move_up(ns.piece_positions[k][self.piece_positions[k].index(p)])
            part_possible_moves.append(ns)
        if self.can_move_right(p):
            #print "can move right " + k + ":" + str(p.ref_point) + " in board:"
            #self.print_bs()
            ns = self.custom_copy(k,p)
            ns.move_right(ns.piece_positions[k][self.piece_positions[k].index(p)])
            part_possible_moves.append(ns)
        if self.can_move_down(p):
            #print "can move down " + k + ":" + str(p.ref_point) + " in board:"
            #self.print_bs()
            ns = self.custom_copy(k,p)
            ns.move_down(ns.piece_positions[k][self.piece_positions[k].index(p)])
            part_possible_moves.append(ns)
        if self.can_move_left(p):
            #print "can move left " + k + ":" + str(p.ref_point) + " in board:"
            #self.print_bs()
            ns = self.custom_copy(k,p)
            ns.move_left(ns.piece_positions[k][self.piece_positions[k].index(p)])
            part_possible_moves.append(ns)
        return part_possible_moves

    def all_available_moves(self):
        possible_moves = {} # key state.get_board() and value state
        # for all the piece types in the puzzle ...
        for k, v in self.piece_positions.iteritems():
            #print "all_available_moves: processing piece type: " + k
            # for each of the pieces of a particular type ...
            for p in v:
                new_moves = self.one_piece_one_step_moves(k,p)
                filtered_moves = [m for m in new_moves if m.get_board() not in possible_moves]
                while  filtered_moves != []:
                    next_bunch = []
                    for m in filtered_moves:
                        # add remainder to possible_moves
                        possible_moves[m.get_board()] = m
                        # map filtered_new_moves over one_piece_one_step_moves
                        next_bunch.extend(m.one_piece_one_step_moves(k,m.piece_positions[k][self.piece_positions[k].index(p)]))
                    # filter against possible
                    filtered_moves = [m for m in next_bunch if m.get_board() not in possible_moves]
        # don't forget to make sure all possible moves have the right number of prior moves
        just_moves = []
        for k, m in possible_moves.iteritems():
            m.nmoves = self.nmoves + 1
            just_moves.append(m)
        return just_moves

    def expand(self):
        return self.all_available_moves()

    def is_goal_state(self):
        return self.get_h() == 0


    # print the state of the test boards
    def print_bs(self):
        print self.get_board()
        print "moves: " + str(self.nmoves) + ", h-estimate: " + str(self.get_h()) + "\n"


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


