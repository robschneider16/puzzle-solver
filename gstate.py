# Using BitVector-3.3
#from BitVector import *
import copy

b1x1 = [([0],[0]),
        ([0],[0]),
        ([0],[0]),
        ([0],[0])]

# each tuple (for up, right, down and left, respectively) has a precondition for the space's location
# relative to the direction of the intended move (i.e., where the space must be if this block moves)
# and a post condition for where the space ends up (given the CURRENT ref_point of the block prior to move)
#global tile_tuples # for 1x1 blocks/tiles

# A generic-state (gstate) supports the representation of puzzle boards
# having pieces of arbitrary shape and the movement of those pieces.
class GState:

    piece_types = []

    def __init__(self, positions=None, prior_moves=0, space_positions=[0],
                 board_width=3, board_height=3,
                 previous_state=None):
        #last_moved_piece = 
        self.bw = board_width
        self.bh = board_height
        self.bsz = board_width*board_height
        if not positions:
            layout = {}
            # default: N-by-N puzzle
            for i in range(1,self.bsz):
                layout[str(i)] = [Piece(i, b1x1)]
            self.piece_positions = layout
        else:
            self.piece_positions = positions
        self.spaces = space_positions
        self.nmoves = prior_moves
        self.dir_deltas = [-board_width, 1, board_width, -1] # appropriate delta for each move directions starting with up and clockwise
        self.prev_state = previous_state # for extracting sequence of moves for the solution
        GState.piece_types = sorted(self.piece_positions.keys()) # fixed and forever 

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
        for k in GState.piece_types:
            v = self.piece_positions[k]
            board += "[" + k + ":"
            board += str(sorted(map(lambda p: p.ref_point,v))) + "]"
        board += "[spaces:" + str(sorted(self.spaces)) + "]"
        return board

    def can_move(self, p1, mov_tup_index, delta):
        """
        Determine if the given piece, p1, can move in the direction given by mov_tup_index,
        with 0 meaning 'up' and working clockwise as with the move tuples.
        """
        moved_rp = p1.ref_point + delta
        if ((moved_rp >= 0) and (moved_rp < self.bsz) and
            ( # moved_rp must have either the same column or same row as where started
                (p1.ref_point/self.bw == moved_rp/self.bw) or (p1.ref_point%self.bw == moved_rp%self.bw))):
            for s in map(lambda i: self.base_to_ref(i, delta, p1), p1.move_tups[mov_tup_index][0]):
                if s not in self.spaces:
                    return False
            return True
        else:
            return False

    def move(self, p1, move_tup_index):
        self.swap(p1, self.dir_deltas[move_tup_index], p1.move_tups[move_tup_index])
        #self.nmoves += 1


    # s is the space
    # p is a piece
    # d is either -1 for left, +1 for right, -board_width for up, and +board_width for down
    def base_to_ref(self, s, d, p):
        # returns a number of the desired space's location
        return (s/p.ncols() * self.bw) + s%p.ncols() + d + p.ref_point

    # p is a Piece; delta is either -1 for left, +1 for right, -board_width for up,
    # and +board_width for down; which_move_tups is the bitmask tuple for the appropriae move direction
    def swap(self, p, delta, which_move_tups):
        for space_tuple in zip(which_move_tups[0],which_move_tups[1]):
            preloc = self.base_to_ref(space_tuple[0], delta, p)
            postloc = self.base_to_ref(space_tuple[1], 0, p)
            space_index = self.spaces.index(preloc)
            self.spaces[space_index] = postloc
        p.ref_point += delta

    # only copy the part of the state relevant to the piece being moved and the spaces
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
        for i in range(4):
            if self.can_move(p,i,self.dir_deltas[i]):
                ns = self.custom_copy(k,p)
                ns.move(ns.piece_positions[k][self.piece_positions[k].index(p)], i)
                part_possible_moves.append(ns)
        return part_possible_moves

    def all_available_moves(self):
        possible_moves = {self.get_board():self} # key state.get_board() and value state
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
            #m.prev_state = self # if want the actual solution
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
    piece_id_counter = 0
    #self.ref_point # a reference location for where this piece is located on the board
    #self.move_tups # a pointer to the move tuples for this type
    #self.shape # a tuple with the row and column dimensions

    def __init__(self, reference_point, move_tuples=None, block_shape=(1,1), label=None, pid=None):
        self.ref_point = reference_point
        self.move_tups = move_tuples
        self.shape = block_shape
        if pid:
            self.id = pid
        else:
            self.id = Piece.piece_id_counter
            Piece.piece_id_counter += 1

    # number of rows in a piece
    def nrows(self):
        return self.shape[0]

    # number of colomns in a piece
    def ncols(self):
        return self.shape[1]


