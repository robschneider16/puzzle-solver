


# Static information for piece-types and their movement properties
# for the 8- and 15-puzzles, each block/tile is distinct because of its number
# thus we have one spaces (1x1) and 15 1x1 block-types

# each tuple (for up, right, down and left, respectively) has a precondition for the space's location
# and a post condition for where the space ends up (given the current ref_point
global tile_tuples = [ ( BitVector(bitsring = '1'), BitVector(bitstring = '1') ),
                       ( BitVector(bitsring = '1'), BitVector(bitstring = '1') ),
                       ( BitVector(bitsring = '1'), BitVector(bitstring = '1') ),
                       ( BitVector(bitsring = '1'), BitVector(bitstring = '1') ) ]

# A generic-state (gstate) supports the representation of puzzle boards
# having pieces of arbitrary shape.

global eight_puzzle_goal
eight_puzzle_goal = map(lambda b: [b], range(9))

#class gstate:
    # need an array of distinct elements:



class piece:
    # a reference location for where this piece is located on the board
    ref_point
    # a pointer to the move tuples for this type
    move_tups

    def __init__(self, reference_point, move_tuples):
        self.ref_point = reference_point
        self.move_tups = move_tuples

    def move_up(self, mt=self.move_tups[0]):
        # check the precondition up one from self.ref_point
        # if valid, move this tile up and the space down
        pass

    def move_right(self, mt=self.move_tups[1]):
        pass

    def move_down(self, mt=self.move_tups[2]):
        pass

    def move_left(self, mt=self.move_tups[3]):
        pass

    
        

