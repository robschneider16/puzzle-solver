from gstate import *

class NGState(GState):

    def can_move(self, p1, mov_tup_index, delta):
        for s in map(lambda i: self.base_to_ref(i, delta, p1), p1.move_tups[mov_tup_index][0]):
            if s not in self.spaces:
                return False
        #print "can_move: piece " + str(p1.id) + ", in dir " + str(delta)
        return True

    def move(self, p1, move_tup_index):
        self.swap(p1, self.dir_deltas[move_tup_index], p1.move_tups[move_tup_index])
        self.nmoves += 1

    def swap(self, p, delta, which_move_tups):
        for space_tuple in zip(which_move_tups[0],which_move_tups[1]):
            preloc = self.base_to_ref(space_tuple[0], delta, p)
            postloc = self.base_to_ref(space_tuple[1], 0, p)
            space_index = self.spaces.index(preloc)
            self.spaces[space_index] = postloc
        p.ref_point += delta

    def one_piece_one_step_moves(self,k,p):
        part_possible_moves = []
        for i in range(4):
            if self.can_move(p,i,self.dir_deltas[i]):
                ns = self.custom_copy(k,p)
                ns.move(ns.piece_positions[k][self.piece_positions[k].index(p)], i)
                part_possible_moves.append(ns)
        return part_possible_moves

    
