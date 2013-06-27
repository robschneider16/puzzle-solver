from gstate import *
from astar import *
from random import choice

global goal_state
goal_state = {"1":[1], "2":[2], "3":[3], "4":[4], "5":[5], "6":[6], "7":[7], "8":[8]}
#goal_state = {"1":[1], "2":[2], "3":[3], "4":[4], "5":[5], "6":[6], "7":[7], "8":[8], "9":[9], "10":[10], "11":[11], "12":[12], "13":[13], "14":[14], "15":[15]}

# for 8- and 15- type puzzles
# ASSUME only one block of each type
class NxNState(GState):

    def manhat_sum(self):
        """using the goal_board """
        sum = 0
        for k, p in goal_state.iteritems():
            hor = abs(self.piece_positions[k][0].ref_point%self.bw - p[0]%self.bw)
            vert = abs(self.piece_positions[k][0].ref_point/self.bw - p[0]/self.bw)
            sum += hor + vert
        return sum

    def print_bs(self):
        # *** THIS ONLY WORKS FOR 1x1 PUZZLES (e.g., 15-puzzle) WITH ONE SPACE
        a = range(self.bsz)
        for k,v in self.piece_positions.iteritems():
            #print str(k) + ' ' + str(v.ref_point)
            a[v[0].ref_point] = k
        #print self.spaces
        a[self.spaces[0]] = "0"
        # NOW PRINT THE BOARD
        start = 0
        end = self.bw
        heur_val = self.get_h()
        print '-' * (self.bsz + 3)
        while end<=self.bsz:
            print '|',
            for x in a[start:end]:
                print '{0:{width}}'.format(x,width=self.bw-1),
            if start == 0:
                print "|  Prior moves : " + str(self.nmoves)
            elif start == self.bw:
                print "|  Estimate yet: " + str(heur_val)
            else:
                print "|"
            start += self.bw
            end += self.bw
        print '-' * (self.bsz + 3)



def shuffle_board(start, shuffle_times):
    i = 0
    shuffled_state = start
    while i < shuffle_times:
        moves = shuffled_state.all_available_moves()
        shuffled_state = choice(moves)
        i += 1
    shuffled_state.nmoves = 0
    return shuffled_state

bs = NxNState()
#bs = NxNState(board_width=4, board_height=4)
#sbs = shuffle_board(bs,50)
bs.print_bs()
astar_search(bs)
