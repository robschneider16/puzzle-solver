from gstate import *
from astar import *

global goal_state
# for the 8 puzzle
# 1 2 5
# 3 0 4
# 6 7 8
goal_state = {"0":4, "1":0, "2":1, "3":3, "4":5, "5":2, "6":6, "7":7, "8":8}


# for 8- and 15- type puzzles
# ASSUME only one block of each type
class NxNState(GState):

    def manhat_sum(self):
        """using the goal_board """
        sum = 0
        for k, p in goal_state.iteritems():
            if k != "0":
                hor = abs(self.piece_positions[k][0].ref_point%self.bw - p%self.bw)
                vert = abs(self.piece_positions[k][0].ref_point/self.bw - p/self.bw)
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
        heur_val = self.get_heur()
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


bs = NxNState()
astar_search(bs)
