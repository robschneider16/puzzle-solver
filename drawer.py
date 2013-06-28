from graphics import *
from random import choice

shapes = {} #move to shape profiles file

shapes["fwL"] = [[0,0],[0,1],[1,1],]
shapes["1x1"] = [[0,0]]
shapes["2x1"] = [[0,0],[0,1]]
shapes["gpc"] = [[1,0],[0,1],[1,1],[2,1]]
shapes["bwL"] = [[1,0],[1,1],[0,1]]
shapes["ifL"] = [[0,0],[1,0],[0,1]]
shapes["1x2"] = [[0,0],[1,0]]

colors = ["white","black","red1","green1","blue1","purple1","orange1","yellow1","violet1","cyan1","red4","green4","blue4","purple4","orange4","yellow4","violet4","cyan4","red2","green2","blue2","purple2","orange2","yellow2","violet2","cyan2","red3","green3","blue3","purple3","orange3","yellow3","violet3","cyan3"]

class artist:
    def __init__(self, board_state, scale=100):
        self.win = GraphWin("Puzzle", board_state.bw*scale, board_state.bh*scale) # open window
        self.unit = scale
        self.w = board_state.bw
        self.h = board_state.bh

        # think about putting in number of moves ect.
    def new_window(self):
        self.win.close()
        self.win = GraphWin("Puzzle", self.w*self.unit, self.h*self.unit)

    
    def draw_specials(self):
        l = Rectangle(Point(0,0), Point(self.unit,2*self.unit))
        l.setFill("white")
        r = Rectangle(Point(0, 4*self.unit), Point(self.unit, 6*self.unit))
        r.setFill("white")
        r.draw(self.win)

    def draw_state(self, pp):
        self.win.flush()
        color_wheel = self.set_colors()
        for k, v in pp.iteritems():
            for p in v:
                self.draw_piece(k, p.ref_point, color_wheel.pop())
        self.win.getMouse()
        self.new_window()

        """        
    def draw_state(self, board_state): # (bw, bh, bsz, piece_positions, spaces, nmoves)
        for k,v in board_state.piece_positions.iteritems():
            for p in sorted(v, key=(lambda k: k.ref_point)): # might not have to sort...
                self.draw_piece(k, p.ref_point)
        self.win.getMouse()
        """
    def draw_piece(self, name, ref, cur_color):
        # find where the ref point is in 2D
        hor = ref%self.w
        vert = ref//(self.h-1)
        # draw a square that is one unit large at this ref point and around it
        for sqr in shapes[name]:
            self.a = Rectangle(Point(hor*self.unit+sqr[0]*self.unit, vert*self.unit+sqr[1]*self.unit), 
                          Point(hor*self.unit+self.unit+sqr[0]*self.unit, vert*self.unit+self.unit+sqr[1]*self.unit))
            self.a.setFill(cur_color)
            self.a.draw(self.win)

    def set_colors(self):
        return ["red1","green1","blue1","purple1","orange1","red4","green4","blue4","purple4","orange4","yellow","white","black","violet","cyan"]
            
    def done(self):
        self.win.close()

"""
my_drawer = artist(bs)
my_drawer.draw_state(bs)
my_drawer.done


win = GraphWin("Puzzle", 100, 200)
c = Circle(Point(50,50),10)
ac = Circle(Point(50,100),10)
c.draw(win)
ac.draw(win)
win.getMouse()
ac = Circle(Point(75,100),10)
ac.draw(win)
win.getMouse()
"""
