class pot_solution:
    board = []
    def __init__(self, new_board, new_moves, heuristic):
        self.moves = new_moves
        self.heur = heuristic
        self.board = new_board

    def get_heur(self):
        return self.heur
    def set_heur(self, new_heur):
        self.heur = new_heur

    def get_moves(self):
        return self.moves
    def set_moves(self):
        self.moves = new_moves

    def inc(self):
        self.moves += 1

    def get_board(self):
        return self.board
    def set_board(self, new_board):
        self.board = new_board
