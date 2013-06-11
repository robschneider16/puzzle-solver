from random import choice
import ftpuzzle
class pot_solution:
    board = []
    def __init__(self, new_board, new_moves, heuristic):
        self.moves = new_moves
        self.heur = heuristic
        self.board = new_board

    def shuffle_board(self, bs, num_moves):
        game = ftpuzzle.ftpuzzle(bs)
        i = 0
        while i<=num_moves:
            moves = game.available_moves(self.board)
            random_move = choice(moves)
            self.board = game.swap(random_move, self.board)
            i += 1
        print "after making " + str(i) + " moves your random board is: "
        self.heur = 1
        self.moves = 0
        game.print_b(self.board)
                

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
