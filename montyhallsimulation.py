from random import choice
from functools import reduce

class MontyHall:

    def __init__(self, door=None, verbose=False):
        if door is None:
            door = choice([1, 2, 3])
        self.door = door
        self.choice = None
        self.available_choices = set([1, 2, 3])
        self.verbose = verbose

    def choose_door(self, door_number=None):
        if door_number is None:
            door_number = choice([1, 2, 3])
        if self.verbose: 
            print(f"You chose door number {door_number}")
        self.choice = door_number
        return self

    def open_door(self):
        doors_with_goats = self.available_choices - set([self.door, self.choice])
        door_to_open = choice(list(doors_with_goats))
        self.available_choices = self.available_choices - set([door_to_open])
        if self.verbose:
            print(f'Door {door_to_open} has a goat!')
        return self

    def switch_choice(self):
        other_choice = self.available_choices - set([self.choice])
        self.choice = list(other_choice)[0]
        if self.verbose:
            print(f'You have now chosen door number {self.choice}')
        return self

    def keep_choice(self):
        if self.verbose:
            print(f'You chose to keep door number {self.choice}')
        return self

    def open_choice(self):
        if self.choice == self.door:
            if self.verbose:
                print('You have won a car!')
            return 1
        else:
            if self.verbose:
                print('You get a brand new goat!')
            return 0


def main():
    N = 100000
    switch_games = (MontyHall().choose_door().open_door().switch_choice().open_choice() for _ in range(N))
    keep_games = (MontyHall().choose_door().open_door().keep_choice().open_choice() for _ in range(N))
    wins_switch = reduce(lambda acc,x: acc + x, switch_games, 0)
    wins_keeps = reduce(lambda acc,x: acc + x, keep_games, 0)

    print(f"The switch strategy won {wins_switch} of {N} games ({wins_switch/N}%)")
    print(f"The keep strategy won {wins_keeps} of {N} games ({wins_keeps/N}%)")

if __name__ == '__main__':
    main()
