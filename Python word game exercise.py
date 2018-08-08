# The 6.00 Word Game

import random
import string

VOWELS = 'aeiou'
CONSONANTS = 'bcdfghjklmnpqrstvwxyz'
HAND_SIZE = 7

SCRABBLE_LETTER_VALUES = {
    'a': 1, 'b': 3, 'c': 3, 'd': 2, 'e': 1, 'f': 4, 'g': 2, 'h': 4, 'i': 1, 'j': 8, 'k': 5, 'l': 1,
    'm': 3, 'n': 1, 'o': 1, 'p': 3, 'q': 10, 'r': 1, 's': 1, 't': 1, 'u': 1, 'v': 4, 'w': 4, 'x': 8,
    'y': 4, 'z': 10
}

WORDLIST_FILENAME = "words.txt"

def loadWords():
    """
    Returns a list of words for the game.
    """
    print("Loading word list from file...")
    # inFile: file
    inFile = open(WORDLIST_FILENAME, 'r')
    # wordList: list of strings
    wordList = []
    for line in inFile:
        wordList.append(line.strip().lower())
    print("  ", len(wordList), "words loaded.")
    return wordList

def getFrequencyDict(sequence):
    """
    Returns dictionary with keys that are string elements,
    values are frequency of elements in a sentence.
    """
    # freqs: dictionary (element_type -> int)
    freq = {}
    for x in sequence:
        freq[x] = freq.get(x,0) + 1
    return freq

#
# Problem #1: Scoring a word
#
def getWordScore(word, n):
    """
    Returns word score as in Scrabble.

    word: string (lowercase letters)
    n: integer (HAND_SIZE; i.e., hand size required for additional points)
    returns: int >= 0
    """
    points = 0
    pointLetters = SCRABBLE_LETTER_VALUES.keys()
    for letter in word:
        if letter in pointLetters:
            points += SCRABBLE_LETTER_VALUES[letter]
    points = points*len(word)
    if len(word) == n:
        points += 50
    return points

#
# Problem #2: Make sure you understand how this function works and what it does!
#
def displayHand(hand):
    """
    Displays letters currently in hand.

    hand: dictionary (string -> int)
    """
    for letter in hand.keys():
        for j in range(hand[letter]):
             print(letter,end=" ")       # print all on the same line
    print()                             # print an empty line

#
# Problem #2: Make sure you understand how this function works and what it does!
#
def dealHand(n):
    """
    Deals a random hand of letters.

    n: int >= 0
    returns: dictionary (string -> int)
    """
    hand={}
    numVowels = n // 3

    for i in range(numVowels):
        x = VOWELS[random.randrange(0,len(VOWELS))]
        hand[x] = hand.get(x, 0) + 1

    for i in range(numVowels, n):
        x = CONSONANTS[random.randrange(0,len(CONSONANTS))]
        hand[x] = hand.get(x, 0) + 1

    return hand

#
# Problem #2: Update a hand by removing letters
#
def updateHand(hand, word):
    """
    Updates the hand to not have the letters played for a given word.

    word: string
    hand: dictionary (string -> int)
    returns: dictionary (string -> int)
    """
    wordL = list(word)
    hand2 = hand.copy()
    for letter in wordL:
        if letter in hand2:
            hand2[letter] -= 1
    return hand2

#
# Problem #3: Test word validity
#
def isValidWord(word, hand, wordList):
    """
    Returns true if word played is valid and the letters in the hand
    are sufficient to form it. False otherwise.

    word: string
    hand: dictionary (string -> int)
    wordList: list of lowercase strings
    """
    i = 0
    n = 0
    x = 0
    wordList2 = wordList[:]
    hand2 = hand.copy()
    word1 = list(word)
    wordL = word1[:]
    for letter in wordL:
        if letter in hand2:
            n += 1
            hand2[letter] -=1
    for letter in hand2:
        if hand2[letter] <= -1:
            n -= 1
    if len(wordL) == n:
        i += 1
    if word in wordList2:
        i += 1
    if i == 2:
        return True
    else:
        return False

#
# Problem #4: Playing a hand
#

def calculateHandlen(hand):
    """
    Returns the length (number of letters) in the current hand.

    hand: dictionary (string-> int)
    returns: integer
    """
    return sum(hand.values())



def playHand(hand, wordList, n):
    totalScore = 0
    output = "Run out of letters."
    while calculateHandlen(hand) > 0:
        displayHand(hand)
        word = input('Enter word, or a "." to indicate that you are finished: ').lower()
        if word == '.':
            output = "Goodbye!"
            break
        else:
            if not isValidWord(word, hand, wordList):
                print("Invalid word, please try again.")
            else:
                score = getWordScore(word, n)
                totalScore += score
                print('"{0:s}" earned {1:d} points. Total: {2:d} points.'.format(word, score, totalScore))
                hand = updateHand(hand, word)
    print('{0:s} Total score: {1:d} points.'.format(output, totalScore))


#
# Problem #5: Playing a game
#

def playGame(wordList):
    """
    Allows user to play a hand given input of 'e' - end, 'n' - new,
    or 'r' - replay
    """
    n = HAND_SIZE
    hand = None
    response = ''
    while response is not 'e':
        response = input('Enter n to deal a new hand, r to replay the last hand, or e to end game: ')
        if response is 'n':
            hand = dealHand(n)
            playHand(hand, wordList, n)
        elif response is 'r':
            if hand is not None:
                playHand(hand, wordList, n)
            else:
                print('You have not played a hand yet. Please play a new hand first!')
        elif response is 'e':
            break
        else:
            print('Invalid command.')




#
# Build data structures used for entire session and play game
#
if __name__ == '__main__':
    wordList = loadWords()
    playGame(wordList)

    from ps4a import *
    import time


    #
    #
    # Computer chooses a word
    #
    #
    def compChooseWord(hand, wordList, n):
        """
        Find word with maximum value score given a hand.

        hand: dictionary (string -> int)
        wordList: list (string)
        n: integer (HAND_SIZE; i.e., hand size required for additional points)

        returns: string or None
        """
        # Create a new variable to store the maximum score seen so far (initially 0)
        bestScore = 0
        # Create a new variable to store the best word seen so far (initially None)
        bestWord = None
        # For each word in the wordList
        for word in wordList:
            # If you can construct the word from your hand
            if isValidWord(word, hand, wordList):
                # find out how much making that word is worth
                score = getWordScore(word, n)
                # If the score for that word is higher than your best score
                if (score > bestScore):
                    # update your best score, and best word accordingly
                    bestScore = score
                    bestWord = word
        # return the best word you found.
        return bestWord

    #
    # Computer plays a hand
    #
    def compPlayHand(hand, wordList, n):
        """
        Allows the computer t o play a hand.

        hand: dictionary (string -> int)
        wordList: list (string)
        n: integer (HAND_SIZE; i.e., hand size required for additional points)
        """
        # Keep track of the total score
        totalScore = 0
        # As long as there are still letters left in the hand:
        while (calculateHandlen(hand) > 0) :
            # Display the hand
            print("Current Hand: ", end=' ')
            displayHand(hand)
            # computer's word
            word = compChooseWord(hand, wordList, n)
            # If the input is a single period:
            if word == None:
                # End the game (break out of the loop)
                break

            # Otherwise (the input is not a single period):
            else :
                # If the word is not valid:
                if (not isValidWord(word, hand, wordList)) :
                    print('This is a terrible error! I need to check my own code!')
                    break
                # Otherwise (the word is valid):
                else :
                    # Tell the user how many points the word earned, and the updated total score
                    score = getWordScore(word, n)
                    totalScore += score
                    print('"' + word + '" earned ' + str(score) + ' points. Total: ' + str(totalScore) + ' points')
                    # Update hand and show the updated hand to the user
                    hand = updateHand(hand, word)
                    print()
        # Game is over (user entered a '.' or ran out of letters), so tell user the total score
        print('Total score: ' + str(totalScore) + ' points.')


    #
    # Problem #6: Playing a game
    #
    #
    def playGame(wordList):
        """
        Allows the user to play a hand.

        wordList: list (string)
        """
        n = HAND_SIZE
        hand = None
        response = ''
        response2 = ''
        response3 = ''
        while response is not 'e':
            response = input('Enter n to deal a new hand, r to replay the last hand, or e to end game: ')
            if response is 'n':
                hand = dealHand(n)
                while response2 is 'u' or 'c':
                    response2 = input('Enter u to have yourself play, c to have the computer play: ')
                    if response2 is 'u':
                        playHand(hand, wordList, n)
                    elif response2 is 'c':
                        #hand = dealHand(n)
                        compPlayHand(hand, wordList, n)
                    elif response2 is 'e':
                        break
                    else:
                        print('Invalid command.')
            elif response is 'r':
                if hand is not None:
                    while response3 is 'u' or 'c':
                        response3 = input('Enter u to have yourself play, c to have the computer play: ')
                        if response3 is 'u':
                            playHand(hand, wordList, n)
                        elif response3 is 'c':
                            compPlayHand(hand, wordList, n)
                        elif response3 is 'e':
                            break
                        else:
                            print('Invalid command.')
                else:
                    print('You have not played a hand yet. Please play a new hand first!')
            elif response is 'e':
                break
            else:
                print('Invalid command.')



    #
    # Build data structures used for entire session and play game
    #
    if __name__ == '__main__':
        wordList = loadWords()
        playGame(wordList)
