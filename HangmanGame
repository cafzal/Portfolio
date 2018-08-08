# Load the list of words into the variable wordlist
# so that it can be accessed from anywhere in the program
wordlist = loadWords()

def isWordGuessed(secretWord, lettersGuessed):
    '''
    secretWord: string, the word the user is guessing
    lettersGuessed: list, what letters have been guessed so far
    returns: boolean, True if all the letters of secretWord are in lettersGuessed;
      False otherwise
    '''
    L = list(secretWord)
    n = 0
    for var in lettersGuessed:
        if var in L:
            n += 1
        else:
            n += 0
    if n == len(secretWord):
        return True
    else:
        return False

def getGuessedWord(secretWord, lettersGuessed):
    '''
    secretWord: string, the word the user is guessing
    lettersGuessed: list, what letters have been guessed so far
    returns: string, comprised of letters and underscores that represents
      what letters in secretWord have been guessed so far.
    '''
    guess = ''
    for i in secretWord:
        if i in lettersGuessed:
            guess = guess + i
        else:
            guess = guess + '_ '
    return guess


def getAvailableLetters(lettersGuessed):
    '''
    lettersGuessed: list, what letters have been guessed so far
    returns: string, comprised of letters that represents what letters have not
      yet been guessed.
    '''
    import string
    av = string.ascii_lowercase
    av = list(av)
    #av = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
    #'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
    #'s', 't', 'u', 'v', 'w', 'x', 'y', 'z']
    lettersGuessed.sort()
    L = ''.join(lettersGuessed)
    for letter in L:
        if letter in lettersGuessed:
            av.remove(letter)
    av = ''.join(av)
    return av


def hangman(secretWord):
    '''
    secretWord: string, the secret word to guess.
    '''
    lettersGuessed = []
    print('Welcome to the game, Hangman!')
    print('I am thinking of a word that is ' + str(len(secretWord)) + ' letters long')
    num = 8
    correct = 0
    while num > 0 and isWordGuessed(secretWord, lettersGuessed) is False:
        print('------------')
        print('You have ' + str(num) + ' guesses left.')
        print('Available letters: ' + getAvailableLetters(lettersGuessed))
        letter_guess = input('Please guess a letter: ')
        if letter_guess in lettersGuessed:
            print("Oops! You've already guessed that letter: " + getGuessedWord(secretWord, lettersGuessed))
        elif letter_guess in secretWord:
            lettersGuessed += letter_guess
            correct += 1
            print('Good guess: ' + getGuessedWord(secretWord, lettersGuessed))
        else:
            lettersGuessed += letter_guess
            num -= 1
            print('Oops! That letter is not in my word: ' + getGuessedWord(secretWord, lettersGuessed))
    if isWordGuessed(secretWord, lettersGuessed) is True:
        print('------------')
        print('Congratulations, you won!')
    else:
        print('------------')
        print('Sorry, you ran out of guesses. The word was ' + str(secretWord))
