import pygtk
import gtk.gdk
import wnck
import time
from collections import defaultdict
from math import sqrt, pow
import subprocess
from Xlib import X, display, ext

#Stores the average colour of a square and what this square means.
#Chosen by a fair dice ro^W^W^W recognising on a genuine gnomine grid.
colours = {\
(169.02777777777777, 169.02777777777777, 169.02777777777777): '0',\
(155.39222222222222, 155.39222222222222, 174.74666666666667): '1',\
(147.64666666666668, 167.12888888888889, 147.64666666666668): '2',\
(181.93411386593203, 150.99678604224059, 150.99678604224059): '3',\
(148.47666666666666, 148.47666666666666, 163.60111111111112): '4',\
(167.44444444444446, 146.58777777777777, 146.58777777777777): '5',\
(205.46000000000001, 205.46000000000001, 205.46000000000001): '.',\
(163.78086419753086, 111.08024691358024, 110.1003086419753): 'F'}

#Coordinates of the actual grid, relative to the gnomine window
topSide = 0
bottomSide = 0
leftSide = 0
rightSide = 0

#Real screen coordinates of the client area of gnomine
windowX = 0
windowY = 0

#Side of a square
squareSide = 0

#Number of squares in the grid
squaresHor = 0
squaresVer = 0

hpath = "dist/build/minesweeper-sweeper/minesweeper-sweeper"

#Looks up the colour in the dictionary that is the closest to the given,
#but it must be closer than tolerance.
def lookupColour (colour, tolerance):
    #empty dictionaries are automatically a no-no
    if not colours:
        return None
    
    #get the colour with the minimum distance
    closest = min (colours.iteritems(), 
                  key = lambda x: getColourDistance (x[0], colour))
    
    #if the colour isn't close enough, don't return anything.
    if getColourDistance (closest[0], colour) > tolerance:
        return None
    else:
        return closest[1]

#Gets the type of the cell at x, y by either looking it up in the dictionary or,
#if it is not recognised, asking the user and adding it to the dictionary.
def getSquareMeaning (bitmap, x, y):
    squareColour = getSquareColour (bitmap, x, y)
    
    meaning = lookupColour (squareColour, 15)
    
    if meaning == None:
        print "Cannot recognize the cell at " + str (x) + ", " + str(y) + \
              " (0-based). What is it (.012345678F)?"
        meaning = raw_input()
        colours[squareColour] = meaning
        
    return meaning

#Gets the average colour of all pixels in the given area on the bitmap
def getAverageColour (x1, y1, x2, y2, bitmap):
    totalR = 0
    totalG = 0
    totalB = 0
    
    for x in range (x1, x2):
        for y in range (y1, y2):
            pix = bitmap[y][x]
            totalR += pix[0]
            totalG += pix[1]
            totalB += pix[2]
    
    factor = 1.0 / ((x2-x1) * (y2-y1))
    
    totalR *= factor
    totalG *= factor
    totalB *= factor
    
    return (totalR, totalG, totalB)

#Gets the average colour of a square on the grid
def getSquareColour (bitmap, x, y):
    return getAverageColour (leftSide + x*squareSide,
                             topSide + y*squareSide,
                             leftSide + (x+1)*squareSide,
                             topSide + (y+1)*squareSide,
                             bitmap)

#Gets the Cartesian distance between colours (a measure of how close the colours
#are together)
def getColourDistance (c1, c2):
    return sqrt (pow((c2[0]-c1[0]), 2) 
               + pow((c2[1]-c1[1]), 2)
               + pow((c2[2]-c1[2]), 2))

#Takes a screenshot of the client area of the Mines window, sets the relevant
#global variables
def getScreenshot():
    screen = wnck.screen_get_default()
    while gtk.events_pending(): gtk.main_iteration()
    windowList = screen.get_windows()
    mineWindow = filter ((lambda w: w.get_name() == "Mines"), windowList)

    if len (mineWindow) == 0:
        print "Couldn't find the Mines window."
        exit()
        
    mineWindow[0].move_to_workspace(screen.get_active_workspace())
    mineWindow[0].activate(0)
    mineSize = mineWindow[0].get_client_window_geometry()
    #46 pixels from the top is the toolbar
    mineSize = (mineSize[0], mineSize[1] + 46, mineSize[2], mineSize[3] - 46)

    time.sleep(0.5)

    #Screenshot the window (46 pixels from the top is the toolbar)
    desktop = gtk.gdk.get_default_root_window()
    desktopSize = desktop.get_size()
    pb = gtk.gdk.Pixbuf(
        gtk.gdk.COLORSPACE_RGB,False,8,mineSize[2], mineSize[3])
    pb = pb.get_from_drawable(desktop, desktop.get_colormap(),
        mineSize[0], mineSize[1], 0,0, mineSize[2], mineSize[3])
    if (pb == None):
        print "Unable to get the screenshot."

    bitmap = pb.get_pixels_array()
    windowX = mineSize[0]
    windowY = mineSize[1]
    
    return (bitmap, windowX, windowY)
    
#Recognizes the left, right, top and bottom sides of the grid
def recognizeDimensions(bitmap):
    backgroundColor = bitmap[0][0]

    topSideDict = defaultdict(int)
    for i in range(bitmap.shape[1]):
        for j in range(bitmap.shape[0]):
            if not ((bitmap[j][i] == backgroundColor).all()):
                topSideDict[j] += 1
                break

    bottomSideDict = defaultdict(int)
    for i in range(bitmap.shape[1]):
        for j in reversed(range(bitmap.shape[0])):
            if not ((bitmap[j][i] == backgroundColor).all()):
                bottomSideDict[j] += 1
                break
                
    leftSideDict = defaultdict(int)
    for i in range(bitmap.shape[0]):
        for j in range(bitmap.shape[1]):
            if not ((bitmap[i][j] == backgroundColor).all()):
                leftSideDict[j] += 1
                break

    rightSideDict = defaultdict(int)
    for i in range(bitmap.shape[0]):
        for j in reversed(range(bitmap.shape[1])):
            if not ((bitmap[i][j] == backgroundColor).all()):
                rightSideDict[j] += 1
                break

    topSide = max(topSideDict.iteritems(), key=lambda x: x[1])[0]
    bottomSide = max(bottomSideDict.iteritems(), key=lambda x: x[1])[0]
    leftSide = max(leftSideDict.iteritems(), key=lambda x: x[1])[0]
    rightSide = max(rightSideDict.iteritems(), key=lambda x: x[1])[0]
    
    squareSide = (rightSide-leftSide+1)/squaresHor
    squaresVer = (bottomSide-topSide+1)/squareSide
    
    return (topSide, bottomSide, leftSide, rightSide, squareSide, squaresVer)


def convertToRealCoords (x, y):
    return (windowX + leftSide + int((x+0.5) * squareSide),
            windowY + topSide + int((y+0.5) * squareSide))

#Takes a screenshot, recognises the cells on it and formats them into a
#string suitable for the Haskell program
def doOneRecognition():
    global windowX
    global windowY
    
    (bitmap, windowX, windowY) = getScreenshot()
    
    result = str(squaresVer) + "\n" + str(squaresHor) + "\n"
    for y in range(squaresVer):
        for x in range(squaresHor):
            result += getSquareMeaning (bitmap, x, y)
    
    result += '\n'
    return result

#Executes a program, feeds inputString into its stdin and returns the output
def execProgram(inputString, path):
    p = subprocess.Popen(path, stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    p.stdin.write(inputString)
    p.stdin.flush()
    return p.stdout.read()

#Marks the relevant cells in the gnomine window as per the Haskell program
#output
def markCells(outputString):
    for command in outputString.split('\n'):
        splitcmd = command.split()
        operator = splitcmd[0]
        coords = splitcmd[1][1:-1].split(',')
        row = int(coords[0])
        col = int(coords[1])
        clickAtCell (col, row, (operator == "Safe"))
    
#Moves the mouse somewhere (real coordinates) and clicks there.
def clickAt (x, y, leftClick = True):
    if leftClick:
        button = 1
    else:
        button = 3
    
    d = display.Display()
    s = d.screen()
    root = s.root
    root.warp_pointer(x,y)
    d.sync()
    
    ext.xtest.fake_input(d, X.ButtonPress, button)
    ext.xtest.fake_input(d, X.ButtonRelease, button)
    d.sync()

def clickAtCell (x, y, leftClick = True):
    print "clicking at cell " + str (x) + ", " + str(y)
    (realx, realy) = convertToRealCoords (x, y)
    print "real = " + str(realx) + ", " + str(realy)
    clickAt (realx, realy, leftClick)
    
#Assumes there already is one bitmap
def loopAll():
    while True:
        inStr = doOneRecognition()
        outStr = execProgram(inStr, hpath)
        if outStr == "":
            break
        markCells (outStr)
    
    
print "Performing the initial recognition..."
(bitmap, windowX, windowY) = getScreenshot()
print "How many cells are there, horizontally?"
squaresHor = int(raw_input())

print "Recognizing the dimensions of the grid..."
(topSide, bottomSide, leftSide, rightSide, squareSide, squaresVer) = recognizeDimensions(bitmap)
print "Recognized. Don't you dare to resize the window anymore!"

print "Opening a starting position..."
clickAtCell (int(squaresHor/2), int(squaresVer/2))

print "Let's do it!"

loopAll()
