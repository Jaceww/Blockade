module Main where
import UI.NCurses
import Control.Concurrent (threadDelay)
import Control.Monad.State.Lazy (liftIO)
import Control.Monad
import Data.List (union)

type Xp = Integer
type Yp = Integer

data Direction = GoUp | GoDown | GoLeft | GoRight deriving (Eq, Show, Read)
data Block = Block {currentPos :: (Xp, Yp), posHistory :: [(Xp, Yp)], dir :: Direction} deriving (Eq, Show, Read)

main :: IO ()
main = runCurses $ do
    setEcho False
    w <- defaultWindow
    updateWindow w $ do
        moveCursor 10 30
        drawString "Welcome To Blockade!"
        moveCursor 12 30
        drawString "Press r to start"
        moveCursor 13 30
        drawString "Press q to quit"
    render
    let block = [Block (4,4) [] GoDown, Block (20,60) [] GoUp]
    --Going in to the game loop
    gameLoop w block False
    -- If q is pressed during game you get back to here
    --End

gameLoop :: Window -> [Block] -> Bool -> Curses ()
gameLoop w b start = loop b start where
    loop b start = do
        color2 <- newColorID ColorGreen ColorBlack 2
        blocks <- updateWindow w $ do
            winSize <- windowSize
            --Check if it's a valid position and if the game have started
            if start && coliderCheck b winSize then do
                 sequence [drawBlock (b !! 0) color2,  drawBlock (b !! 1) color2]
            else if start == False then return b
            --Game Over
            else return[] <$> do
                    clear
                    setColor defaultColorID
                    moveCursor 10 30
                    drawString "Game Over!"
                    
        render
        ev <- getEvent w $ Just(0)

        --Delay for rendering the screen
        liftIO $ threadDelay 100000

        -- If Game over, then restart with inital values 
        if ((blocks == []) && (start == True)) then do
            -- Delay for displaying game over a couple of sec
            liftIO $ threadDelay 1000000
            blocks <- updateWindow w $ do
                clear
                setColor defaultColorID
                moveCursor 10 30
                drawString "Welcome To Blockade!"
                moveCursor 12 30
                drawString "Press r to start"
                moveCursor 13 30
                drawString "Press q to quit"
            render
            loop [Block (4,4) [] GoDown, Block (20,60) [] GoUp] False

        else case ev of
            -- game loop
            Nothing -> loop blocks start

            -- Quit
            Just (EventCharacter 'q') -> return ()

            -- Start the game and Prints the Boarder
            Just (EventCharacter 'r') -> 
                do 
                    color3 <- newColorID ColorBlack ColorGreen 1
                    if start == False then updateWindow w $do 
                        clear
                        setColor color3
                        drawBorder  (Just glyphStipple)
                                    (Just glyphStipple)
                                    (Just glyphStipple)
                                    (Just glyphStipple)
                                    (Just glyphStipple)
                                    (Just glyphStipple)
                                    (Just glyphStipple)
                                    (Just glyphStipple)
                    else return ()
                    loop blocks True 

            -- Controls player 1
            Just (EventCharacter 'w') -> 
                loop [(blocks !! 0) {dir = GoUp}, blocks !! 1] start
            Just (EventCharacter 's') -> 
                loop [(blocks !! 0) {dir = GoDown}, blocks !! 1] start
            Just (EventCharacter 'a') -> 
                loop [(blocks !! 0) {dir = GoLeft}, blocks !! 1] start
            Just (EventCharacter 'd') -> 
                loop [(blocks !! 0) {dir = GoRight}, blocks !! 1] start

            -- Controls Player 2
            Just (EventSpecialKey KeyUpArrow) -> 
                loop [blocks !! 0, (blocks !! 1) {dir = GoUp}] start
            Just (EventSpecialKey KeyDownArrow) -> 
                loop [blocks !! 0, (blocks !! 1) {dir = GoDown}] start
            Just (EventSpecialKey KeyLeftArrow) -> 
                loop [blocks !! 0, (blocks !! 1) {dir = GoLeft}] start
            Just (EventSpecialKey KeyRightArrow) -> 
                loop [blocks !! 0, (blocks !! 1) {dir = GoRight}] start  
    

drawBlock :: Block -> ColorID -> Update Block
drawBlock Block {currentPos = c, posHistory = h, dir = d} color =
    do
        moveCursor (fst c) (snd c)
        setColor color
        drawGlyph glyphStipple
        moveCursor (fst nextPos) (snd nextPos)
        drawGlyph arrow
        return $ Block nextPos (c:h) d
    where 
        arrow
            | (d == GoUp) = glyphArrowU
            | (d == GoDown) = glyphArrowD
            | (d == GoLeft) = glyphArrowL
            | (d == GoRight) = glyphArrowR
        nextPos
            | (d == GoUp) = addDir c (-1,0)
            | (d == GoDown) = addDir c (1,0)
            | (d == GoLeft) = addDir c (0,-1)
            | (d == GoRight) = addDir c (0,1)

--Helper function to drawBlock, adds togheter two positions
addDir :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
addDir t1 t2 = (fst t1 + fst t2, snd t1 + snd t2) 

coliderCheck :: [Block] -> (Integer, Integer) -> Bool
coliderCheck block winSize
    | elem p1Pos inHistory || elem p2Pos inHistory = False
    | p1PosX == 0 || p1PosY == 0 || p2PosX == 0 || p2PosY == 0 = False
    | p1PosX == winX  || p1PosY == winY || p2PosX == winX|| p2PosY == winY = False
    | otherwise = True
    where 
        p1Pos = currentPos(block !! 0)
        p2Pos = currentPos(block !! 1)
        inHistory = union (posHistory(block !! 0)) (posHistory(block !! 1))
        p1PosX = fst p1Pos 
        p1PosY = snd p1Pos
        p2PosX = fst p2Pos
        p2PosY = snd p2Pos
        winX = fst winSize -1
        winY = snd winSize -1


    
    