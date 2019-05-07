import Control.Concurrent
import System.IO

delay = 1000000
seconds = 80
message = "\rWe're back from school: "

formatTimer :: (Int, Int) -> String
formatTimer (x, y) = insertZeroIfNeeded(x)++":"++insertZeroIfNeeded(y)

insertZeroIfNeeded :: Int -> String
insertZeroIfNeeded x
    | elem x [0..9] = '0' : (show x)
    | otherwise = show x

minutesAndSeconds :: Int -> (Int, Int)
minutesAndSeconds x = (div x 60, mod x 60)

wrapIo :: String -> [IO()]
wrapIo x = [(threadDelay delay), (putStr (message++x)), (hFlush stdout)]

main = do
    sequence (concat (map (wrapIo.formatTimer.minutesAndSeconds) (reverse [0..seconds])))
    putStrLn "\nLet's have fun!"
