import System.Process (createProcess, waitForProcess, StdStream(CreatePipe), std_out, std_in, std_err, proc)
import System.IO (Handle, hFlush, stdout, hPutStr, hGetChar, hClose, openTempFile, hReady)
import Data.List (elemIndex, intercalate)
import Data.Maybe (fromMaybe)
import Control.Monad (foldM, foldM_)
import System.Directory(getTemporaryDirectory, removeFile)
import System.Environment(lookupEnv)
import Control.Monad.IO.Class (liftIO)
import System.Console.Haskeline

data ReplState = ReplState { prompts :: [String],
                             inputs :: [String],
                             outputs :: [String] }
                             deriving (Show, Eq)

data GHCIProc = GHCIProc { inp :: Handle,
                           out :: Handle,
                           err :: Handle }
                           deriving (Show, Eq)

cursorAtSpot x y = "\x1b[" ++ show ( y + 1 ) ++ ";" ++ show (x + 1) ++ "H"
newScreen = putStr "\x1b[2J" >> hFlush stdout

fullRenderAtRow :: Int -> ReplState -> IO ()
fullRenderAtRow row replState = putStr (cursorAtSpot 0 row) >>
    hFlush stdout >> newScreen >> fullRender replState

-- Renders a full REPL session; it better fit on one screen.
-- EXCEPT for the last prompt, which will be rendered by getInputLine
fullRender :: ReplState -> IO ()
fullRender (ReplState prompts inputs outputs) =
    recursiveRender (mergeThree (reverse (tail prompts)) (reverse inputs) (reverse outputs))

recursiveRender :: [String] -> IO ()
recursiveRender = foldr (\x -> (>>) (putStr x >> hFlush stdout)) (return ())

-- Zip three sequences of equal length
mergeThree :: [a] -> [a] -> [a] -> [a]
mergeThree (x:xs) (y:ys) (z:zs) = [x, y, z] ++ mergeThree xs ys zs
mergeThree [] [] [] = []

tailOrEmpty :: [String] -> [String]
tailOrEmpty [] = []
tailOrEmpty (_:rest) = rest

doUserCommand :: GHCIProc -> ReplState -> InputT IO (GHCIProc, ReplState)
doUserCommand proc replState = do
    minput <- getInputLine $ head $ prompts replState

    case minput of
        (Just "") -> liftIO $ reevaluate (inputs replState) proc
        (Just "undo") -> liftIO $ reevaluate (tailOrEmpty $ inputs replState) proc
        (Just "edit") ->
            liftIO $ fromEditor (bufferFromReplState replState) >>=
                (\newBuffer -> reevaluate (inputsFromBuffer newBuffer) proc)
        (Just s) -> liftIO $ doCommand proc replState (s ++ "\n") >>= (\x -> return (proc, x))
        --Nothing -> please crash

doCommand :: GHCIProc -> ReplState -> String -> IO ReplState
doCommand (GHCIProc ghci_stdin ghci_stdout ghci_stderr) (ReplState prompts inputs outputs) input = do
    hPutStr ghci_stdin input
    hFlush ghci_stdin
    (out, prompt) <- outputAndPrompt ghci_stdout
    err <- waitingOutput ghci_stderr
    return (ReplState (prompt:prompts) (input:inputs) ((err ++ out):outputs))

reevaluate :: [String] -> GHCIProc -> IO (GHCIProc, ReplState)
reevaluate inputs ghci = do

    -- close the old GHCI process
    hPutStr (inp ghci) "\xa"
    hFlush (inp ghci)

    newInterp <- interp
    header <- readTillPrompt (out newInterp)

    replState <- foldM (doCommand newInterp) (ReplState [header] [] []) (reverse inputs)
    return (newInterp, replState)

waitingOutput fileHandle = do
    isReady <- hReady fileHandle
    if isReady
        then do
            curChar <- hGetChar fileHandle
            rest <- waitingOutput fileHandle
            return (curChar : rest)
        else return ""

-- Returns the output from a previous command and prompt
outputAndPrompt :: Handle -> IO (String, String)
outputAndPrompt fileHandle = (\response ->
    let prompt:reversed_output = reverse (lines response) in
        (intercalate "\n" (reverse reversed_output ++ [""]) , prompt)) <$>
    readTillPrompt fileHandle

readTillPrompt :: Handle -> IO String
readTillPrompt fileHandle = do
    c <- hGetChar fileHandle
    if c == '>' then do
        nextChar <- hGetChar fileHandle
        isReady <- hReady fileHandle
        case (nextChar, isReady) of
            (' ', False) -> return (">" ++ [nextChar])
            (c, True) -> (['>', c] ++) <$> readTillPrompt fileHandle
    else (c:) <$> readTillPrompt fileHandle

interp :: IO GHCIProc
interp = createProcess (proc "ghci" []){
            std_out = CreatePipe,
            std_in = CreatePipe,
            std_err = CreatePipe} >>=
            (\(Just stdin, Just stdout, Just stderr, p_handle) ->
                return (GHCIProc stdin stdout stderr))

fromEditor :: String -> IO String
fromEditor buffer = do
    tempdir <- getTemporaryDirectory
    (tempfile, temph) <- openTempFile tempdir "session.hs"
    hPutStr temph buffer
    hFlush temph
    hClose temph
    visual <- lookupEnv "VISUAL"
    editor <- lookupEnv "EDITOR"
    (_, _, _, p_handle) <- createProcess
        (proc (fromMaybe (fromMaybe "vim" editor) visual) [tempfile])
    waitForProcess p_handle
    editedBuffer <- readFile tempfile
    removeFile tempfile
    return editedBuffer

inputsFromBuffer :: String -> [String]
inputsFromBuffer buffer = reverse [x ++ "\n" | x <- lines buffer, head x /= '-']

bufferFromReplState :: ReplState -> String
bufferFromReplState (ReplState _ inputs outputs) =
    bufferFromInputOutputPairs "--# save and quit to eval" $ zip inputs outputs

bufferFromInputOutputPairs :: String -> [(String, String)] -> String
bufferFromInputOutputPairs = foldl (\acc (input, output) ->
    input ++ intercalate "" ["--# " ++ x ++ "\n" | x <- lines output] ++ acc)

main :: IO ()
main = do
    proc <- interp
    initialPrompt <- readTillPrompt (out proc)
    runInputT defaultSettings (loop (proc, ReplState [initialPrompt] [] []))
    where
        loop :: (GHCIProc, ReplState) -> InputT IO ()
        loop (proc, rs) = do
            liftIO $ fullRenderAtRow 0 rs
            doUserCommand proc rs >>= loop
