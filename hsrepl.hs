import System.Process (createProcess, waitForProcess, StdStream(CreatePipe), std_out, std_in, std_err, proc)
import System.IO (Handle, hFlush, stdout, hPutStr, hGetChar, hClose, openTempFile, hReady)
import Data.List (elemIndex, intercalate)
import Data.Maybe (fromMaybe)
import Control.Monad (foldM, foldM_)
import System.Directory(getTemporaryDirectory, removeFile)
import System.Environment(lookupEnv)


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
fullRender :: ReplState -> IO ()
fullRender (ReplState prompts inputs outputs) =
    recursiveRender (combineThree (reverse prompts) (reverse inputs) (reverse outputs))

recursiveRender :: [String] -> IO ()
recursiveRender (x:xs) = putStr x >> hFlush stdout >> recursiveRender xs
recursiveRender [] = return ()

-- Zip three sequences of lengths x, y, z where x >= y >= z and x <= z + 1
combineThree :: [a] -> [a] -> [a] -> [a]
combineThree (x:xs) (y:ys) (z:zs) = [x, y, z] ++ combineThree xs ys zs
combineThree (x:xs) (y:yx) [] = [x, y]
combineThree (x:xs) [] [] = [x]

tailOrEmpty :: [String] -> [String]
tailOrEmpty [] = []
tailOrEmpty (_:rest) = rest

doUserCommand :: GHCIProc -> ReplState -> IO (GHCIProc, ReplState)
doUserCommand proc replState =
    getLine >>= (\input -> case input of
        "" -> reevaluate (inputs replState) proc
        "undo" -> reevaluate (tailOrEmpty $ inputs replState) proc
        "edit" ->
            fromEditor (bufferFromReplState replState) >>=
                (\newBuffer -> reevaluate (inputsFromBuffer newBuffer) proc)
        otherwise -> doCommand proc replState (input ++ "\n") >>= (\x -> return (proc, x)))

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
outputAndPrompt fileHandle =
    readTillPrompt fileHandle >>= (\response ->
        let prompt:reversed_output = reverse (lines response) in
            return (intercalate "\n" (reverse reversed_output ++ [""]) , prompt))

readTillPrompt :: Handle -> IO String
readTillPrompt fileHandle = do
    c <- hGetChar fileHandle
    if c == '>' then do
        nextChar <- hGetChar fileHandle
        isReady <- hReady fileHandle
        case (nextChar, isReady) of
            (' ', False) -> return (">" ++ [nextChar])
            (c, True) -> readTillPrompt fileHandle >>= \rest ->
                            return ('>':c:rest)
    else readTillPrompt fileHandle >>= \rest -> return (c : rest)

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
    editor <- lookupEnv "EDITOR"  -- TODO possibly skip this lookup
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
    bufferFromInputsOutputs (reverse inputs) (reverse outputs)

bufferFromInputsOutputs :: [String] -> [String] -> String
bufferFromInputsOutputs (input:inputs) (output:outputs) =
    input ++
    intercalate "" ["--# " ++ x ++ "\n" | x <- lines output] ++
    bufferFromInputsOutputs inputs outputs
bufferFromInputsOutputs [] [] = ""

step :: (GHCIProc, ReplState) -> int -> IO (GHCIProc, ReplState)
step (proc, rs) _ = fullRenderAtRow 0 rs >> doUserCommand proc rs

main = interp >>= \ghci ->
    readTillPrompt (out ghci) >>= \r ->
        foldM_ step (ghci, ReplState [r] [] []) [1..]
