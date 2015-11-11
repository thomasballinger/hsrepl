import System.Process
import System.IO
import Data.List
import Data.Maybe (fromMaybe)
import Control.Monad (foldM, foldM_)


data ReplState = ReplState { prompts :: [String],
                             inputs :: [String],
                             outputs :: [String] }
                             deriving (Show, Eq)

data GHCIProc = GHCIProc { inp :: Handle,
                           out :: Handle,
                           err :: Handle }
                           deriving (Show, Eq)

cursorAtSpot x y = "\x1b[" ++ show ( y + 1 ) ++ ";" ++ show (x + 1) ++ "H"
newScreen = do
    putStr "\x1b[2J"
    hFlush stdout

red s = "\x1b[41m" ++ s ++ "\x1b[0m"

fullRenderAtRow :: Int -> ReplState -> IO ()
fullRenderAtRow row replState = do
    putStr $ cursorAtSpot 0 row
    hFlush stdout
    newScreen
    fullRender replState

-- Renders a full REPL session; it better fit on one screen.
fullRender :: ReplState -> IO ()
fullRender (ReplState prompts inputs outputs) =
    recursiveRender (ReplState (reverse prompts) (reverse inputs) (reverse outputs))

recursiveRender :: ReplState -> IO ()
recursiveRender (ReplState prompts inputs outputs) =
    case prompts of
        [] -> return ()
        prompt:restPrompts -> do
            putStr prompt
            hFlush stdout
            case inputs of
                [] -> return ()
                inp:restInputs -> do
                    putStr $ inp ++ "\n"
                    hFlush stdout
                    case outputs of
                        [] -> return ()
                        out:restOutputs -> do
                            putStr out
                            hFlush stdout
                            recursiveRender (ReplState restPrompts restInputs restOutputs)

doUserCommand :: GHCIProc -> ReplState -> IO (GHCIProc, ReplState)
doUserCommand proc replState = do
    input <- getLine
    case input of
        "" -> reevaluate replState proc
        otherwise -> do
            newState <- doCommand proc replState input
            return (proc, newState)

doCommand :: GHCIProc -> ReplState -> String -> IO ReplState
doCommand (GHCIProc ghci_stdin ghci_stdout ghci_stderr) (ReplState prompts inputs outputs) input = do
    hPutStrLn ghci_stdin input
    hFlush ghci_stdin
    (out, prompt) <- outputAndPrompt ghci_stdout
    err <- waitingOutput ghci_stderr
    return (ReplState (prompt:prompts) (input:inputs) ((red err ++ out):outputs))

reevaluate :: ReplState -> GHCIProc -> IO (GHCIProc, ReplState)
reevaluate replState ghci = do

    -- close the old GHCI process
    hPutStr (inp ghci) "\xa"
    hFlush (inp ghci)

    newInterp <- interp
    header <- readTillPrompt (out newInterp)

    replState <- foldM (doCommand newInterp) (ReplState [header] [] []) (reverse $ inputs replState)
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
outputAndPrompt fileHandle = do
    response <- readTillPrompt fileHandle
    let lastNewline = fromMaybe 0 (elemIndex '\n' (reverse response)) in
        let splitSpot = length response - lastNewline in
            let output = take splitSpot response in
                let prompt = drop splitSpot response in
                    return (output, prompt)

readTillPrompt :: Handle -> IO String
readTillPrompt fileHandle = do
    c <- hGetChar fileHandle
    if c == '>'
        then do
            space <- hGetChar fileHandle
            return (">" ++ [space])
        else do
            rest <- readTillPrompt fileHandle
            return (c : rest)

interp :: IO GHCIProc
interp = do
    (Just ghci_stdin, Just ghci_stdout, Just ghci_stderr, p_handle) <- createProcess (proc "/usr/local/bin/ghci" []){ std_out = CreatePipe, std_in = CreatePipe, std_err = CreatePipe }
    return (GHCIProc ghci_stdin ghci_stdout ghci_stderr)

step :: (GHCIProc, ReplState) -> int -> IO (GHCIProc, ReplState)
step (proc, rs) _ = do
    fullRenderAtRow 10 rs
    (newproc, newrs) <- doUserCommand proc rs
    return (newproc, newrs)

main = do
    ghci <- interp
    r <- readTillPrompt (out ghci)
    putStr r
    hFlush stdout
    foldM_ step (ghci, ReplState [r] [] []) [1..]

