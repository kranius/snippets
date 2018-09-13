import Control.Concurrent
import Control.Monad (forever)
import Control.Concurrent.Chan

data AskForMeow = GibFood | Smile

meowMe chan chanBack = do
  niceTry <- readChan chan
  case niceTry of
    GibFood -> writeChan chanBack "Meow"
    Smile   -> writeChan chanBack "No"

cat action = do
  forkIO $ forever action


main :: IO ()
main = do
  putStrLn "Hey kitty kitty"

  foodInputChan <- newChan
  catOutputChan <- newChan

  cat $ meowMe foodInputChan catOutputChan

  writeChan foodInputChan Smile
  response <- readChan catOutputChan
  putStrLn response

  writeChan foodInputChan GibFood
  response' <- readChan catOutputChan
  putStrLn response'

  getLine
  return ()
