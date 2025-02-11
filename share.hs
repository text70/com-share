import Network.Socket
import Network.Socket.ByteString (recv, send)
import Control.Concurrent (forkIO)
import Control.Monad (forever, void)
import System.Timeout (timeout)

main :: IO ()
main = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 9999 0)
    listen sock 5
    forever $ do
        (conn, _) <- accept sock
        void $ forkIO $ handle conn

handle :: Socket -> IO ()
handle sock = do
    env <- recv sock 4096
    if null env 
        then close sock
        else do
            void $ timeout 60000000 $ send sock env
            close sock
