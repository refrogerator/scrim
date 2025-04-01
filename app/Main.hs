{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

import Network.Socket qualified as NS
import Network.Socket.ByteString.Lazy (sendAll, recv, sendWithFds)
import qualified Data.ByteString.Lazy as B
import System.Environment (getEnv)
import Data.Binary.Put
import Data.Binary.Get
import Data.Bits (shiftR)
import Data.Word (Word8, Word16, Word32)
import Data.ByteString (ByteString)
import Control.Monad (forever, replicateM_, void, unless, when)
import Data.Map hiding (map, foldl')
import Lens.Micro.Platform
import Control.Monad.State (StateT, evalStateT, MonadIO (liftIO))
import Data.Foldable (sequenceA_, find)
import Foreign.C (CString, newCString)
import System.Posix.Types (Fd)
import Foreign (nullPtr, Ptr, free, Storable (peek, poke), plusPtr)
import System.Exit (exitSuccess)
import Codec.Picture (withImage, PixelRGB8 (PixelRGB8), DynamicImage (ImageRGB8))
import Codec.Picture.Saving (imageToPng)
import Control.Applicative (liftA3)

foreign import ccall "shm_open" shmOpen :: CString -> Int -> Int -> IO Int
foreign import ccall "shm_unlink" shmUnlink :: CString -> IO Int
foreign import ccall "ftruncate" ftruncate :: Int -> Int -> IO Int
foreign import ccall "mmap" mmap :: Ptr () -> Int -> Int -> Int -> Int -> Int -> IO (Ptr Int)

data MessageHeader = MessageHeader 
    { _objectId :: Word32
    , _methodId :: Word16 
    , _size     :: Word16
    } deriving Show

alignInt :: Integral a => a -> a -> a
alignInt i a = if i `mod` a /= 0 then i - (i `mod` a) + a else i


writeString :: B.ByteString -> Put
writeString s = do
    let l = B.length s
    putWord32host $ fromIntegral l + 1
    putLazyByteString s
    putWord8 0
    -- traceM (show $ fromIntegral (alignInt (l + 1) 4 - (l + 1)))
    replicateM_ (fromIntegral $ alignInt (l + 1) 4 - (l + 1)) (putWord8 0)

readString :: Get ByteString
readString = do
    l <- getWord32host
    s <- getByteString $ fromIntegral $ l - 1
    skip $ fromIntegral (alignInt l 4 - (l - 1))
    pure s

readMsg :: Get MessageHeader
readMsg = liftA3 MessageHeader
    getWord32host
    getWord16host
    getWord16host

getRegistry :: ObjectId -> Put
getRegistry r = do
    putInt32host 1 -- display singleton id
    putInt16host 1 -- get_registry id
    putInt16host $ 8 + 4 -- size
    putInt32host $ fromIntegral r -- new id for registry object

readGlobalMsg :: Get Global
readGlobalMsg = liftA3 Global 
    (fromIntegral <$> getWord32host) 
    readString 
    (fromIntegral <$> getWord32host)

readErrorMsg :: Get (ObjectId, Int, ByteString)
readErrorMsg = do
    object <- getWord32host -- 4
    errorId <- getWord32host -- 4
    message <- readString -- 16
    pure (fromIntegral object, fromIntegral errorId, message)

type Client a = StateT ClientState IO a

type ObjectId = Int

type MethodId = Int

type Object = Map MethodId Method

type Method = B.ByteString -> Client ()

data UserState = UserState
    { _registryId :: ObjectId
    , _seatName :: ByteString
    , _surface :: ObjectId
    , _layerSurface :: ObjectId
    , _output :: ObjectId
    , _outputSize :: (Int, Int)
    , _wlShm :: ObjectId
    , _shmBuffer :: ObjectId
    , _shmPool :: ObjectId
    , _copyBuffer :: Ptr Int
    , _framebuffer :: Ptr Int
    , _screenshotFormat :: Word32
    , _cursorPos :: (Int, Int)
    , _initialCursorPos :: (Int, Int)
    , _lock :: Bool
    }

data Global = Global 
    { _name :: ObjectId
    , _interface :: ByteString
    , _version :: Int
    } deriving Show
    
data ClientState = ClientState
    { _currentId :: ObjectId
    , _registry  :: Map ByteString ObjectId
    , _objects :: Map ObjectId Object
    , _globals :: [Global]
    , _socket :: NS.Socket
    , _userState :: UserState
    }

makeLenses ''UserState
makeLenses ''ClientState

handleErrorMsg :: B.ByteString -> Client ()
handleErrorMsg msg = do
    let (_object, _errorId, message) = runGet readErrorMsg msg
    liftIO $ putStrLn $ "ERROR: " <> show message

handleGlobalMsg :: B.ByteString -> Client ()
handleGlobalMsg msg = do
    let gm = runGet readGlobalMsg msg
    -- liftIO $ putStrLn $ "object " <> show gm._name <> " supporting interface " <> show gm._interface <> " found at version " <> show gm._version
    registry %= insert gm._interface (fromIntegral gm._name)
    -- handlers' <- use handlers
    -- for_ (handlers' !? gm._interface) (\(v, i, o) -> writeBind (fromIntegral gm._name) (B.fromStrict gm._interface) v o >>= i)

callAssociatedMethod :: ObjectId -> ObjectId -> B.ByteString -> Client ()
callAssociatedMethod o m msg = do
    os <- use objects
    let fun = os !? o >>= (!? m)
    case fun of
        Just f -> f msg
        Nothing -> pure ()

pollMessages :: Client MessageHeader
pollMessages = do
    sock <- use socket
    msg' <- liftIO $ recv sock 8
    let header = runGet readMsg msg'
    -- liftIO $ print header

    msg <- if header._size > 8 then 
        liftIO $ recv sock $ fromIntegral header._size - 8 
        else pure ""

    case header._objectId of
        1 -> case header._methodId of
            0 -> handleErrorMsg msg
            _ -> pure ()
        2 -> case header._methodId of
            0 -> handleGlobalMsg msg
            _ -> pure ()
        o -> callAssociatedMethod (fromIntegral o) (fromIntegral header._methodId) msg

    pure header

pollUntilSyncDone :: Client ()
pollUntilSyncDone = do
    -- send sync
    writeMsg 1 0 $ putWord32host 3

    let gaming = do
            header <- pollMessages
            unless (header._objectId == 3) gaming

    gaming

pollUntilUnlocked :: Client ()
pollUntilUnlocked = do
    let gaming = do
            void pollMessages
            l <- use $ userState . lock
            when l gaming

    gaming

runOnGlobal :: ByteString -> (ObjectId -> Client a) -> Client a
runOnGlobal i f = do
    globs <- use globals
    let glob = find (\a -> a._interface == i) globs
    case glob of
        Nothing -> error $ "interface " <> show i <> " not supported by compositor"
        Just g -> f g._name

bindToGlobal :: ByteString -> Int -> Client ObjectId
bindToGlobal i v = runOnGlobal i $ \n -> writeBind n (B.fromStrict i) v

newId :: Client ObjectId
newId = use currentId <* (currentId += 1)

writeBind :: ObjectId -> B.ByteString -> Int -> Client ObjectId
writeBind n interface version = do
    cur <- newId

    -- liftIO $ putStrLn $ "INFO: binding to " <> show interface <> " version " <> show version
    -- liftIO $ putStrLn $ "INFO: allocating object id " <> show cur

    registryId' <- use $ userState . registryId

    writeMsg registryId' 0 $ do
        putWord32host $ fromIntegral n
        writeString interface
        putWord32host $ fromIntegral version
        putWord32host $ fromIntegral cur

    pure cur

msgToBS :: ObjectId -> Int -> Put -> B.ByteString
msgToBS o m p = runPut $ do
        putWord32host header._objectId
        putWord16host header._methodId
        putWord16host header._size
        putLazyByteString s

    where s = runPut p
          header = MessageHeader (fromIntegral o) (fromIntegral m) (8 + fromIntegral (B.length s))

writeMsgS :: NS.Socket -> ObjectId -> Int -> Put -> IO ()
writeMsgS sock o m p = do
    sendAll sock $ msgToBS o m p

writeMsg :: ObjectId -> Int -> Put -> Client ()
writeMsg o m p = do
    sock <- use socket

    liftIO $ sendAll sock $ msgToBS o m p
    
handleInitialMessages :: NS.Socket -> ObjectId -> IO [Global]
handleInitialMessages sock callback = do
    msg' <- recv sock 8
    let header = runGet readMsg msg'

    msg <- if header._size > 8 then 
        liftIO $ recv sock $ fromIntegral header._size - 8 
        else pure ""

    if header._objectId == 2 then do
        let gm = runGet readGlobalMsg msg
        rest <- handleInitialMessages sock callback
        pure $ gm:rest
    else pure []

wlCompositorHandler :: ObjectId -> Client ()
wlCompositorHandler o = do
    cur <- newId

    -- create surface
    writeMsg o 0 $ putWord32host $ fromIntegral cur

    userState . surface .= cur

createFdBuffer :: Int -> IO (Fd, Ptr Int)
createFdBuffer fileSize = do
    path <- newCString "gaming"

    fd <- shmOpen path 194 0o600 -- O_RDWR | O_EXCL | O_CREAT

    -- print fd

    unlinkRes <- shmUnlink path -- TODO: handle errors
    truncateRes <- ftruncate fd fileSize

    free path

    filePtr <- mmap nullPtr fileSize (1 + 2) 1 fd 0 -- PROT_READ + PROT_WRITE, MAP_SHARED
    pure (fromIntegral fd, filePtr)

createBuffer :: Int -> Int -> Int -> Word32 -> Client (ObjectId, Fd, Ptr Int)
createBuffer offset width height format = do
    pool <- newId

    sock <- use socket

    oShm <- use $ userState . wlShm

    let fileSize = width * height * 4

    (fd, filePtr) <- liftIO $ createFdBuffer fileSize

    -- create pool
    liftIO $ sendWithFds sock (msgToBS oShm 0 (do
        putWord32host $ fromIntegral pool
        putWord32host $ fromIntegral fileSize)) [fd]

    cur <- newId

    let stride = width * 4

    -- create buffer
    writeMsg pool 0 $ do
        putWord32host $ fromIntegral cur
        putInt32host $ fromIntegral offset
        putInt32host $ fromIntegral width
        putInt32host $ fromIntegral height
        putInt32host $ fromIntegral stride
        putWord32host format

    pure (cur, fd, filePtr)

wlShmHandler :: ObjectId -> Client ()
wlShmHandler o = userState . wlShm .= o

-- getModeInfo :: Get String
-- getModeInfo = do
--     _ <- getWord32host
--     width <- getInt32host
--     height <- getInt32host
--     refresh <- getInt32host
--     pure $ printf "%vx%v@%v" width height (fromIntegral @Int32 @Float refresh / 1000.0)

noInitHandler :: ObjectId -> Client ()
noInitHandler _ = pure ()

wlrSurfaceConfigure :: ObjectId -> ObjectId -> B.ByteString -> Client ()
wlrSurfaceConfigure fb o str = do
    let (s, _w, _h) = runGet (liftA3 (,,) getWord32host getWord32host getWord32host) str

    -- liftIO $ print (s, w, h)

    -- ack_configure
    writeMsg o 6 $ putWord32host s

    surf <- use $ userState . surface

    -- attach
    writeMsg surf 1 $ do
        putWord32host $ fromIntegral fb
        putWord32host 0
        putWord32host 0

    -- commit
    writeMsg surf 6 $ pure ()

writePPM :: (Int, Int) -> (Int, Int) -> Client ()
writePPM c d = do
    let (startX, startY) = min c d
        (endX, endY)     = max c d
        width  = endX - startX + 1
        height = endY - startY + 1

    sp <- use $ userState . copyBuffer

    (swidth, _) <- use $ userState . outputSize

    let getPixel o' x y = do
            let o = ((x + (y * swidth)) * 4) + o'
            r <- peek @Word8 $ plusPtr sp (o + 0)
            g <- peek @Word8 $ plusPtr sp (o + 1)
            b <- peek @Word8 $ plusPtr sp (o + 2)
            pure $ PixelRGB8 r g b

    image <- liftIO $ withImage width height $ getPixel $ (startX + (startY * swidth)) * 4

    liftIO $ B.putStr $ imageToPng $ ImageRGB8 image

    -- liftIO $ B.writeFile "/tmp/screenshot.png" $ imageToPng $ ImageRGB8 image

pattern ARGB8888 :: Word32
pattern ARGB8888 = 0x0

pattern XRGB8888 :: Word32
pattern XRGB8888 = 0x1

pattern XBGR8888 :: Word32
pattern XBGR8888 = 0x34324258

pattern ABGR8888 :: Word32
pattern ABGR8888 = 0x34324241

copyBuf :: Client ()
copyBuf = do
    fb <- use $ userState . framebuffer
    cb <- use $ userState . copyBuffer
    format <- use $ userState . screenshotFormat

    (swidth, sheight) <- use $ userState . outputSize

    let setPixelXRGB p x y (r, g, b) = do
            let o = (x + (y * swidth)) * 4
            poke @Word8 (plusPtr p (o + 2)) r
            poke @Word8 (plusPtr p (o + 1)) g
            poke @Word8 (plusPtr p (o + 0)) b
        setPixel = setPixelXRGB

    let getPixelXBGR p x y = do
            let o = (x + (y * swidth)) * 4
            r <- peek @Word8 $ plusPtr p (o + 0)
            g <- peek @Word8 $ plusPtr p (o + 1)
            b <- peek @Word8 $ plusPtr p (o + 2)
            pure (r, g, b)

        getPixelXRGB p x y = do
            let o = (x + (y * swidth)) * 4
            r <- peek @Word8 $ plusPtr p (o + 2)
            g <- peek @Word8 $ plusPtr p (o + 1)
            b <- peek @Word8 $ plusPtr p (o + 0)
            pure (r, g, b)

        getPixel = case format of
            ARGB8888 -> getPixelXRGB
            XRGB8888 -> getPixelXRGB
            ABGR8888 -> getPixelXBGR
            XBGR8888 -> getPixelXBGR
            _ -> error "unsupported format"
            
    liftIO $ sequenceA_ $ (\y x -> getPixel cb x y >>= setPixel fb x y) <$> [0..sheight - 1] <*> [0..swidth - 1]

wlrScreencopyManagerHandler :: ObjectId -> Client ()
wlrScreencopyManagerHandler o = do
    cur <- newId

    op <- use $ userState . output

    -- liftIO $ putStrLn "INFO: setting buffer"

    writeMsg o 0 $ do
        putWord32host $ fromIntegral cur
        putInt32host 0
        putWord32host $ fromIntegral op

    objects %= insert cur (fromList [(0, \s -> do
            let (f, _w, _h, _st) = runGet ((,,,) <$> getWord32host <*> getWord32host <*> getWord32host <*> getWord32host) s

            -- liftIO $ putStrLn "INFO: setting buffer PART 2"

            (width, height) <- use $ userState . outputSize

            -- format: xbgr8888
            (buf, _, ptr) <- createBuffer 0 width height f

            userState . screenshotFormat .= f

            userState . copyBuffer .= ptr

            writeMsg cur 0 $ putWord32host $ fromIntegral buf)
        , (2, const (userState . lock .= False)) -- lock until screenshot taken
            ])

    userState . lock .= True

    pollUntilUnlocked


wlrLayerShellHandler :: ObjectId -> Client ()
wlrLayerShellHandler o = do
    cur <- newId
    surf <- use $ userState . surface

    writeMsg o 0 $ do -- get layer surface
        putWord32host $ fromIntegral cur  -- wlr_layer_surface
        putWord32host $ fromIntegral surf -- surface
        putWord32host 0                   -- null output
        putWord32host 3                   -- overlay layer
        writeString "screenshot"          -- namespace

    userState . layerSurface .= cur

    writeMsg cur 1 $ putWord32host 0b1111 -- set anchor 
    writeMsg cur 2 $ putInt32host (-1)    -- set exclusive zone
    writeMsg cur 4 $ putWord32host 1      -- set keyboard interactivity
    writeMsg surf 6 (pure ())             -- commit

    (width, height) <- use $ userState . outputSize

    (fb, _, ptr) <- createBuffer 0 (fromIntegral width) (fromIntegral height) XRGB8888

    userState . framebuffer .= ptr

    copyBuf

    objects %= insert cur (fromList [(0, wlrSurfaceConfigure fb cur), (1, pure $ liftIO exitSuccess)])

wlSeatHandler :: ObjectId -> Client ()
wlSeatHandler o = do
    objects %= insert o (fromList [(1, \v -> do
                -- liftIO $ SB.putStrLn $ runGet readString v
                userState . seatName .= B.toStrict v
            )])

    keyboard <- newId

    -- get_keyboard
    writeMsg o 1 $ putWord32host $ fromIntegral keyboard

    objects %= insert keyboard (fromList [(3, \v -> do
                let key = runGet (do
                        _s <- getWord32host
                        _t <- getWord32host
                        getWord32host) v
                liftIO $ when (key == 0x1) -- escape
                    exitSuccess
                )])

    pointer <- newId

    -- get_pointer
    writeMsg o 0 $ putWord32host $ fromIntegral pointer

    shapeManager <- bindToGlobal "wp_cursor_shape_manager_v1" 1

    shapeManagerPointer <- newId
    writeMsg shapeManager 1 $ do
        putWord32host $ fromIntegral shapeManagerPointer
        putWord32host $ fromIntegral pointer

    objects %= insert pointer (fromList [(3, \v -> do
            let ms = runGet (do
                        _s <- getWord32host
                        _t <- getWord32host
                        b' <- getWord32host
                        s' <- getWord32host
                        pure (b', s')
                    ) v
            -- 272 == BTN_LEFT
            -- 273 == BTN_RIGHT
            -- liftIO $ print ms

            cursor <- use $ userState . cursorPos

            case ms of
                (272, 1) -> userState . initialCursorPos .= cursor
                (272, 0) -> do
                    ip <- use $ userState . initialCursorPos
                    -- liftIO $ print (ip, cursor)
                    writePPM ip cursor
                    liftIO exitSuccess

                -- exit on right click
                (273, 1) -> liftIO exitSuccess

                _ -> pure ()

            -- liftIO $ print cursor
        )
        , (2, \v -> do
            let cursor = runGet (do
                        _t <- getWord32host
                        x' <- getWord32host
                        y' <- getWord32host
                        pure (fromIntegral $ x' `shiftR` 8, fromIntegral $ y' `shiftR` 8)
                    ) v
            userState . cursorPos .= cursor
        )
        , (0, \v -> do
            let ev = runGet getWord32host v
            writeMsg shapeManagerPointer 1 $ do
                putWord32host ev
                putWord32host 8
        )])


initClient :: Client ()
initClient = do
    bindToGlobal "wl_output" 4 >>= \o -> do
        userState . output .= o
        objects %= insert o 
            (fromList 
                [ (1, \v -> userState . outputSize .= runGet (do
                        _ <- getWord32host
                        width <- getInt32host
                        height <- getInt32host
                        pure (fromIntegral width, fromIntegral height)) v)
                -- , (4, liftIO . SB.putStrLn . runGet readString)
                ])
    bindToGlobal "wl_seat" 9 >>= wlSeatHandler
    bindToGlobal "wl_compositor" 6 >>= wlCompositorHandler
    bindToGlobal "wl_shm" 1 >>= wlShmHandler
    bindToGlobal "zwlr_screencopy_manager_v1" 2 >>= wlrScreencopyManagerHandler
    bindToGlobal "zwlr_layer_shell_v1" 4 >>= wlrLayerShellHandler
    forever pollMessages

runClient :: IO ()
runClient = do
    sock <- NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol
    runtimeDir <- getEnv "XDG_RUNTIME_DIR"
    waylandDisplay <- getEnv "WAYLAND_DISPLAY"
    let waylandPath = runtimeDir <> "/" <> waylandDisplay
    -- putStrLn $ "connecting to " <> waylandPath
    NS.connect sock $ NS.SockAddrUnix waylandPath

    -- create registry
    writeMsgS sock 1 1 $ putWord32host 2

    -- send sync
    writeMsgS sock 1 0 $ putWord32host 3
    
    globs <- handleInitialMessages sock 3

    -- traverse_ (\gm -> putStrLn $ "object " <> show gm._name <> " supporting interface " <> show gm._interface <> " found at version " <> show gm._version)  globs

    evalStateT initClient $ ClientState 4 empty empty globs sock (UserState 2 "" 0 0 0 (0, 0) 0 0 0 nullPtr nullPtr 0 (0, 0) (0, 0) False)

main :: IO ()
main = runClient
