{- LANGUAGE ForeignFunctionInterface -}

import Prelude hiding (Left, Right)

import Control.Applicative
import Control.Arrow (first, (&&&))
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import Data.Maybe
import Data.Time
import Debug.Trace
import Foreign
import Foreign.C.String
import Foreign.C.Types
import System.Exit (exitSuccess)
import System.Random
import System.Random.Shuffle

foreign import ccall "binding.h" initialize :: IO CInt
foreign import ccall "binding.h" getWidth :: IO CInt
foreign import ccall "binding.h" getHeight :: IO CInt

foreign import ccall "binding.h" nextFrame :: IO ()

foreign import ccall "binding.h getKey" c_getKey :: IO CInt

data Key = KeyNone
         | KeyEscape
         | KeySpace
         | KeyToggleRects
         | KeyDist Int
         | KeyOther CInt
         deriving (Show, Eq, Ord)

getKey :: IO Key
getKey = toKey <$> c_getKey
    where toKey (-1)  = KeyNone
          toKey 27    = KeyEscape
          toKey 32    = KeySpace
          toKey 114   = KeyToggleRects -- 'r'
          toKey 45    = KeyDist (-1)   -- '-'
          toKey key | key >= 48 && key < 58 = KeyDist (fromIntegral $ key - 48) -- '0'..'9'
                    | otherwise             = KeyOther key

foreign import ccall "binding.h getMarkers" c_getMarkers :: IO (Ptr CInt)
foreign import ccall "binding.h getMarkersLen" c_getMarkersLen :: IO CSize

newtype Marker = Marker { markerId :: Int } deriving (Show, Eq, Ord)

getMarkers :: IO [Marker]
getMarkers = do ptr <- c_getMarkers
                len <- c_getMarkersLen
                map (Marker . fromIntegral) <$> peekArray (fromIntegral len) ptr

foreign import ccall "binding.h getPosition" c_getPosition :: CInt -> IO (Ptr Point)

getPosition :: Marker -> IO Point
getPosition m = peek =<< (c_getPosition . fromIntegral . markerId $ m)

foreign import ccall "binding.h drawMarker" c_drawMarker :: CInt -> Ptr Color -> IO ()

data Color = Color Double Double Double

instance Storable Color where
    sizeOf _ = 3 * sizeOf (undefined :: CUChar)
    alignment _ = alignment (undefined :: CUChar)
    peek ptr = do
        let c2f = (/ 255) . realToFrac
        let ptr' = castPtr ptr :: Ptr CUChar
        r <- c2f <$> peekElemOff ptr' 0
        g <- c2f <$> peekElemOff ptr' 1
        b <- c2f <$> peekElemOff ptr' 2
        return $ Color r g b
    poke ptr (Color r g b) = do
        let f2c = round . (* 255)
        let ptr' = castPtr ptr :: Ptr CUChar
        pokeElemOff ptr' 0 $ f2c r
        pokeElemOff ptr' 1 $ f2c g
        pokeElemOff ptr' 2 $ f2c b

red  = Color 1 0 0
blue = Color 0 0 1

drawMarker :: Color -> Marker -> IO ()
drawMarker c m = with c $ c_drawMarker (fromIntegral $ markerId m)

foreign import ccall "binding.h drawChevron" c_drawChevron :: Ptr Point -> Ptr Color -> IO ()
foreign import ccall "binding.h drawRect" c_drawRect :: Ptr Point -> Ptr Color -> IO ()
foreign import ccall "binding.h drawText" c_drawText :: Ptr Point -> CString -> Ptr Color -> IO ()

data Point = Point Int Int
           deriving (Show, Eq)

instance Storable Point where
    sizeOf _ = 2 * sizeOf (undefined :: CInt)
    alignment _ = sizeOf (undefined :: CInt)
    peek ptr = do
        let ptr' = castPtr ptr :: Ptr CInt
        x <- fromIntegral <$> peekElemOff ptr' 0
        y <- fromIntegral <$> peekElemOff ptr' 1
        return $ Point x y
    poke ptr (Point x y) = do
        let ptr' = castPtr ptr :: Ptr CInt
        pokeElemOff ptr' 0 $ fromIntegral x
        pokeElemOff ptr' 1 $ fromIntegral y

keyPoints :: Maybe Point -> Reader ARState (Int, Int, Int, Int, Int, Int, Int)
keyPoints focus = do
        width <- asks width
        height <- asks height
        dist <- asks dist
        return $ let cx = width `div` 2
                     cy = height `div` 2
                     Point fx fy = case focus of
                                       Just point | dist /= -1 -> point
                                       _                       -> Point cx cy
                     dx = fx - cx
                     dy = fy - cy
                     d = if dist == -1
                         then 0
                         else realToFrac dist
                     s = 1 - d * 0.05 :: Double
                     scale = round . (* s) . realToFrac
                     sw = scale width
                     sh = scale height
                     w' = cx - cy
                     w = dx + if dist == -1
                              then sw
                              else w' + sh
                     h = dy + sh
                     x = dx + if dist == -1
                              then width - w
                              else w' + height - sh
                     y = dy + height - sh
                     b = scale $ height `div` 8
                     clampX = max 0 . min width
                     clampY = max 0 . min height
                 in (clampX x, clampY y, clampX w, clampY h, fx, fy, b)

chevronPoints :: Side -> Maybe Point -> Reader ARState [Point]
chevronPoints side focus = do
        useRects <- asks useRects
        (x, y, w, h, cx, cy, b) <- keyPoints focus
        return $ let wb = w - b
                     hb = h - b
                     xb = x + b
                     yb = y + b
                     b2 = b `div` 2
                     cxl = cx - b2
                     cxr = cx + b2
                     cyu = cy - b2
                     cyd = cy + b2
                 in if useRects
                    then case side of
                             Right -> [ Point wb  cyu
                                      , Point w   cyd
                                      ]
                             Up    -> [ Point cxl y
                                      , Point cxr yb
                                      ]
                             Left  -> [ Point x   cyu
                                      , Point xb  cyd
                                      ]
                             Down  -> [ Point cxl hb
                                      , Point cxr h
                                      ]
                    else case side of
                             Right -> [ Point w  h
                                      , Point wb hb
                                      , Point wb yb
                                      , Point w  y
                                      ]
                             Up    -> [ Point x  y
                                      , Point xb yb
                                      , Point wb yb
                                      , Point w  y
                                      ]
                             Left  -> [ Point x  h
                                      , Point x  y
                                      , Point xb yb
                                      , Point xb hb
                                      ]
                             Down  -> [ Point x  h
                                      , Point w  h
                                      , Point wb hb
                                      , Point xb hb
                                      ]

textPosition side focus = do
        (x, y, w, h, cx, cy, b) <- keyPoints focus
        return $ let hb = b `div` 2
                 in case side of
                        Right -> Point (w - hb) cy
                        Up    -> Point cx       (y + hb)
                        Left  -> Point (x + hb) cy
                        Down  -> Point cx       (h - hb)

drawChevron :: Maybe Point -> Bool -> Chevron -> ReaderT ARState IO ()
drawChevron focus selected (Chevron side text) = do
        useRects <- asks useRects
        pos <- dropIO $ textPosition side focus
        points <- dropIO $ chevronPoints side focus
        liftIO $ withCString text $ \cstr ->
                 withArray points $ \pointsptr ->
                 with c $ \cptr ->
                 with c' $ \cptr' ->
                 with pos $ \pptr -> do
                     if useRects
                     then c_drawRect pointsptr cptr
                     else c_drawChevron pointsptr cptr
                     c_drawText pptr cstr cptr'
    where c   = Color cc cc cc
          c'  = Color cc' cc' cc'
          cc  | selected  = 1
              | otherwise = 0
          cc' = 1 - cc

data Side = Right
          | Up
          | Left
          | Down
          deriving (Show, Eq, Ord, Enum, Bounded)

instance Random Side where
    randomR (l, u) = first toEnum . randomR (fromEnum l, fromEnum u)
    random = randomR (minBound, maxBound)

data Chevron = Chevron { chevSide :: Side
                       , chevText :: String
                       }
                       deriving (Show, Eq, Ord)

chevron (Just side) (_:[]) = [Chevron side (show side)]
chevron _ _ = []

data Mode = Idle
          | Running
          deriving (Show, Eq, Ord)

data ARState = ARState { width    :: Int
                       , height   :: Int
                       , useRects :: Bool
                       , dist     :: Int
                       , unshown  :: [Maybe Side]
                       , shown    :: [Maybe Side]
                       , lastTick :: UTCTime
                       }
             deriving (Show)

dropIO :: Reader ARState a -> ReaderT ARState IO a
dropIO = mapReaderT (return . runIdentity)

readOnly :: ReaderT ARState IO a -> StateT ARState IO a
readOnly r = do s <- get
                liftIO $ runReaderT r s

dispSequence = do
    let basic = [Up, Down, Left, Right]
    let seq1 = Nothing:addGaps basic
    seq2 <- fmap addGaps . shuffleM . take (4 * 4) . cycle $ basic
    let seq3 = map Just [Up, Down, Up]
    let seq4 = map Just [Left, Right, Left]
    let seq5 = map Just [Right, Up, Left, Down, Right] 
    let seq6 = map Just [Left, Up, Right, Down, Left] 
    return $ concat . intersperse [Nothing] $ [seq1, seq2, seq3, seq4, seq5, seq6]
  where
    addGaps = intersperse Nothing . map Just

main :: IO ()
main = do success <- initialize
          unless (success == 1) $ error "Failed to initialize"
          width <- fromIntegral <$> getWidth
          height <- fromIntegral <$> getHeight
          ds <- evalRandIO dispSequence
          putStrLn $ "Display sequence: " ++ show ds
          now <- liftIO getCurrentTime
          evalStateT (loop Idle) $ ARState width height False 0 ds [] now

loop :: Mode -> StateT ARState IO ()
loop mode = do
        liftIO nextFrame
        markers <- liftIO getMarkers
        liftIO $ mapM_ (drawMarker red) markers
        key <- liftIO getKey
        when (key /= KeyNone) . liftIO . putStrLn $ "Key pressed: " ++ show key
        when (key == KeyEscape) $ liftIO exitSuccess
        case mode of
            Idle -> do
                case key of
                    KeyToggleRects -> do
                        useRects' <- not <$> gets useRects
                        liftIO . putStrLn $ "Set useRects to: " ++ show useRects'
                        modify $ \s -> s { useRects = useRects' }

                    KeyDist n      -> do
                        liftIO . putStrLn $ "Set dist to: " ++ show n
                        modify $ \s -> s { dist = n }

                    KeySpace       -> do
                        now <- liftIO getCurrentTime
                        modify $ \s -> s { unshown = reverse (shown s) ++ unshown s
                                         , shown = []
                                         , lastTick = (addUTCTime (-100) now)
                                         }
                        s <- get
                        liftIO . putStrLn $ "Beginning sequence with state: " ++ show s
                        loop Running

                    _ -> return ()

            Running -> do
                case key of
                    KeySpace -> do
                        liftIO $ putStrLn "Cancelling sequence"
                        loop Idle
                    _ -> return ()
                now <- liftIO getCurrentTime
                lastTick' <- gets lastTick
                side' <-
                    if diffUTCTime now lastTick' >= 1
                    then do
                        (side', rest) <- (listToMaybe &&& tail) <$> gets unshown
                        case side' of
                            Just side -> do
                                modify $ \s -> s { unshown = rest
                                                 , shown = side:shown s
                                                 , lastTick = now
                                                 }
                                liftIO . putStrLn $ "Side: " ++ show side
                                return (Just side)
                            Nothing -> return Nothing
                    else Just . head <$> gets shown
                case side' of
                    Just side -> do
                        let chev = chevron side markers
                        focus <- liftIO $ case listToMaybe markers of
                                              Just m  -> getPosition m >>= return . Just
                                              Nothing -> return Nothing
                        mapM_ (readOnly . drawChevron focus False) $ chev
                    Nothing -> do
                        liftIO $ putStrLn "Finished sequence"
                        loop Idle
        when (null markers) $ do
            liftIO $ putStrLn "ERROR: Lost marker lock"
            loop Idle
        loop mode
