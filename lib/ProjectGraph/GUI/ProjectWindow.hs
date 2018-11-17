{-# LANGUAGE OverloadedLabels #-}

module ProjectGraph.GUI.ProjectWindow (display) where

import           Control.Monad (unless, when)
import           Control.Monad.Trans.Reader (runReaderT)
import           Data.Char (chr)
import           Data.GraphViz
                    ( DotGraph
                    , GraphvizCommand(..)
                    , GraphvizOutput(..)
                    , PrintDot
                    , graphvizWithHandle
                    , quitWithoutGraphviz
                    )
import           Data.GraphViz.Commands.IO (hGetDot)
import           Data.GraphViz.Types (ParseDotRepr)
import           Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import qualified Data.Text as Text (pack)
import           Data.Word (Word32)
import           Foreign.Ptr (castPtr)
import           Graphics.Rendering.Cairo
                    ( liftIO
                    , restore
                    , save
                    , scale
                    , translate
                    )
import           Graphics.Rendering.Cairo.Internal (Render(..))
import           Graphics.Rendering.Cairo.Types (Cairo(..))
import           GI.Cairo (Context(..))
import           GI.Gdk (EventMask(..), ModifierType(..), withManagedPtr)
import           GI.Gtk
                    ( AttrOp(..)
                    , DrawingArea(..)
                    , WindowPosition(..)
                    , Window(..)
                    , get
                    , mainQuit
                    , new
                    , on
                    )
import           Graphics.XDot.Parser (getOperations, getSize)
import           Graphics.XDot.Types (Object(..), Operation, Point, Rectangle)
import           Graphics.XDot.Viewer (drawAll)
import           ProjectGraph.App (appTitle, gtkMain, initApp)

type Objects = ([(Object String, Operation)], Rectangle)

data Zoom = Proportional | ZoomLevel Int

data State = State
    { objects :: Objects
    , bounds :: [(Object String, Rectangle)]
    , mouseCoords :: Point
    , hover :: Object String
    , zoom :: Zoom
    }

type Width = Int

type Height = Int

initialState :: Objects -> State
initialState objs = State objs [] (0, 0) None Proportional

leftButton :: Word32
leftButton = 1

toXDotGraph :: (Ord a, ParseDotRepr dg n, PrintDot a) => DotGraph a -> IO (dg n)
toXDotGraph dg = graphvizWithHandle Dot dg (XDot Nothing) hGetDot

incrementZoom :: Zoom -> Zoom
incrementZoom Proportional = ZoomLevel 10
incrementZoom (ZoomLevel n) = ZoomLevel (n + 1)

decrementZoom :: Zoom -> Zoom
decrementZoom Proportional = ZoomLevel 10
decrementZoom (ZoomLevel n)
    | n == 1 = ZoomLevel 1
    | otherwise = ZoomLevel (n - 1)

display :: (Ord a, PrintDot a) => DotGraph a -> IO ()
display dg = do
    quitWithoutGraphviz "Graphviz is not installed"
    xdg <- toXDotGraph dg

    let objs = (getOperations xdg, getSize xdg)

    state <- newIORef $ initialState objs

    initApp

    window <- new Window
                [ #defaultHeight := 512
                , #defaultWidth := 512
                , #title := Text.pack appTitle
                ]

    canvas <- new DrawingArea []
    #add window canvas

    #setPosition window WindowPositionCenter

    on window #keyPressEvent $ \eventKey -> do
        keyState <- get eventKey #state
        c <- (chr . fromIntegral) <$> get eventKey #keyval
        when (keyState == [ ModifierTypeControlMask ]) $
            case c of
                '0' -> modifyIORef state (\s -> s { zoom = Proportional })
                '-' -> modifyIORef state (\s -> s { zoom = decrementZoom (zoom s) })
                '=' -> modifyIORef state (\s -> s { zoom = incrementZoom (zoom s) })
        #queueDraw canvas
        return True

    on window #destroy mainQuit

    #addEvents canvas
                [ EventMaskButtonPressMask
                , EventMaskPointerMotionMask
                ]

    on canvas #draw $ \context -> do
        w <- fromIntegral <$> #getAllocatedWidth canvas
        h <- fromIntegral <$> #getAllocatedHeight canvas
        renderWithContext context $ redraw w h state
        return True

    on canvas #motionNotifyEvent $ \eventMotion -> do
        x <- get eventMotion #x
        y <- get eventMotion #y
        modifyIORef state (\s -> s { mouseCoords = (x, y) })
        onMotionNotifyEvent canvas state
        return True

    on canvas #buttonPressEvent $ \eventButton -> do
        button <- get eventButton #button
        when (button == leftButton) $ do
            onButtonPressEvent state
        return True

    #showAll window

    gtkMain

onButtonPressEvent :: IORef State -> IO ()
onButtonPressEvent state = do
    s <- readIORef state
    case hover s of
        Node t -> putStrLn $ "Node clicked: " ++ t
        Edge f t -> putStrLn $ "Edge clicked: " ++ f ++ " -> " ++ t
        _ -> return ()

onMotionNotifyEvent :: DrawingArea -> IORef State -> IO ()
onMotionNotifyEvent canvas state = do
    oldS <- readIORef state
    let oldHover = hover oldS

    modifyIORef state $ \s' -> (
            let (mx, my) = mouseCoords s'
                check (name', (x,y,w,h)) =
                    if x <= mx && mx <= x + w &&
                        y <= my && my <= y + h
                    then name' else None

                validOne (None:xs) = validOne xs
                validOne (x:_) = x
                validOne _ = None
            in s' {hover = validOne $ map check (bounds s')}
            )

    s <- readIORef state
    unless (oldHover == hover s) $ #queueDraw canvas

renderWithContext :: GI.Cairo.Context -> Render () -> IO ()
renderWithContext ct r = withManagedPtr ct $ \p ->
    runReaderT (runRender r) (Cairo (castPtr p))

scaleDiagram :: Zoom -> Width -> Height -> Double -> Double -> (Double, Double, Double, Double)
scaleDiagram Proportional rw rh sw sh =
    let scaleX = min (fromIntegral rw / sw) (fromIntegral rh / sh)
        scaleY = scaleX
        offsetX = 0.5 * fromIntegral rw
        offsetY = 0.5 * fromIntegral rh
    in (scaleX, scaleY, offsetX, offsetY)
scaleDiagram (ZoomLevel n) rw rh sw sh =
    let offsetX = 0.5 * fromIntegral rw
        offsetY = 0.5 * fromIntegral rh
        s = fromIntegral n / 10.0
    in (s, s, offsetX, offsetY)

redraw :: Width -> Height -> IORef State -> Render ()
redraw rw rh state = do
    s <- liftIO $ readIORef state

    let (ops, size'@(_, _, sw, sh)) = objects s
        (scaleX, scaleY, offsetX, offsetY) = scaleDiagram (zoom s) rw rh sw sh

    save
    translate offsetX offsetY
    scale scaleX scaleY

    result <- drawAll (hover s) size' ops

    restore

    let boundingBoxes = map
            (\(o, (x, y, w, h)) -> (o, (x * scaleX + offsetX, y * scaleY + offsetY, w * scaleX, h * scaleY)))
            result

    liftIO $ modifyIORef state (\s' -> s' { bounds = boundingBoxes })
