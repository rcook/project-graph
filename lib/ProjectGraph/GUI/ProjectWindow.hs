module ProjectGraph.GUI.ProjectWindow (display) where

import           Control.Monad (unless, when)
import           Control.Monad.Trans.Class (MonadTrans(..))
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
import           Graphics.Rendering.Cairo (Render, liftIO, restore, save, scale, translate)
import           Graphics.UI.Gtk
                    ( AttrOp(..)
                    , Click(..)
                    , EventMask(..)
                    , MouseButton(..)
                    , WidgetClass
                    , buttonPressEvent
                    , containerAdd
                    , draw
                    , drawingAreaNew
                    , eventButton
                    , eventClick
                    , eventCoordinates
                    , mainGUI
                    , mainQuit
                    , motionNotifyEvent
                    , objectDestroy
                    , on
                    , set
                    , widgetAddEvents
                    , widgetGetAllocatedHeight
                    , widgetGetAllocatedWidth
                    , widgetQueueDraw
                    , widgetShowAll
                    , windowDefaultHeight
                    , windowDefaultWidth
                    , windowNew
                    , windowTitle
                    )
import           Graphics.XDot.Parser (getOperations, getSize)
import           Graphics.XDot.Types (Object(..), Operation, Point, Rectangle)
import           Graphics.XDot.Viewer (drawAll)
import           ProjectGraph.App (appTitle, initApp)

data State = State
    { objects :: ([(Object String, Operation)], Rectangle)
    , bounds :: [(Object String, Rectangle)]
    , mouseCoords :: Point
    , hover :: Object String
    }

toXDotGraph :: (Ord a, ParseDotRepr dg n, PrintDot a) => DotGraph a -> IO (dg n)
toXDotGraph dg = graphvizWithHandle Dot dg (XDot Nothing) hGetDot

display :: (Ord a, PrintDot a) => DotGraph a -> IO ()
display dg = do
    quitWithoutGraphviz "Graphviz is not installed"

    xdg <- toXDotGraph dg

    let objs = (getOperations xdg, getSize xdg)

    state <- newIORef $ State objs [] (0, 0) None

    initApp

    window <- windowNew

    set window
        [ windowDefaultHeight := 512
        , windowDefaultWidth := 512
        , windowTitle := appTitle
        ]

    on window objectDestroy $ do
        mainQuit
        return ()

    {-
    button <- buttonNew
    containerAdd window button

    set button
        [ buttonLabel := "Close"
        ]

    on button buttonPressEvent $ lift $ do
        putStrLn "Quit app"
        mainQuit
        return True
    -}

    canvas <- drawingAreaNew
    containerAdd window canvas

    widgetAddEvents canvas [ PointerMotionMask ]

    on canvas draw $
        redraw canvas state

    on canvas motionNotifyEvent $ do
        coords <- eventCoordinates
        lift $ do
            modifyIORef state (\s -> s { mouseCoords = coords })
            onMotionNotifyEvent canvas state
            return True

    on canvas buttonPressEvent $ do
        button <- eventButton
        eClick <- eventClick
        lift $ do
            putStrLn "Event: canvas.buttonPressEvent"
            when (button == LeftButton && eClick == SingleClick) $ do
                putStrLn "Event: left single-click"
                onButtonPressEvent state
            return True

    widgetShowAll window

    on window objectDestroy $ do
        mainQuit
        return ()

    mainGUI

onButtonPressEvent :: IORef State -> IO ()
onButtonPressEvent state = do
    s <- readIORef state
    case hover s of
        Node t -> putStrLn $ "Node clicked: " ++ t
        Edge f t -> putStrLn $ "Edge clicked: " ++ f ++ " -> " ++ t
        _ -> return ()

onMotionNotifyEvent :: WidgetClass w => w -> IORef State -> IO ()
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
    unless (oldHover == hover s) $ widgetQueueDraw canvas

redraw :: WidgetClass w => w -> IORef State -> Render ()
redraw canvas state = do
    s <- liftIO $ readIORef state
    rw <- liftIO $ widgetGetAllocatedWidth canvas
    rh <- liftIO $ widgetGetAllocatedHeight canvas

    let (ops, size'@(_,_,sw,sh)) = objects s

    -- Proportional scaling
    let scalex = min (fromIntegral rw / sw) (fromIntegral rh / sh)
        scaley = scalex
        offsetx = 0.5 * fromIntegral rw
        offsety = 0.5 * fromIntegral rh

    save
    translate offsetx offsety
    scale scalex scaley

    result <- drawAll (hover s) size' ops

    restore

    let boundingBoxes = map
            (\(o, (x, y, w, h)) -> (o, (x * scalex + offsetx, y * scaley + offsety, w * scalex, h * scaley)))
            result

    liftIO $ modifyIORef state (\s' -> s' { bounds = boundingBoxes })
