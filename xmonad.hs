{-# LANGUAGE DeriveDataTypeable #-}

import Control.Concurrent
import Data.List
import Data.Monoid
import XMonad
import XMonad.Actions.PhysicalScreens
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.OneBig
import XMonad.Layout.ResizableTile
import XMonad.Util.Cursor
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Run
import XMonad.Util.Timer
import qualified XMonad.Util.ExtensibleState as XS


--{{{ Necessary variables to calculate dzen2 bar dimensions
resX = 1366
resY = 768
--}}}



--{{{ The Main Function
main = do
    let font = "'-*-*-*-*-*-*-10-*-*-*-*-*-*-*'"
    let height = "12"
    tlBar <- spawnPipe  $  "dzen2 -dock "
                        ++ "-x 0 -y 0 "
                        ++ "-w " ++  show (resX `div` 4) ++ " "
                        ++ "-h " ++ height ++ " "
                        ++ "-ta l "
                        ++ "-fn " ++ font ++ " "
                        ++ "-fg #000000 "
                        ++ "-bg #ffffff "
                        ++ "-e onstart=lower -p"
    tcBar <- spawnPipe  $  "dzen2 -dock "
                        ++ "-x " ++ show (resX `div` 4) ++ " "
                        ++ "-y 0 "
                        ++ "-w " ++ show ((resX `div` 4) * 2) ++ " "
                        ++ "-h " ++ height ++ " "
                        ++ "-ta c "
                        ++ "-fn " ++ font ++ " "
                        ++ "-fg #000000 "
                        ++ "-bg #ffffff "
                        ++ "-e onstart=lower "
                        ++ "-p"
    trBar <- spawnPipe  $  "dzen2 -dock "
                        ++ "-x " ++ show ((resX `div` 4 ) * 3) ++ " "
                        ++ "-y 0 "
                        ++ "-w " ++ show ((resX `div` 4) + 2) ++ " "
                        ++ "-h " ++ height ++ " "
                        ++ "-ta r "
                        ++ "-fn " ++ font ++ " "
                        ++ "-fg #000000 "
                        ++ "-bg #ffffff "
                        ++ "-e onstart=lower "
                        ++ "-p"
    xmonad $ docks def
        {
          modMask               = mod4Mask
        , terminal              = "gnome-terminal"
        , normalBorderColor     = colorBlack
        , focusedBorderColor    = colorWhite
        , borderWidth           = 1
        , startupHook           = defaultStartupHook
        , layoutHook            = defaultLayoutHook
        , handleEventHook       = defaultHandleEventHook
        , logHook               =
            tlHook tlBar
        <+> tcHook tcBar
        <+> trHook trBar
        } `additionalKeys` addKeys
--}}}



--{{{ Keyboard Shortcuts
addKeys =
    [ ((modM .|. controlMask, xK_l), spawn lock)
    , ((modM .|. controlMask, xK_s), sendMessage ToggleStruts)
    , ((modM, xK_p)                , spawn menu)
    , ((0, 0x1008FF11), spawn "amixer -q set Master 2%-")
    , ((0, 0x1008FF12), spawn "amixer -D pulse set Master Playback Switch toggle")
    , ((0, 0x1008FF13), spawn "amixer -q set Master 2%+")
    , ((0, 0x1008FFB2), spawn "amixer set Capture toggle")
    ] ++
    --{{{ Multiple Screens
    [ ((modM, xK_w), viewScreen def 0)
    , ((modM, xK_e), viewScreen def 1)
    , ((modM .|. shiftMask, xK_w), sendToScreen def 0)
    , ((modM .|. shiftMask, xK_e), sendToScreen def 1)
    , ((modM, xK_a), spawn extendToVGA)
    , ((modM, xK_s), spawn mirrorToVGA)
    , ((modM, xK_d), spawn disableVGA)
    ---}}}
    ]
    where
        modM = mod4Mask
        lock = "gnome-screensaver-command -l & sleep 3 && xset dpms force off"
        menu = "dmenu_run"
        extendToVGA = "/usr/bin/xrandr --output VGA-1 --auto && /usr/bin/xrandr --output VGA-1 --right-of LVDS-1"
        mirrorToVGA = "/usr/bin/xrandr --output VGA-1 --auto && /usr/bin/xrandr --output VGA-1 --same-as LVDS-1"
        disableVGA = "/usr/bin/xrandr --output VGA-1 --off"
--}}}



--{{{ Colors & Appearance
colorBlack  = "#000000"
colorGreen  = "#00A000"
colorGray   = "#404040"
colorRed    = "#A00000"
colorYellow = "#D0A000"
colorWhite  = "#ffffff"
--}}}



--{{{ Startup Hooks
defaultStartupHook =
        (setDefaultCursor xC_left_ptr)
    <+> (spawn "/usr/bin/feh --bg-scale /usr/share/backgrounds/warty-final-ubuntu.png")
    <+> (liftIO $ threadDelay 1000000)
    <+> (startTimer 1 >>= XS.put . TID)
--}}}



--{{{ Layout Hook
defaultLayoutHook =
    avoidStruts $
        defaultTiled
    ||| defaultFull
    ||| defaultOnebig
    where
        defaultTiled   = ResizableTall 1 0.05 0.5 []
        defaultFull    = noBorders Full
        defaultOnebig  = OneBig 0.75 0.75
--}}}



--{{{ Log Hooks
tlHook x = dynamicLogWithPP $ def
    { ppOrder              = \(a:_:_:x) -> [a] ++ x
    , ppOutput             = hPutStrLn x
    , ppHiddenNoWindows    = (\x -> "^fg(" ++ colorGray ++ ")" ++ x)
    , ppHidden             = (\x -> "^fg(" ++ colorWhite ++ ")" ++ x)
    , ppCurrent            = (\x -> "^fg(" ++ colorWhite ++ ")[" ++ x ++ "]")
    , ppVisible            = (\x -> "^fg(" ++ colorWhite ++ ")" ++ x)
    , ppVisibleNoWindows   = Just (\x -> "^fg(" ++ colorGray ++ ")" ++ x)
    , ppExtras             = [defaultLayoutL]
    }
tcHook x = dynamicLogWithPP $ def
    { ppOrder     = \(_:_:_:x) -> x
    , ppOutput    = hPutStrLn x
    , ppExtras    = [defaultTitleL]
    }
trHook x = dynamicLogWithPP $ def
    { ppOrder     = \(_:_:_:x) -> x
    , ppOutput    = hPutStrLn x
    , ppSep       = "^fg(" ++ colorGray ++ ")|^fg()"
    , ppExtras    = [tempL, batteryL, dropboxL, wifiLevelL, defaultDateL]
    }
--}}}



--{{{ Loggers
defaultDateL :: Logger
defaultDateL = date $ "%A, ^fg()%d^fg().^fg(" ++ colorGray ++ ")%m.%Y "
    ++ "^fg()%H^fg():^fg()%M^fg(" ++ colorGray ++ "):%S^fg()"

defaultLayoutL :: Logger
defaultLayoutL = onLogger format logLayout where
    format s = rename $ (words s) !! 0
    rename s
        | s == "OneBig"         = "M/S"
        | s == "ResizableTall"  = "Tiled"
        | s == "Full"           = "Fullscreen"
        | otherwise             = s

defaultTitleL :: Logger
defaultTitleL = shortenL 100 logTitle

defaultWorkspaceL :: Logger
defaultWorkspaceL = logCurrent

wifiLevelL :: Logger
wifiLevelL = onLogger format $ logCmd "awk 'NR==3 {print substr($4, 1,3)}' /proc/net/wireless" where
    format x
        | nx x > -67    = "^fg(" ++ colorGray ++ ")" ++ x ++ "dBm"
        | otherwise     = "^fg(" ++ colorRed ++ ")" ++ x ++ "dBm"
    nx x = read x :: Integer

dropboxL :: Logger
dropboxL = onLogger format $ logCmd "/usr/bin/dropbox status" where
    format x
        | x == "Dropbox isn't running!"     = ""
        | x == "Up to date"                 = "^fg(" ++ colorGreen ++ ")❖^fg()"
        | otherwise                         = "^fg(" ++ colorYellow ++ ")❖^fg()"

batteryL :: Logger
batteryL = onLogger format $ logCmd "cat /sys/class/power_supply/BAT0/capacity" where
    format x
        | nx x > 25   = "^fg()" ++ x ++ "%"
        | nx x > 10   = "^fg(" ++ colorYellow ++ ")" ++ x ++ "%"
        | otherwise   = "^fg(" ++ colorRed ++ ")" ++ x
    nx x = read x :: Integer

tempL :: Logger
tempL = onLogger format $ logCmd "cat /sys/class/thermal/thermal_zone1/temp" where
    format x
        | nx x > 70 = "^fg(" ++ colorYellow ++ ")" ++ show (nx x) ++ "°"
        | nx x > 80 = "^fg(" ++ colorRed ++ ")" ++ show (nx x) ++ "°"
        | otherwise = ""
    nx x = (read x :: Integer) `div` 1000
--}}}



--{{{ Handle Event Hook
data TidState = TID TimerId deriving Typeable
instance ExtensionClass TidState where
    initialValue = TID 0

defaultHandleEventHook = clockEventHook

clockEventHook e = do
    (TID t) <- XS.get
    handleTimer t e $ do
        startTimer 1 >>= XS.put . TID
        ask >>= logHook.config
        return Nothing
    return $ All True
--}}}
