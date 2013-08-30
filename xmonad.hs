import Data.Ratio ((%))
import System.IO
import System.Exit

import XMonad
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import XMonad.Util.Run
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.CycleWS

import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.IM
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Grid

import qualified XMonad.StackSet as W
import qualified Data.Map as M


colorForeground    = "#eee8d5"
colorBackground    = "#1b1d1e"
colorCyan          = "#2aa198"
colorYellow        = "#b58900"
colorOrange        = "#cb4b16"
colorRed           = "#dc322f"
colorDarkGray      = "#586e75"
colorPink          = "#d33682"
colorGreen         = "#859900"
colorBlue          = "#268bd2"
colorViolet        = "#6c71c4"
colorWhite         = "#ccccc6"
colorNormalBorder  = "#073642"
colorFocusedBorder = colorYellow

statusBarFont      = "Inconsolata:bold:size=10"
terminalProgram    = "gnome-terminal"

modMask' :: KeyMask
modMask' = mod4Mask

spaces              = ["1:main","2:web","3:vim","4:chat","5:music","6:misc"]

xmonadBarCmd = "dzen2 -x '0' -y '0' -h '24' -w '960' -ta 'l' -fg '" ++
    colorForeground ++
    "' -bg '" ++
    colorBackground ++
    "' -fn '" ++ statusBarFont ++ "'"

statusBarCmd = "conky -c /home/mdomke/.xmonad/conkyrc | " ++
    "dzen2 -y '0' -x '960' -w '960' -h '24' -ta 'r' -bg '" ++
    colorBackground ++
    "' -fg '" ++
    colorForeground ++
    "' -fn '" ++ statusBarFont ++ "'"

spotifyBarCmd = "dzspotify | dzen2 -x 1000 -h 24 -w 420 -ta c -fg '" ++
    colorCyan  ++
    "' -bg '" ++
    colorBackground  ++
    "' -fn '" ++ statusBarFont ++ "'"

main = do
    dzenLeftBar   <- spawnPipe xmonadBarCmd
    dzenRightBar  <- spawnPipe statusBarCmd
    dzenCenterBar <- spawnPipe spotifyBarCmd
    xmonad $ withUrgencyHookC
                dzenUrgencyHook { args = ["-bg", colorRed, "fg", colorBackground, "-xs", "1", "-y", "25"] }
                urgencyConfig { remindWhen = Every 15 }
           $ defaultConfig {
                 terminal            = terminalProgram
               , workspaces          = spaces
               , keys                = keys'
               , modMask             = modMask'
               , startupHook         = setWMName "LG3D"
               , layoutHook          = layoutHook'
               , manageHook          = manageHook'
               , logHook             = logHandler dzenLeftBar
               , normalBorderColor   = colorNormalBorder
               , focusedBorderColor  = colorFocusedBorder
               , borderWidth         = 2
           }


manageHook' :: ManageHook
manageHook' = (composeAll . concat $
    [ [resource  =? r --> doIgnore           | r <- myIgnores] -- ignore desktop
    , [className =? c --> doShift  "1:main"  | c <- myDev    ] -- move dev to main
    , [className =? c --> doShift  "2:web"   | c <- myWebs   ] -- move webs to main
    , [className =? c --> doShift  "3:vim"   | c <- myVim    ] -- move webs to main
    , [className =? c --> doShift  "4:chat"  | c <- myChat   ] -- move chat to chat
    , [className =? c --> doShift  "5:music" | c <- myMusic  ] -- move music to music
    , [className =? c --> doCenterFloat      | c <- myFloats ] -- float my floats
    ])
    where
        role      = stringProperty "WM_WINDOW_ROLE"
        name      = stringProperty "WM_NAME"
        myFloats  = ["VirtualBox","Xmessage","XFontSel"]
        myWebs    = ["Google-chrome"]
        myMusic	  = ["Spotify"]
        myChat	  = ["Pidgin","Buddy List", "Empathy"]
        myDev	  = ["gnome-terminal"]
        myVim	  = ["Gvim"]
        myIgnores = ["desktop","desktop_window","notify-osd","stalonetray","trayer"]


bitmapsDir = "/home/mdomke/.xmonad/bitmaps"
logHandler :: Handle -> X ()
logHandler h = dynamicLogWithPP $ defaultPP {
    ppCurrent         = dzenColor colorYellow     colorBackground . pad
  , ppVisible         = dzenColor colorForeground colorBackground . pad
  , ppHidden          = dzenColor colorForeground colorBackground . pad
  , ppHiddenNoWindows = dzenColor colorDarkGray   colorBackground . pad
  , ppUrgent          = dzenColor colorBackground colorRed . pad
  , ppWsSep           = " "
  , ppSep             = "  |  "
  , ppLayout          = dzenColor colorYellow colorBackground .
        (\x -> case x of
         "ResizableTall"        -> "^i(" ++ bitmapsDir ++ "/tall.xbm)"
         "Mirror ResizableTall" -> "^i(" ++ bitmapsDir ++ "/mtall.xbm)"
         "Full"                 -> "^i(" ++ bitmapsDir ++ "/full.xbm)"
         "ReflectX IM Grid"     -> "IM"
         "Simple Float"         -> "~"
         _                      -> x)
  , ppTitle           = (" " ++) . dzenColor colorForeground colorBackground . dzenEscape
  , ppOutput          = hPutStrLn h
}


layoutHook'   = avoidStruts $ onWorkspaces ["4:chat"] imLayout $ defaultLayout
tiledLayout   = ResizableTall 1 (2/100) (60/100) []
defaultLayout = avoidStruts $ tiledLayout ||| Mirror tiledLayout ||| Full ||| simpleFloat
imLayout      = reflectHoriz $ withIM (1%5) (Or (Title "Buddy List") (Title "Contact List")) Grid


applicationMenu :: XPConfig
applicationMenu =
    defaultXPConfig {
        font              = "xft: " ++ statusBarFont
      , bgColor           = colorBackground
      , fgColor           = colorBlue
      , bgHLight          = colorBlue
      , fgHLight          = colorBackground
      , borderColor       = colorNormalBorder
      , promptBorderWidth = 1
      , height            = 24
      , historyFilter     = deleteConsecutive
    }


keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask,               xK_r     ), runOrRaisePrompt applicationMenu)
    , ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask .|. shiftMask, xK_c     ), kill)
    , ((modMask .|. shiftMask, xK_q     ), spawn "gnome-session-quit --logout")
    , ((modMask,               xK_q     ), spawn "/usr/bin/xmonad --recompile && /usr/bin/xmonad --restart")
    , ((modMask .|. shiftMask, xK_l     ), spawn "gnome-screensaver-command -l")
    , ((0,                     xK_Print ), spawn "scrot -e 'mv $f ~/screenshots/'")
    , ((modMask,		       xK_o     ), spawn "google-chrome")
    , ((modMask,               xK_m     ), spawn "nautilus --no-desktop --browser")
    , ((modMask,               xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf) -- reset layout on current desktop to default
    , ((modMask,               xK_b     ), sendMessage ToggleStruts)
    , ((modMask,               xK_n     ), refresh)
    , ((modMask,               xK_Tab   ), windows W.focusDown)                -- move focus to next window
    , ((modMask,               xK_j     ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp  )
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown)                 -- swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp)                   -- swap the focused window with the previous window
    , ((modMask,               xK_Return), windows W.swapMaster)
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)     -- Push window back into tiling
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)
    , ((modMask,               xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask,               xK_period), sendMessage (IncMasterN (-1)))
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
          | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
          , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
          | (key, sc) <- zip [xK_w, xK_e] [0, 1]
          , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-- vim:foldmethod=marker sw=4 sts=4 ts=4 tw=0 et ai nowrap
