import System.IO
import System.Exit
import XMonad hiding( (|||) )

-- ACTIONS
import XMonad.Actions.FloatKeys
import XMonad.Actions.GridSelect

-- UTIL
import XMonad.Util.EZConfig
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run(spawnPipe, safeSpawn)
import XMonad.Util.NamedWindows (getName)

-- HOOKS
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

-- LAYOUT
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Fullscreen
import XMonad.Layout.ResizableTile

-- MISC
import Data.List (sortBy)
import Data.Function (on)
import Control.Monad (forM_, join)
import qualified Data.Map as M
import qualified XMonad.StackSet as W

{-
myStartupHook = do
  spawn "$HOME/.config/polybar/launch.sh"
-}

-- Some Colors
orange = "ffcc00"
black  = "000000"
white  = "ffffff"
blue   = "0033cc"
red    = "ff0000"

-- dark colors
dblue  = "000066"


myTerminal = "termite"
myModmask = mod4Mask  -- win-key
myBW = 2
myFBC = "orange"
myFont = "SourceCodePro-12"


{-- XMOBAR --}
    -- hide xmobar with mod+b. keycodes
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)
myXmobarrc = "~/.xmonad/xmobarrc"
myPP = xmobarPP {ppCurrent = xmobarColor "#ff0000" "" . wrap "{" "}"}


{-- USEFUL STUFF --}
myScreensaver = "/usr/bin/xscreensaver-command -l"


{-- Grid Select --}
myGSConfig = defaultGSConfig {
    gs_cellheight = 50
   ,gs_cellwidth = 250
   ,gs_cellpadding = 10
  -- ,gs_font = "" ++ myFont ++ ""
}

{-- KEY BINDINGS --}
-- Key-codes can be found in: '/usr/include/X11/keysymdef.h'
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  [((mod4Mask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
  ,((mod4Mask .|. shiftMask, xK_g), gridselectWorkspace myGSConfig W.view)

  -- Basics
  ,((mod4Mask .|. shiftMask, xK_c), kill)
  ,((mod4Mask .|. controlMask, xK_r), spawn "xmonad --recompile && xmonad --restart")
  ,((mod4Mask .|. controlMask, xK_f), setLayout $ XMonad.layoutHook conf)     -- Reset Layout

  -- This is a comment
  ,((mod4Mask, xK_l), spawn "xscreensaver-command -l")
  ,((mod4Mask, xK_u), spawn "$HOME/xmonad-update.sh")
  ,((0, xK_Print), spawn "scrot")

  -- Application Launcher
  ,((mod4Mask, xK_space), spawn "rofi -show drun -display-drun 🐧")

  -- Window manipulation
  ,((mod4Mask, xK_Up), windows W.swapUp)
  ,((mod4Mask, xK_f), sendMessage $ JumpToLayout "Full")
  ,((mod4Mask, xK_s), sendMessage $ JumpToLayout "Tall")
  ,((mod4Mask .|. controlMask, xK_d), withFocused $ windows . W.sink)         -- floating -> tiled
  ,((mod4Mask .|. controlMask, xK_h), withFocused (keysMoveWindow (-30, 0)))  -- Move window left
  ,((mod4Mask .|. controlMask, xK_l), withFocused (keysMoveWindow (30, 0)))   -- Move window right
  ,((mod4Mask .|. controlMask, xK_k), withFocused (keysMoveWindow (0, -30)))  -- Move window up
  ,((mod4Mask .|. controlMask, xK_j), withFocused (keysMoveWindow (0, 30)))   -- Move window down

  ,((mod4Mask .|. shiftMask, xK_plus), sendMessage Expand)                    -- Expand Focused Area
  ,((mod4Mask .|. shiftMask, xK_minus), sendMessage Shrink)                   -- Shrink Focused Area
  ,((mod4Mask .|. controlMask, xK_plus), sendMessage MirrorExpand)

  -- MUSIC RELATED
  ,((0, xK_Pause), spawn "mpc toggle")
  ,((mod4Mask, xK_Page_Up), spawn "mpc prev")
  ,((mod4Mask, xK_Page_Down), spawn "mpc next")
  ]
  ++
  [((m .|. mod4Mask, k), windows $ f i)    -- Move to workspaces
         | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9])
         , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]


{-- RULES FOR WINDOWS --}
myManageHooks = composeAll [
   className =? "mutt"                   --> doShift "8:MAIL"
  ,className =? "Emacs"                  --> doShift "3:TeX"

  -- System
  ,className =? "Htop"                   --> doFloat
  ,className =? "Nm-connection-editor"   --> doCenterFloat

  -- Browsers
  ,className =? "Qutebrowser"            --> doShift "1:WWW"

  -- Viewers
  ,className =? "XDvi"                   --> doCenterFloat
  ,className =? "Xfig"                   --> doShift "5:PLOT"
  ,className =? "Zathura"                --> doCenterFloat
  ,className =? "Gnuplot"                --> doCenterFloat

  -- Steam
  ,className =? "Steam"                  --> doFloat
  ,className =? "Friends List"           --> doFloat
  ,className =? "Steam News"             --> doFloat
  ,className =? "Settings"               --> doFloat

  -- MISC
  ,resource  =? "music"                  --> doShift "7:MUSIC"
  --isFullscreen                          --> (doF W.focusDown <+> doFullFloat)
  ]


{-- WORKSPACES --}
myWS = ["1:WWW", "2:UEB", "3:TeX", "4:PDF", "5:MAIL", "6:MISC", "7:MUSIC", "8:GAMES"]


{-- DEFINITIONS --}
tall = Tall 1 (3/100) (1/2)


myConfig = defaultConfig {
   terminal = myTerminal
   ,modMask = myModmask
   ,borderWidth = myBW
   ,workspaces = myWS
   ,keys = myKeys
   ,focusedBorderColor = myFBC
   ,manageHook = myManageHooks
   -- startupHook = myStartupHook,
   ,layoutHook = tall ||| Mirror tall ||| Full
   }

main = xmonad =<< statusBar "xmobar" myPP toggleStrutsKey myConfig
