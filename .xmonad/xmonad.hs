import XMonad
import XMonad.Util.EZConfig
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import qualified Data.Map as M
import qualified XMonad.StackSet as W

{-
import System.IO
import System.Exit
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
-}

{-
=================
 BASIC CONFIG
=================
-}
main = xmonad =<< statusBar "xmobar" myPP toggleStrutsKey myConfig

myConfig = defaultConfig{
   terminal = myTerminal,
   modMask = myModmask,
   borderWidth = myBW,
   workspaces = myWS,
   focusedBorderColor = myFBC
   } `additionalKeys` myKeys


{-
==========================
  MISC SETTINGS & PATHS
==========================
-}
myTerminal = "termite"
myModmask = mod4Mask  -- win-key
myBW = 2
myFBC = "orange"
--myFBC = "#ff0000"

myXmobarrc = "~/.xmonad/xmobar-default.hs"

-- hide xmobar with mod+b. keycodes 
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

             
{-
==========================================================
  KEYBINDINGS
    /_ Key-codes can be found in: '/usr/include/X11/keysymdef.h'
==========================================================
-}
myKeys = [
 	((mod4Mask, xK_l), spawn "xscreensaver-command -lock"),
        ((mod4Mask, xK_u), spawn "$HOME/xmonad-update.sh"),
        ((0, xK_Print), spawn "scrot")
         ]


-- color & indicator of the active workspace
myPP = xmobarPP {ppCurrent = xmobarColor "#ff0000" "" . wrap "{" "}"}


{-
=====================
  MANAGING HOOKS
=====================
-}
myManageHooks = composeAll
                [
                 className =? "Chromium"	--> doShift "WWW",
                 className =? "Qutebowser"	--> doShift "WWW"
                ]

{-
======================
  WORKSPACE NAMES
======================
 -}
myWS = ["WWW", "CODE", "COMPILE", "MISC", "XMONAD", "LATEX", "MUSIC", "VPN"]

       
{-
  TODO: SCRATCHPAD 
-}
-- myScratchpads = [NS "htop" "xterm -e htop" (title =? "htop") defaultFloating]

