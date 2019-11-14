import XMonad hiding( (|||) )

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
myFBC = orange


myXmobarrc = "~/.xmonad/xmobarrc"

-- hide xmobar with mod+b. keycodes
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- Key-codes can be found in: '/usr/include/X11/keysymdef.h'
myKeys = [
  ((mod4Mask, xK_l), spawn "xscreensaver-command -lock"),
  ((mod4Mask, xK_u), spawn "$HOME/xmonad-update.sh"),
  ((0, xK_Print), spawn "scrot"),

  -- Application Launcher
  ((mod4Mask, xK_space), spawn "rofi -show drun -display-drun :"),

  -- Window movement
  ((mod4Mask, xK_Up), windows W.swapUp),
  ((mod4Mask, xK_f), sendMessage $ JumpToLayout "Full"),
  ((mod4Mask, xK_s), sendMessage $ JumpToLayout "Tall"),

  -- MUSIC RELATED
  ((0, xK_Pause), spawn "mpc toggle"),
  ((mod4Mask, xK_Page_Up), spawn "mpc prev"),
  ((mod4Mask, xK_Page_Down), spawn "mpc next")
  ]

myManageHooks = composeAll [
  className =? "Qutebrowser"            --> doShift "1:WWW",
  className =? "mutt"                   --> doShift "8:MAIL",
  className =? "Emacs"                  --> doShift "3:TeX",
  className =? "XDvi"                   --> doCenterFloat,
  className =? "Xfig"                   --> doShift "5:PLOT",
  className =? "Zathura"                --> doCenterFloat,
  className =? "Nm-connection-editor"   --> doCenterFloat,
  className =? "Gnuplot"                --> doCenterFloat,

  -- Steam
  className =? "Friends List"           --> doCenterFloat,
  className =? "Steam News"             --> doCenterFloat,
  className =? "Settings"               --> doCenterFloat,

  resource  =? "music"                  --> doShift "7:MUSIC",
  isFullscreen                          --> (doF W.focusDown <+> doFullFloat)
  ]

myWS = ["1:WWW", "2:UEB", "3:TeX", "4:PDF", "5:MAIL", "6:MISC", "7:MUSIC", "8:GAMES"]

tall = Tall 1 (3/100) (1/2)


myConfig = defaultConfig {
   terminal = myTerminal,
   modMask = myModmask,
   borderWidth = myBW,
   workspaces = myWS,
   focusedBorderColor = myFBC,
   manageHook = myManageHooks,
   -- startupHook = myStartupHook,
   layoutHook = tall ||| Mirror tall ||| Full
   } `additionalKeys` myKeys

myPP = xmobarPP {ppCurrent = xmobarColor "#ff0000" "" . wrap "{" "}"}


main = xmonad =<< statusBar "xmobar" myPP toggleStrutsKey myConfig
