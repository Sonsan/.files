import XMonad
import XMonad.Util.EZConfig
import XMonad.Util.Scratchpad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog

main = xmonad =<< statusBar "xmobar" myPP toggleStrutsKey myConfig

myConfig = defaultConfig{
   terminal = myTerminal,
   modMask = myModmask,
   borderWidth = myBW,
   workspaces = myWS,
   focusedBorderColor = myFBC
   }


-- color & indicator of the active workspace
myPP = xmobarPP {ppCurrent = xmobarColor "#ff0000" "" . wrap "{" "}"}

-- hide xmobar with mod+b. keycodes in '/usr/include/X11/keysymdef.h'
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myWS = ["WWW", "CODE", "COMPILE", "MISC", "XMONAD", "LATEX", "MUSIC"]

myTerminal = "xterm -fg white -bg black"
myModmask = mod4Mask  -- win-key
myBW = 2
myFBC = "#ff0000"
