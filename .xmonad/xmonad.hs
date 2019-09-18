import XMonad hiding( (|||) )
import XMonad.Util.EZConfig
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutModifier
import qualified Data.Map as M
import qualified XMonad.StackSet as W
    

myTerminal = "termite"
myModmask = mod4Mask  -- win-key
myBW = 2
myFBC = "orange"
--myFBC = "#ff0000"

myXmobarrc = "~/.xmonad/xmobarrc.hs"

-- hide xmobar with mod+b. keycodes 
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- Key-codes can be found in: '/usr/include/X11/keysymdef.h'
myKeys = [
  ((mod4Mask, xK_l), spawn "xscreensaver-command -lock"),
  ((mod4Mask, xK_u), spawn "$HOME/xmonad-update.sh"),
  ((0, xK_Print), spawn "scrot"),

  -- Window movement
  ((mod4Mask, xK_Up), windows W.swapUp),
  ((mod4Mask, xK_f), sendMessage $ JumpToLayout "Full"),
  ((mod4Mask, xK_s), sendMessage $ JumpToLayout "Tall"), 
                                        
  -- MUSIC RELATED
  ((0, xK_Pause), spawn "mpc toggle"),
  ((mod4Mask, xK_Page_Up), spawn "mpc prev"),
  ((mod4Mask, xK_Page_Down), spawn "mpc next") 
  ]

myPP = xmobarPP {ppCurrent = xmobarColor "#ff0000" "" . wrap "{" "}"}

myManageHooks = composeAll [
  className =? "Qutebrowser"		--> doShift "1:WWW",
  className =? "mutt"        		--> doShift "8:MAIL",
  className =? "Emacs"       		--> doShift "3:TeX",
  className =? "XDvi"        		--> doCenterFloat,
  className =? "Xfig"        		--> doShift "5:PLOT",
  className =? "Zathura"     		--> doCenterFloat,
  className =? "Nm-connection-editor" 	--> doCenterFloat,
  className =? "Gnuplot"		--> doCenterFloat,
  resource  =? "music"       		--> doShift "7:MUSIC",
  isFullscreen		     		--> (doF W.focusDown <+> doFullFloat)
  ]

myWS = ["1:WWW", "2:UEB", "3:TeX", "4:PDF", "5:PLOT", "6:MISC", "7:MUSIC", "8:MAIL"]

tall = Tall 1 (3/100) (1/2)
       
myConfig = defaultConfig{
   terminal = myTerminal,
   modMask = myModmask,
   borderWidth = myBW,
   workspaces = myWS,
   focusedBorderColor = myFBC,
   manageHook = myManageHooks,
   layoutHook = tall ||| Mirror tall ||| Full
   } `additionalKeys` myKeys
     
main = xmonad =<< statusBar "xmobar" myPP toggleStrutsKey myConfig

