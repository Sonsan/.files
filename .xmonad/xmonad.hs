import System.IO
import System.Exit
import XMonad hiding( (|||) )

-- ACTIONS
import XMonad.Actions.FloatKeys
import XMonad.Actions.GridSelect
import XMonad.Actions.Minimize
import XMonad.Actions.Submap
import qualified XMonad.Actions.FlexibleResize as Flex

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
import XMonad.Hooks.UrgencyHook
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
  spawn "$HOME/.config/polybar/launch.sh"  -- Launch Polybar
-}

myStartupHook = return()

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
  [((mod4Mask .|. shiftMask, xK_Return)        ,spawn $ XMonad.terminal conf)
  ,((mod4Mask .|. shiftMask, xK_g)             ,gridselectWorkspace myGSConfig W.view)

  -- Basics
  ,((mod4Mask, xK_space)                       ,sendMessage NextLayout)                           -- next tiling style
  ,((mod4Mask, xK_Tab)                         ,windows W.focusDown)                              -- go to next window
  ,((mod4Mask .|. shiftMask, xK_c)             ,kill)                                             -- Close current window
  ,((mod4Mask .|. shiftMask, xK_Tab)           ,windows W.focusUp)                                -- go to prev window
  ,((mod4Mask .|. controlMask, xK_f)           ,setLayout $ XMonad.layoutHook conf)               -- Reset Layout

  -- MISC
  ,((mod4Mask .|. controlMask, xK_r)           ,spawn "xmonad --recompile && xmonad --restart")   -- Recompile & Restart Xmonad
  ,((mod4Mask, xK_l)                           ,spawn "xscreensaver-command -l")
  ,((0, xK_Print)                              ,spawn "scrot '%Y-%m-%d_$wx$h.png' -e 'mv $f ~/Pictures/Screenshots/'")

  -- Application Launcher
  ,((mod4Mask, xK_space)                       ,spawn "rofi -show drun")
  ,((mod4Mask .|. shiftMask, xK_space)         ,spawn "rofi -modi file-browser -show file-browser -file-browser-disable-status -file-browser-show-hidden")
  ,((mod4Mask .|. controlMask, xK_space)       ,spawn "rofi -modi file-browser -show file-browser -file-browser-disable-status -file-browser-dir '/home/nils/Documents/University'")

  -- Window manipulation
  ,((mod4Mask, xK_Up)                          ,windows W.swapUp)
  ,((mod4Mask, xK_f)                           ,sendMessage $ JumpToLayout "Full")
  ,((mod4Mask, xK_s)                           ,sendMessage $ JumpToLayout "Tall")
  ,((mod4Mask .|. controlMask, xK_d)           ,withFocused $ windows . W.sink)                      -- floating -> tiled
  ,((mod4Mask .|. controlMask, xK_h)           ,withFocused (keysMoveWindow (-20, 0)))               -- Move window left
  ,((mod4Mask .|. controlMask, xK_l)           ,withFocused (keysMoveWindow (20, 0)))                -- Move window right
  ,((mod4Mask .|. controlMask, xK_k)           ,withFocused (keysMoveWindow (0, -20)))               -- Move window up
  ,((mod4Mask .|. controlMask, xK_j)           ,withFocused (keysMoveWindow (0, 20)))                -- Move window down

  ,((mod4Mask .|. shiftMask, xK_plus)          ,sendMessage Expand)                                  -- Expand Focused Area
  ,((mod4Mask .|. shiftMask, xK_minus)         ,sendMessage Shrink)                                  -- Shrink Focused Area

  -- Window Size
  ,((mod4Mask .|. controlMask, xK_plus)        ,withFocused (keysAbsResizeWindow (10, 10) (0, 1)))   -- bigger (all sides)
  ,((mod4Mask .|. controlMask, xK_minus)       ,withFocused (keysAbsResizeWindow (-10, -10) (1, 0))) -- smaller (all sides)
  ,((mod4Mask .|. mod1Mask, xK_l)              ,withFocused (keysAbsResizeWindow (10, 0) (1, 0)))    -- bigger (right side)
  ,((mod4Mask .|. mod1Mask, xK_j)              ,withFocused (keysAbsResizeWindow (0, 10) (1, 0)))    -- bigger (down)


  ,((mod4Mask .|. shiftMask, xK_r)             ,withFocused (keysMoveWindowTo (1280, 0) (1, 0)))     -- move window to top right
  ,((mod4Mask .|. shiftMask, xK_l)             ,withFocused (keysMoveWindowTo (0, 0) (0, 0)))

  ,((mod4Mask .|. controlMask, xK_m)           ,withFocused minimizeWindow)                          -- minimize window
  ,((mod4Mask .|. shiftMask,   xK_m)           ,withFocused maximizeWindow)                          -- maximize window

  -- Scratchpads
  ,((mod4Mask .|. shiftMask, xK_s), submap . M.fromList $ [
        ((0, xK_t)      ,namedScratchpadAction myScratchpads "htop")
       ,((0, xK_p)      ,namedScratchpadAction myScratchpads "gnuplot")
       ,((0, xK_u)      ,namedScratchpadAction myScratchpads "units")
       ,((0, xK_m)      ,namedScratchpadAction myScratchpads "neomutt")
       ,((0, xK_a)      ,namedScratchpadAction myScratchpads "alsamixer")
       ,((0, xK_Return) ,namedScratchpadAction myScratchpads "terminal")
  ])

  -- MUSIC RELATED
  ,((0, xK_Pause)                              ,spawn "mpc toggle")
  ,((mod4Mask, xK_Page_Up)                     ,spawn "mpc prev")
  ,((mod4Mask, xK_Page_Down)                   ,spawn "mpc next")
  --,((0, xF86xk_AudioMute)                      ,spawn "amixer -q set Master toggle")
  --,((0, xF86xk_AudioLowerVolume)               ,spwan "amixer -q set Master 5%-")
  --,((0, xF86xk_AudioRaiseVolume)               ,spawn "amixer -q set Master 5%+")
  ]
  ++
  [((m .|. mod4Mask, k), windows $ f i)    -- Move to workspaces
         | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9])
         , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

{-- MOUSE BINDINGS --}
myMouse (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    -- button1 = right ; button2 = left ; button3 = middle ; button4 = scrollUp ; button5 = scrollDown
     ((mod4Mask,button1)                  ,(\w -> focus w >> mouseMoveWindow w))     -- make window float and move via drag
    ,((mod4Mask .|. shiftMask, button1)   ,(\w -> focus w >> mouseResizeWindow w))   -- resize window
    ,((mod4Mask, button3)                 ,(\w -> focus w >> windows W.swapMaster))  -- raise window to top of stack
  ]

{-- SCRATCHPADS --}
myScratchpads = [
    NS  "htop"       "termite -e htop"       (title =? "htop")        defaultFloating
   ,NS  "gnuplot"    "termite -e gnuplot"    (title =? "gnuplot")     defaultFloating
   ,NS  "units"      "termite -e units"      (title =? "units")       defaultFloating
   ,NS  "neomutt"    "termite -e neomutt"    (title =? "neomutt")     defaultFloating
   ,NS  "alsamixer"  "termite -e alsamixer"  (title =? "alsamixer")   defaultFloating
   ,NS  "terminal"    spawnTerminal          (title =? "scratchpad")  termSettings
  ] where
  spawnTerminal = myTerminal ++ " -t 'scratchpad' -c '/home/nils/.config/termite/scratch_config'"
  termSettings = customFloating $ W.RationalRect l t w h
    where
      h = 0.2
      w = 0.8
      t = 0
      l = (1-w)/2


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

  -- Games
  ,className =? "Steam"                  --> doFloat

  -- Chat
  ,className =? "Telegram"               --> doCenterFloat
  ,className =? "Media Viewer"           --> doCenterFloat

  -- MISC
  ,resource  =? "music"                  --> doShift "7:MUSIC"
  ,namedScratchpadManageHook myScratchpads
  ]


{-- WORKSPACES --}
myWS = ["1:WWW", "2:UeB", "3:TeX", "4:PDF", "5:MAIL", "6:MISC", "7:MUSIC", "8:GAMES"]


{-- DEFINITIONS --}
tall = Tall 1 (3/100) (1/2)


{-- NOTIFICATIONS --}
-- TODO

myConfig = defaultConfig {
   terminal                       = myTerminal
   ,modMask                       = myModmask
   ,borderWidth                   = myBW
   ,workspaces                    = myWS
   ,keys                          = myKeys
   ,mouseBindings                 = myMouse
   ,focusedBorderColor            = myFBC
   ,manageHook                    = myManageHooks
   ,startupHook                   = myStartupHook
   ,layoutHook                    = tall ||| Mirror tall ||| Full
   }

main = xmonad =<< statusBar "xmobar" myPP toggleStrutsKey myConfig
