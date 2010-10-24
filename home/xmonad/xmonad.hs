import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Hooks.ManageHelpers
import XMonad.Actions.CycleWS
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Actions.NoBorders
import XMonad.Actions.DynamicWorkspaces
import qualified XMonad.StackSet as W
import XMonad.Util.Run
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowGo
import Monad
import Data.Monoid (All (All))
import System.IO

myWorkspaces  = ["SHELL", "FIREFOX", "MAIL", "4", "5", "6", "7", "8", "9"]

myManageHook = composeAll
    [
        className =? "Firefox" --> doShift "FIREFOX"
        , title =? "Preferenze di Firefox" --> doCenterFloat
        , title =? "Componenti aggiuntivi" --> doCenterFloat
        , title =? "Download" --> doCenterFloat
        , title =? "Copio i file" --> doCenterFloat
        , title =? "Sposto i file" --> doCenterFloat
        , title =? "Elimino i file" --> doCenterFloat
        , title =? "Rinomina file" --> doCenterFloat
        , className =? "thunderbird-bin" --> doShift "MAIL"
        , className =? "Mozilla Thunderbird" --> doShift "MAIL"
        , className =? "Gnome-mplayer" --> doFloat
        , className =? "VCLSalFrame" --> doFloat
        , className =? "MPlayer" --> doFullFloat
        , isFullscreen --> doFullFloat
        , className =? "Gimp" --> doNewWS "gimp"
    ]
    where
        doNewWS tg = (liftX $ addUniqueHiddenWS tg) >> doShift tg
        addUniqueHiddenWS tg = withWindowSet $ \s -> if null (filter ( (== tg) . W.tag) (W.workspaces s))
                                                     then addWorkspace tg
                                                     else return ()
main = do
    xmproc <- spawnPipe "/usr/bin/xmobar"
    xmonad $ defaultConfig
        { borderWidth = 1
        , terminal = "urxvt"
        , workspaces = myWorkspaces
        , manageHook = manageDocks <+> myManageHook
        , layoutHook = onWorkspace "gimp" (avoidStruts gimp) (avoidStruts $ layoutHook defaultConfig)
        , logHook = dynamicLogWithPP $ xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , startupHook = do
            spawn "~/.start_trayer.sh"
        , modMask = mod4Mask
        , normalBorderColor = "#000000"
        , focusedBorderColor = "#555555"
        , handleEventHook = evHook

        } `additionalKeys`
        [  ((mod1Mask, xK_a), spawn "urxvtc")
         , ((mod1Mask, xK_r), spawn "gmrun")
         , ((mod1Mask, xK_n), spawn "emesene")
         , ((mod1Mask, xK_m), spawn "thunderbird-bin")
         , ((mod1Mask, xK_f), spawn "firefox")
         , ((mod4Mask, xK_s), spawn "upscreen -G")
         , ((mod1Mask, xK_e), spawn "urxvt -e alsamixer")
         , ((mod1Mask, xK_l), spawn "xlock")
         , ((mod1Mask, xK_p), spawn "pcmanfm")
         , ((mod4Mask, xK_Right), nextWS)
         , ((mod4Mask, xK_Left), prevWS)
         , ((mod4Mask .|. shiftMask, xK_Right), shiftToNext)
         , ((mod4Mask .|. shiftMask, xK_Left), shiftToPrev)
         , ((mod4Mask, xK_f), withFocused toggleBorder)
         , ((mod4Mask, xK_x), sendMessage ToggleStruts)
         , ((mod4Mask .|. shiftMask, xK_BackSpace), removeWorkspace)
        ]
        where
            gimp = withIM (0.11) (Role "gimp-toolbox") $ reflectHoriz $
                   withIM (0.15) (Role "gimp-dock") Full


-- Helper functions to fullscreen the window
fullFloat, tileWin :: Window -> X ()
fullFloat w = windows $ W.float w r
    where r = W.RationalRect 0 0 1 1
tileWin w = windows $ W.sink w

evHook :: Event -> X All
evHook (ClientMessageEvent _ _ _ dpy win typ dat) = do
  state <- getAtom "_NET_WM_STATE"
  fullsc <- getAtom "_NET_WM_STATE_FULLSCREEN"
  isFull <- runQuery isFullscreen win

  -- Constants for the _NET_WM_STATE protocol
  let remove = 0
      add = 1
      toggle = 2

      -- The ATOM property type for changeProperty
      ptype = 4 

      action = head dat

  when (typ == state && (fromIntegral fullsc) `elem` tail dat) $ do
    when (action == add || (action == toggle && not isFull)) $ do
         io $ changeProperty32 dpy win state ptype propModeReplace [fromIntegral fullsc]
         fullFloat win
    when (head dat == remove || (action == toggle && isFull)) $ do
         io $ changeProperty32 dpy win state ptype propModeReplace []
         tileWin win

  -- It shouldn't be necessary for xmonad to do anything more with this event
  return $ All False

evHook _ = return $ All True
