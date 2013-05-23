import XMonad
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog[M M3
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run

myBorderWidth        = 1
myFocusedBorderColor = "black"
myModMask            = mod4Mask
myNormalBorderColor  = "black"
myNumlockMask        = mod2Mask
myTerminal           = "gnome-terminal"
myWorkspaces         = ["1","2","3","4"]
myFocusFollowsMouse = False

myManageHook = composeAll
    [ className =? "Vlc"            --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "GNU Image Manipulation" --> doFloat
    ]

-----------------------------------------------------------------------

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
      terminal           = myTerminal
    , manageHook         = manageDocks <+> myManageHook <+> manageHook defaultConfig
    , focusFollowsMouse  = myFocusFollowsMouse
    , borderWidth        = myBorderWidth
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , logHook            = updatePointer (Relative 0.5 0.5) <+> dynamicLogWithPP xmobarPP { ppOutput = hPutStrLn xmproc, ppTitle = xmobarColor "black" "" . shorten 50 }
    , layoutHook         = avoidStruts $ layoutHook defaultConfig
}