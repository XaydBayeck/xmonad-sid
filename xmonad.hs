{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Data.Map as M
import Data.Monoid ()
import System.Exit ()
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks
  ( AvoidStruts,
    Direction2D (D, L, R, U),
    avoidStruts,
    docks,
    manageDocks,
  )
import XMonad.Hooks.ManageHelpers (doFullFloat, isDialog, isFullscreen)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.Decoration (ModifiedLayout)
import XMonad.Layout.Gaps
  ( GapMessage (DecGap, IncGap, ToggleGaps),
    Gaps,
    gaps,
    setGaps,
  )
import XMonad.Layout.NoBorders (SmartBorder, smartBorders)
import XMonad.Layout.Spacing (Border (Border), Spacing, spacingRaw)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Loggers (logTitles)
import XMonad.Util.SpawnOnce (spawnOnce)

type m $ a = m a

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal :: String
myTerminal = "alacritty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth :: Dimension
myBorderWidth = 4

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask :: KeyMask
myModMask = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces :: [String]
myWorkspaces = ["\63083", "\63288", "\63306", "\61723", "\63107", "\63601", "\63391", "\61713", "\61884"]

-- myWorkspaces = ["\63083", "\63288", "\63306", "\61723", "\63107", "\63601"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor :: String
myNormalBorderColor = "#3b4252"

myFocusedBorderColor :: String
myFocusedBorderColor = "#bc96da"

-- addNETSupported :: Atom -> X ()
-- addNETSupported x = withDisplay $ \dsplay -> do
--   root <- asks theRoot
--   netSupported <- getAtom "_NET_SUPPORTED"
--   atom <- getAtom "ATOM"
--   liftIO $ do
--     sup <- join . maybeToList <$> getWindowProperty32 dsplay netSupported root
--     when (fromIntegral x `notElem` sup) $
--       changeProperty32 dsplay root netSupported atom propModeAppend [fromIntegral x]
--
-- addEWMHFullscreen :: X ()
-- addEWMHFullscreen = do
--   wms <- getAtom "_NET_WM_STATE"
--   wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
--   mapM_ addNETSupported [wms, wfs]

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
clipboardy :: MonadIO m => m () -- Don't question it
clipboardy = spawn "rofi -modi \"\63053 :greenclip print\" -show \"\63053 \" -run-command '{cmd}' -theme ~/.config/rofi/launcher/style.rasi"

maimcopy :: MonadIO m => m () -- Don't question it
maimcopy = spawn "maim -s | xclip -selection clipboard -t image/png && notify-send \"Screenshot\" \"Copied to Clipboard\" -i flameshot"

maimsave :: MonadIO m => m () -- Don't question it
maimsave = spawn "maim -s ~/Pictures/desktop/$(date +%Y-%m-%d_%H-%M-%S).png && notify-send \"Screenshot\" \"Saved to Desktop\" -i flameshot"

rofiLauncher :: MonadIO m => m () -- Don't question it
rofiLauncher = spawn "rofi -no-lazy-grab -show drun -modi run,drun,window -theme $HOME/.config/rofi/launcher/style -drun-icon-theme \"candy-icons\" "

trayerColor :: String
trayerColor = "0x3b4252"

trayerStartup :: MonadIO m => m () -- Don't question it
trayerStartup = spawn $ "sleep 3 && trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 0 --tint " ++ trayerColor ++ " --height 16"

myKeymap :: XConfig Layout -> M.Map (KeyMask, KeySym) $ X ()
myKeymap conf =
  mkKeymap conf $
    [ -- launch a terminal
      ("M-S-<Return>", spawn $ XMonad.terminal conf),
      -- lock screen
      ("M-F1", spawn "betterlockscreen -l"),
      -- launch rofi and dashboard
      ("M-o", rofiLauncher),
      -- Audio keys
      ("<xF86AudioPlay>", spawn "playerctl play-pause"),
      ("<xF86AudioPrev>", spawn "playerctl previous"),
      ("<xF86AudioNext>", spawn "playerctl next"),
      ("<xF86AudioRaiseVolume>", spawn "pactl set-sink-volume 0 +5%"),
      ("<xF86AudioLowerVolume>", spawn "pactl set-sink-volume 0 -5%"),
      ("<xF86AudioMute>", spawn "pactl set-sink-mute 0 toggle"),
      -- Brightness keys
      ("<xF86MonBrightnessUp>", spawn "brightnessctl s +10%"),
      ("<xF86MonBrightnessDown>", spawn "brightnessctl s 10-%"),
      -- Screenshot
      ("<Print>", maimcopy),
      ("M-<Print>", maimsave),
      -- My Stuff
      ("M-z", spawn "exec ~/bin/inhibit_activate"),
      ("M-S-z", spawn "exec ~/bin/inhibit_deactivate"),
      ("M-S-a", clipboardy),
      -- close focused window
      ("M-S-c", kill),
      -- GAPS!!!
      ("M-C-g", sendMessage ToggleGaps), -- toggle all gaps
      ("M-S-g", sendMessage $ setGaps [(L, 10), (R, 10), (U, 10), (D, 10)]), -- reset the GapSpec
      ("M-C-t", sendMessage $ IncGap 10 L), -- increment the left-hand gap
      ("M-S-t", sendMessage $ DecGap 10 L), -- decrement the left-hand gap
      ("M-C-y", sendMessage $ IncGap 10 U), -- increment the top gap
      ("M-S-y", sendMessage $ DecGap 10 U), -- decrement the top gap
      ("M-C-u", sendMessage $ IncGap 10 D), -- increment the bottom gap
      ("M-S-u", sendMessage $ DecGap 10 D), -- decrement the bottom gap
      ("M-C-i", sendMessage $ IncGap 10 R), -- increment the right-hand gap
      ("M-S-i", sendMessage $ DecGap 10 R), -- decrement the right-hand gap

      -- Rotate through the available layout algorithms
      ("M-<Space>", sendMessage NextLayout),
      --  Reset the layouts on the current workspace to default
      ("M-S-<Space>", setLayout $ XMonad.layoutHook conf),
      -- Resize viewed windows to the correct size
      ("M-n", refresh),
      -- Move focus to the next window
      ("M-Tab", windows W.focusDown),
      -- Move focus to the next window
      ("M-j", windows W.focusDown),
      -- Move focus to the previous window
      ("M-k", windows W.focusUp),
      -- Move focus to the master window
      ("M-m", windows W.focusMaster),
      -- Swap the focused window and the master window
      ("M-<Return>", windows W.swapMaster),
      -- Swap the focused window with the next window
      ("M-S-j", windows W.swapDown),
      -- Swap the focused window with the previous window
      ("M-S-k", windows W.swapUp),
      -- Shrink the master area
      ("M-h", sendMessage Shrink),
      -- Expand the master area
      ("M-l", sendMessage Expand),
      -- Push window back into tiling
      ("M-t", withFocused $ windows . W.sink),
      -- Increment the number of windows in the master area
      ("M-,", sendMessage (IncMasterN 1)),
      -- Deincrement the number of windows in the master area
      ("M-.", sendMessage (IncMasterN (-1))),
      -- Toggle the status bar gap
      -- Use this binding with avoidStruts from Hooks.ManageDocks.
      -- See also the statusBar function from Hooks.DynamicLog.
      --
      -- , (("M, xK_b), sendMessage ToggleStruts)
      --
      -- Quit xmonad
      ("M-S-q", spawn "~/bin/powermenu.sh"),
      -- Restart xmonad
      ("M-q", spawn "xmonad --recompile ; xmonad --restart"),
      -- Run xmessage with a summary of the default keybindings (useful for beginners)
      -- TODO: figure out wich key is xK_slash
      ("M-S-/", spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
      ++
      --
      -- mod-[1..9], Switch to workspace N
      -- mod-shift-[1..9], Move client to workspace N
      --
      -- [ ((m .|. modm, k), windows $ f i)
      [ (m ++ [k], windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) ['1' .. '9'],
          (f, m) <- [(W.greedyView, "M-"), (W.shift, "M-S-")]
      ]
      ++
      --
      -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
      -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
      --
      [ (m ++ [key], screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip ['w', 'e', 'r'] [0 ..],
          (f, m) <- [(W.view, "M-"), (W.shift, "M-S-")]
      ]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modm}) =
  M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ( (modm, button1),
        \w ->
          focus w >> mouseMoveWindow w
            >> windows W.shiftMaster
      ),
      -- mod-button2, Raise the window to the top of the stack
      ((modm, button2), \w -> focus w >> windows W.shiftMaster),
      -- mod-button3, Set the window to floating mode and resize by dragging
      ( (modm, button3),
        \w ->
          focus w >> mouseResizeWindow w
            >> windows W.shiftMaster
      )
      -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
type MyLayout = ModifiedLayout Gaps (ModifiedLayout Spacing (ModifiedLayout SmartBorder (ModifiedLayout AvoidStruts (Choose Tall (Choose (Mirror Tall) Full)))))

myLayout :: MyLayout a
myLayout = gaps [(L, 10), (R, 10), (U, 10), (D, 10)] $ spacingRaw True (Border 10 10 10 10) True (Border 10 10 10 10) True $ smartBorders baseLayout
  where
    baseLayout = avoidStruts (tiled ||| Mirror tiled ||| Full)
    tiled = Tall nmaster delta ratio -- default tiling algorithm partitions the screen into two panes
    nmaster = 1 -- The default number of windows in the master pane
    ratio = 1 / 2 -- Default proportion of screen occupied by master pane
    delta = 3 / 100 -- Percent of screen to increment by when resizing panes

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook :: ManageHook
myManageHook =
  manageDocks
    <+> composeAll
      [ className =? "MPlayer" --> doFloat,
        className =? "Gimp" --> doFloat,
        className =? "nannou" --> doFloat,
        isDialog --> doFloat,
        resource =? "desktop_window" --> doIgnore,
        resource =? "kdesktop" --> doIgnore,
        isFullscreen --> doFullFloat
      ]

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook :: X ()
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook :: X ()
myStartupHook = do
  -- spawnOnce "exec ~/bin/eww daemon"
  spawn "xsetroot -cursor_name left_ptr"
  spawn "exec ~/bin/lock.sh"
  spawnOnce "feh --recursive --bg-scale --randomize ~/wallpapers/*"
  spawnOnce "picom --experimental-backends"
  spawnOnce "greenclip daemon"
  spawnOnce "dunst"
  trayerStartup

main :: IO ()
main = xmonad . docks . ewmhFullscreen . ewmh . myXmobarProp $ myConfig

--- xmobar config
myXmobarProp :: XConfig MyLayout -> XConfig (ModifiedLayout AvoidStruts MyLayout)
myXmobarProp = withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey

myXmobarPP :: PP
myXmobarPP =
  def
    { ppSep = magenta " • ",
      ppTitleSanitize =  xmobarStrip,
      ppCurrent = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2,
      ppHidden = white . wrap " " "",
      ppHiddenNoWindows = lowWhite . wrap " " "",
      ppUrgent = red . wrap (yellow "!") (yellow "!"),
      ppOrder = \[ws, l, _, wins] -> [ws, l, wins],
      ppExtras = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused = wrap (white "[") (white "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue . ppWindow

    -- Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarFont 2 .xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta = xmobarColor "#ff79c6" ""
    blue = xmobarColor "#bd93f9" ""
    white = xmobarColor "#f8f8f2" ""
    yellow = xmobarColor "#f1fa8c" ""
    red = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
myConfig :: XConfig MyLayout
myConfig =
  def
    { -- simple stuff
      terminal = myTerminal,
      focusFollowsMouse = myFocusFollowsMouse,
      clickJustFocuses = myClickJustFocuses,
      borderWidth = myBorderWidth,
      modMask = myModMask,
      workspaces = myWorkspaces,
      normalBorderColor = myNormalBorderColor,
      focusedBorderColor = myFocusedBorderColor,
      -- key bindings
      keys = myKeymap,
      mouseBindings = myMouseBindings,
      -- hooks, layouts
      manageHook = myManageHook,
      layoutHook = myLayout,
      -- handleEventHook = fullscreenEventHook,
      logHook = myLogHook,
      startupHook = myStartupHook
    }

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help =
  unlines
    [ "The default modifier key is 'super'. Default keybindings:",
      "",
      "-- launching and killing programs",
      "mod-Shift-Enter  Launch xterminal",
      "mod-o            Launch rofi",
      "mod-Shift-c      Close/kill the focused window",
      "mod-Space        Rotate through the available layout algorithms",
      "mod-Shift-Space  Reset the layouts on the current workSpace to default",
      "mod-n            Resize/refresh viewed windows to the correct size",
      "",
      "-- move focus up or down the window stack",
      "mod-Tab        Move focus to the next window",
      "mod-Shift-Tab  Move focus to the previous window",
      "mod-j          Move focus to the next window",
      "mod-k          Move focus to the previous window",
      "mod-m          Move focus to the master window",
      "",
      "-- modifying the window order",
      "mod-Return   Swap the focused window and the master window",
      "mod-Shift-j  Swap the focused window with the next window",
      "mod-Shift-k  Swap the focused window with the previous window",
      "",
      "-- resizing the master/slave ratio",
      "mod-h  Shrink the master area",
      "mod-l  Expand the master area",
      "",
      "-- floating layer support",
      "mod-t  Push window back into tiling; unfloat and re-tile it",
      "",
      "-- increase or decrease number of windows in the master area",
      "mod-comma  (mod-,)   Increment the number of windows in the master area",
      "mod-period (mod-.)   Deincrement the number of windows in the master area",
      "",
      "-- quit, or restart",
      "mod-Shift-q  Quit xmonad",
      "mod-q        Restart xmonad",
      "mod-[1..9]   Switch to workSpace N",
      "",
      "-- Workspaces & screens",
      "mod-Shift-[1..9]   Move client to workspace N",
      "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
      "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
      "",
      "-- Mouse bindings: default actions bound to mouse events",
      "mod-button1  Set the window to floating mode and move by dragging",
      "mod-button2  Raise the window to the top of the stack",
      "mod-button3  Set the window to floating mode and resize by dragging"
    ]
