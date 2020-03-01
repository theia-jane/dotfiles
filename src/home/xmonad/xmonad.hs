import XMonad
import qualified XMonad.StackSet as W
-- import qualified Data.Map        as M
-- import Control.Monad
-- import System.IO
-- import System.IO.Unsafe
-- import Data.List
-- import Data.Ratio ((%))
-- import XMonad.ManageHook
-- Actions
-- import XMonad.Actions.CycleWS
-- import XMonad.Actions.SpawnOn
-- import XMonad.Actions.OnScreen
-- import XMonad.Actions.SwapWorkspaces
-- import XMonad.Actions.Submap
-- import XMonad.Actions.Search
-- import XMonad.Actions.UpdatePointer
-- import XMonad.Actions.SpawnOn
-- Hooks
-- import XMonad.Hooks.ManageHelpers
-- import XMonad.Hooks.DynamicLog
-- import XMonad.Hooks.ManageDocks
-- import XMonad.Hooks.SetWMName
-- import XMonad.Hooks.FadeInactive
-- import XMonad.Hooks.UrgencyHook hiding (Never)
-- import XMonad.Hooks.ICCCMFocus
-- Layouts
-- import XMonad.Layout.PerWorkspace
-- import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.TwoPane
import XMonad.Layout.NoBorders
-- import XMonad.Layout.Reflect
import XMonad.Layout.Tabbed
-- import qualified XMonad.Layout.Magnifier as Mag
import XMonad.Layout.MultiToggle
import XMonad.Layout.Minimize
-- import XMonad.Layout.BoringWindows
-- import qualified XMonad.Layout.ToggleLayouts as Tog
-- Prompts
-- import XMonad.Prompt
-- import XMonad.Prompt.Input
-- import XMonad.Prompt.Workspace
-- import XMonad.Prompt.Shell
-- import XMonad.Prompt.XMonad
-- import XMonad.Prompt.RunOrRaise
-- import XMonad.Prompt.AppendFile
-- Util
-- import XMonad.Util.NamedScratchpad
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)


main = do
  spawn "init-post-wm xmonad"
  xmonad $ defaultConfig
    { manageHook = manageHook defaultConfig
    , modMask = mod4Mask -- Rebind Mod to the Windows key
    , borderWidth = 5
    , layoutHook = myLayoutHook
    } `additionalKeysP` myKeys

myLayoutHook =
    Tall 1 (10/100) (60/100)
    ||| TwoPane (15/100) (55/100)
    ||| Grid
    ||| Full

myKeys =
  [ ("M-C-r", sendMessage NextLayout)
  , ("M-S-m", windows W.swapMaster)
  ]
