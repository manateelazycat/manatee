-- Author:     Andy Stewart <lazycat.manatee@gmail.com>
-- Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
-- 
-- Copyright (C) 2010 ~ 2011 Andy Stewart, all rights reserved.
-- 
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

import Distribution.Simple
import Manatee.Core.Config
import Manatee.Core.Types
import Manatee.Toolkit.Cabal.Utils
import Manatee.Toolkit.General.List
import Manatee.Toolkit.General.Map
import Manatee.Extension.ImageViewer.PageMode

import qualified Data.Map as M

main = defaultMainWithHooks 
       simpleUserHooks {
         -- Update PageTypeRule after install successful.
         postInst = \ _ _ pack_des lbi -> do
             (PageTypeRule rule) <- readConfig pageTypeRulePath (PageTypeRule M.empty)
             writeConfig pageTypeRulePath (PageTypeRule (M.insert "PageImage" "manatee-imageviewer" rule))
             -- Update FileOpenRule.
             let match = RegexpMatch "^image/*"
                 rule  = ("Open image", "PageImage", "")
             (FileOpenRule ruleMap) <- readConfig fileOpenRulePath (FileOpenRule M.empty)
             writeConfig fileOpenRulePath 
                         (FileOpenRule (let openRules = 
                                                case findMinMatch ruleMap (\ oMath _ -> oMath == match) of
                                                  Just (_, rules) -> insertUnique rule rules
                                                  Nothing -> [rule]
                                        in M.insert match openRules ruleMap))
             -- Update PageModeRule.
             (PageModeRule modeRule) <- readConfig pageModeRulePath (PageModeRule M.empty)
             writeConfig pageModeRulePath (PageModeRule 
                                                (M.insert "PageImage" (Left $ pageModeName imageMode) modeRule))
             -- Update Application info.
             let snapshotPath = getDataFilePath pack_des lbi "data/welcome/snapshot.png"
             (WelcomeApplication apps) <- readConfig welcomeApplicationPath (WelcomeApplication M.empty)
             writeConfig welcomeApplicationPath (WelcomeApplication 
                                                 (M.insert 
                                                   ("Image Viewer", snapshotPath)
                                                   ("PageImage", snapshotPath, [])
                                                   apps))
       }
