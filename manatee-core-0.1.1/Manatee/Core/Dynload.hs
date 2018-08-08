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

{-#Language ScopedTypeVariables#-}
{-# LANGUAGE DeriveDataTypeable #-}
module Manatee.Core.Dynload where

import Control.Concurrent
import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Typeable
import DynFlags
import Exception
import GHC
import GHC.Paths (libdir)
import Manatee.Core.Config
import Manatee.Core.Types
import System.Directory
import System.FilePath

import Data.List
import System.Exit
import System.Process
import HscTypes
import OccName
import Module
import BinIface
import HscMain hiding (compileExpr)
import TcRnMonad

data EmptyCustomize = 
    EmptyCustomize 
    deriving Typeable

instance Customize EmptyCustomize where
  customizeConfigFile _ = ""
  customizeLoad       _ = []

-- | Empty customize new.
emptyCustomizeNew :: IO CustomizeWrap
emptyCustomizeNew = 
  return (CustomizeWrap EmptyCustomize)

-- | Cast customize.
castCustomize :: Typeable b => CustomizeWrap -> b
castCustomize (CustomizeWrap c) =
    fromMaybe (error "castCustomize : impossible failed.") (cast c)
 
-- | Dynamic loading module in running program.
dynload :: (String, String, [(String, HValue -> IO ())]) -> IO ()
dynload (filepath, moduleName, loadList) = 
   defaultErrorHandler defaultDynFlags $ 
      runGhc (Just libdir) $ 
        ghandle 
          -- Avoid exception when dynamic loading.
          (\ (e :: SomeException) -> liftIO $ putStrLn $ "# " ++ show e)
          (do                   
             -- Set DynFlags.
             dFlags <- getSessionDynFlags 
             setSessionDynFlags dFlags {ghcLink = LinkInMemory} -- this step is necessary
             -- Load module.
             target <- guessTarget filepath Nothing
             setTargets [target]
             r <- load (LoadUpTo (mkModuleName moduleName))
             -- Check.
             case r of
                 Failed ->
                     liftIO $ putStrLn $ "* Load module " ++ moduleName ++ " failed."
                 Succeeded -> do
                     liftIO $ putStrLn $ "* Load module " ++ moduleName ++ " success."
                     -- Find module.
                     m <- findModule (mkModuleName moduleName) Nothing
                     -- Set context.
                     setContext [] [(m, Nothing)]
                     -- Load value.
                     forM_ loadList $ \ (symbolName, loadFun) -> 
                         ghandle 
                           -- Skip current symbol if get error when compileExpr.
                           (\ (e :: SomeException) -> 
                             liftIO $ putStrLn $ "# Get error and skip symbol `" ++ symbolName ++ "`\n    " ++ show e)
                           (do
                             hValue <- compileExpr (moduleName ++ "." ++ symbolName)
                             liftIO $ loadFun hValue))

-- | Load user config.
loadConfig :: CustomizeWrap -> Bool -> IO ()
loadConfig (CustomizeWrap customize) isAsync = do
  -- Get configure directory.
  configDir <- getConfigDirectory
  -- Init.
  let configFile = customizeConfigFile customize
      filepath = configDir </> configPath </> configFile
      configLoad = customizeLoad customize
      load = do
        putStrLn $ "* Loading configure file : " ++ filepath
        dynload (filepath, "Config.User", configLoad)
  isExist <- doesFileExist filepath

  -- Try load user's configure file.
  if null configLoad
     -- Skip load if no customize option. 
     then putStrLn "No customize option, skip load configure file."
     else if isExist
             -- Load config file.
             then if isAsync then forkIO load >> return () else load
             -- Skip load if no configure file exist.
             else putStrLn $ "Config file `" ++ filepath ++ "` no exist, use default customize option."

-- | Generate config file.
generateConfig :: String -> [String] -> IO ()
generateConfig configFile scanList = do
  -- Get directory.
  configDir <- getConfigDirectory
  let configHsFile = configDir </> configPath </> configFile
      configHiFile = replaceExtension configHsFile ".hi"
  isHsFileExist <- doesFileExist configHsFile
  isHiFileExist <- doesFileExist configHiFile
  -- Detect user configure file.
  scanUser <- 
      if isHsFileExist
         then do 
           processHandle <- runCommand ("ghc --make " ++ configHsFile)
           exitCode <- waitForProcess processHandle
           case exitCode of
             ExitSuccess -> return True
             ExitFailure _ -> do
                 putStrLn $ configHsFile ++ " compile failed, skip user's configure file."
                 return False
         else do 
           putStrLn $ configHsFile ++ " no exist, skip scan user's configure file."
           return False
  -- Generate User.hs and Import.hs.
  if scanUser && isHiFileExist
     then do
       -- Copy user's configure file.
       readFile configHsFile >>= writeFile "./Config/User.hs"  
       -- Get export list.
       moduleInterface <- readBinIface' configHiFile
       let ifaceExport = mi_exports moduleInterface
           exports = map (\ (mod, items) -> 
                            (Module.moduleNameString $ Module.moduleName mod
                            ,concatMap (\item -> 
                                            case item of
                                              Avail name -> [occNameString name]
                                              AvailTC _ list -> 
                                                  map occNameString list
                                       ) items)
                         ) ifaceExport
           (((_, exportList):_), _) = partition (\ (mName, _) -> mName == "Config.User") exports
       -- Generate Import.hs
       writeFile "./Config/Import.hs" $ generateImportString (filter (\x -> x `elem` scanList) exportList) scanList
     else do
       -- Generate User.hs and Import.hs
       writeFile "./Config/User.hs" emptyUserString
       writeFile "./Config/Import.hs" $ emptyImportString scanList

-- | Read binary interface file to get module information.
readBinIface' :: FilePath -> IO ModIface
readBinIface' hi_path = do
    e <- newHscEnv defaultCallbacks undefined
    initTcRnIf 'r' e undefined undefined (readBinIface IgnoreHiWay QuietBinIFaceReading hi_path)

-- | Concat improt, convert ["a","b","c"] to (a, b, c)
concatImport :: [String] -> String
concatImport []  = ""
concatImport [x] = x
concatImport (x:xs) = 
  (x ++ ", ") ++ concatImport xs

-- | Emtpy user string.
emptyUserString :: String
emptyUserString = 
    copyrightString
 ++ "module Config.User where"

-- | Emtpy import string.
emptyImportString :: [String] -> String
emptyImportString scanList = 
    copyrightString
 ++ "module Config.Import "
 ++ "(" ++ concatImport scanList ++ ") where \n\n"
 ++ "import Config.Default"

-- | Generate import string.
generateImportString :: [String] -> [String] -> String
generateImportString [] scanList = 
    emptyImportString scanList
generateImportString exportList scanList =
    copyrightString
 ++ "module Config.Import "
 ++ "(" ++ concatImport scanList ++ ") where \n\n"
 ++ "import Config.User (" ++ concatImport exportList ++ ")\n"
 ++ "import Config.Default hiding (" ++ concatImport exportList ++ ")\n"

-- | Copyright string.
copyrightString :: String
copyrightString = 
     "-- Author:     Andy Stewart <lazycat.manatee@gmail.com>"
  ++ "\n-- Maintainer: Andy Stewart <lazycat.manatee@gmail.com>"
  ++ "\n-- "
  ++ "\n-- Copyright (C) 2010 Andy Stewart, all rights reserved."
  ++ "\n-- "
  ++ "\n-- This program is free software: you can redistribute it and/or modify"
  ++ "\n-- it under the terms of the GNU General Public License as published by"
  ++ "\n-- the Free Software Foundation, either version 3 of the License, or"
  ++ "\n-- any later version."
  ++ "\n-- "
  ++ "\n-- This program is distributed in the hope that it will be useful,"
  ++ "\n-- but WITHOUT ANY WARRANTY; without even the implied warranty of"
  ++ "\n-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
  ++ "\n-- GNU General Public License for more details."
  ++ "\n-- "
  ++ "\n-- You should have received a copy of the GNU General Public License"
  ++ "\n-- along with this program.  If not, see <http://www.gnu.org/licenses/>.\n\n"
