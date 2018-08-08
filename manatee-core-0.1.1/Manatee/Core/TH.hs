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

{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, FlexibleInstances #-}
module Manatee.Core.TH where

import Control.Applicative
import Control.Monad
import DBus.Types 
import Language.Haskell.TH

-- | Unpack variant with corresponding type.
unpackVariantList :: Name -> Name -> ExpQ
unpackVariantList tagt dty = do
  -- getCons :: Info -> [(Name, Int)]
  let getCons (TyConI (DataD _ _ _ x _)) = [ (n,length ts) | NormalC n ts <- x ]
  -- :: Name -> Q [Name]
  tags <- (map fst . getCons) `fmap` reify tagt
  -- :: Name -> Q [(Name, Int)]
  dats <- getCons `fmap` reify dty
  [| \tag ls ->
       -- caseE :: ExpQ -> [MatchQ] -> ExpQ
       $( caseE [| (tag,ls) |] 
          -- [MatchQ]
          [do
            -- [DatArg] with length `n`.
            xs <- replicateM n (newName "x")
            match 
              -- (TagName, DatName arg1 arg2 ... argn)
              (tupP [recP t [],listP (map varP xs)])
              -- BodyQ
              (normalB $ foldl 
                           (\f x -> [| $f <*> $x |])
                           [| pure $(conE d) |]
                           (map (\x -> [| fromVariant $(varE x) |] ) xs))
              -- [DecQ]
              []
            -- [(Name, (Name, Int))]
            | (t,(d,n)) <- tags `zip` dats ]
          )
      |]

-- | This string is append in SignalArgs with MemberName.
-- Example dbus member named `Foo`, and SignalArgs should be `FooArgs`.
-- Otherwise, function `checkSignalArgs` will failed.
signalArgsTemplate :: String
signalArgsTemplate = "Args"

-- | Unpack variant with corresponding type.
checkSignalArgs :: Name -> Name -> ExpQ
checkSignalArgs tagt dty = do
  let getCons (TyConI (DataD _ _ _ x _)) = [ (n, ts) | NormalC n ts <- x ]
  tags <- (map fst . getCons) `fmap` reify tagt
  dats <- (map fst . getCons) `fmap` reify dty
  [| \tag ls ->
       $( caseE [| (tag,ls) |] 
          [do
            -- Get base name.
            let memberName = nameBase t ++ signalArgsTemplate
                signalArgsName = nameBase d
            match 
              (tupP [recP t [], recP d []])
              (normalB [| memberName == signalArgsName |])
              []
            | (t,d) <- tags `zip` dats ]
          )
      |]

-- | Pack variant with corresponding type.
packVariantList :: String -> Name -> Q [Dec]
packVariantList = packListTemplate (\x -> [| toVariant $(varE x) |]) 

-- | Pack variant with corresponding type.
packStringList :: String -> Name -> Q [Dec]
packStringList = packListTemplate (\x -> [| show $(varE x) |])

-- | This is template code for transform typ to [a]
packListTemplate :: (Name -> ExpQ) -> String -> Name -> Q [Dec]
packListTemplate fun str args = do
  TyConI (DataD _ _ _ constructors _) <- reify args
  let packList (NormalC name fields) = do
      vars <- replicateM (length fields) (newName "x")
      let pats = map varP vars
      clause [conP name pats]   -- build [PatQ], don't empty, otherwise got "unbound variable"
             (normalB (listE $ map fun vars)) 
             []

  body <- forM constructors (return . packList)
  fmap (: []) $ funD (mkName str) body

-- | Build declaration.
-- If this function got "unbound variable", try use `packVariantList` 
-- style to build [PatQ] explicitly.
mkDec :: String -> ExpQ -> DecQ
mkDec str f = funD (mkName str) [clause [] (normalB f) []]

-- | Build function declaration.
mkFunDec :: String -> ExpQ -> Q [Dec]
mkFunDec str f = fmap (: []) $ mkDec str f

-- | Generate n unique variables and return them in form of patterns and expressions.
genPE fields = do   
  ids <- replicateM (length fields) (newName "x") 
  return (map varP ids, map varE ids)

-- | Generate Variable instance.
deriveVariable _t = [d| 
    instance Variable $_t where
        toVariant = toVariant . show
        fromVariant x = fmap (\v -> read v :: $_t) $ fromVariant x|]

