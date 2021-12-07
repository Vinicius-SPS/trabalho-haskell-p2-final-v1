{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Common.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}
import Data.Function

import Data.Text (Text, unpack)
import Data.Functor.Identity

import Obelisk.Route
import Obelisk.Route.TH

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Produto :: BackendRoute ()
  BackendRoute_Editar :: BackendRoute Int
  BackendRoute_Buscar :: BackendRoute Int
  BackendRoute_Listar :: BackendRoute ()
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Insere :: BackendRoute ()


  --TRABALHO
  BackendRoute_Restaurante :: BackendRoute()
  BackendRoute_ClienteRestaurante :: BackendRoute()
  BackendRoute_Prato :: BackendRoute()

  BackendRoute_Listar_Restaurante :: BackendRoute()
  BackendRoute_Editar_Restaurante :: BackendRoute()
  BackendRoute_Deletar_Restaurante :: BackendRoute()

  BackendRoute_Listar_ClienteRestaurante :: BackendRoute()
  BackendRoute_Editar_ClienteRestaurante :: BackendRoute()
  BackendRoute_Deletar_ClienteRestaurante :: BackendRoute()

  BackendRoute_Listar_Prato :: BackendRoute()
  BackendRoute_Editar_Prato :: BackendRoute()
  BackendRoute_Deletar_Prato :: BackendRoute()
  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

checkedFullRouteEncoder
  :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
checkedFullRouteEncoder = checkEncoder fullRouteEncoder & \case
  Left err -> error $ unpack err
  Right encoder -> encoder

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_Insere -> PathSegment "nome" $ unitEncoder mempty
      BackendRoute_Produto -> PathSegment "produto" $ unitEncoder mempty
      BackendRoute_Buscar -> PathSegment "buscar" readShowEncoder
      BackendRoute_Editar -> PathSegment "editar" readShowEncoder
      BackendRoute_Listar -> PathSegment "listar" $ unitEncoder mempty

      BackendRoute_Prato -> PathSegment "prato" $ unitEncoder mempty
      BackendRoute_ClienteRestaurante -> PathSegment "cliente" $ unitEncoder mempty
      BackendRoute_Restaurante -> PathSegment "restaurante" $ unitEncoder mempty

      BackendRoute_Listar_Restaurante -> PathSegment "listarRestaurante" $ unitEncoder mempty
      BackendRoute_Editar_Restaurante -> PathSegment "editarRestaurante" $ unitEncoder mempty
      BackendRoute_Deletar_Restaurante -> PathSegment "deletarRestaurante" $ unitEncoder mempty

      BackendRoute_Listar_ClienteRestaurante -> PathSegment "listarClienteRestaurante" $ unitEncoder mempty
      BackendRoute_Editar_ClienteRestaurante -> PathSegment "editarClienteRestaurante" $ unitEncoder mempty
      BackendRoute_Deletar_ClienteRestaurante -> PathSegment "deletarClienteRestaurante" $ unitEncoder mempty

      BackendRoute_Listar_Prato -> PathSegment "listarPrato" $ unitEncoder mempty
      BackendRoute_Editar_Prato -> PathSegment "editarPrato" $ unitEncoder mempty
      BackendRoute_Deletar_Prato -> PathSegment "deletarPrato" $ unitEncoder mempty

      )


      --TRABALHO


  (\case
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty)

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
