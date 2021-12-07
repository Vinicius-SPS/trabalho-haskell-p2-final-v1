{-# LANGUAGE DeriveGeneric #-}
{-# language DeriveAnyClass  #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Common.Api where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple

newtype Nome = Nome Text deriving (Generic, ToJSON, FromJSON)

data Produto = Produto {
    nomeProduto :: Text,
    valorProduto :: Double,
    qtProduto :: Int
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)


-- EDITAR RESTAURANTE
data Restaurante = Restaurante {
    idRest :: Int,
    nomeRestaurante :: Text,
    quantidadeFila :: Double,
    notaRestaurante :: Int
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

-- EDITAR CLIENTE
data ClienteRestaurante = ClienteRestaurante {
    idCliente :: Int,
    nomeCliente :: Text,
    telefone :: Int,
    qntPessoas :: Int
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

-- EDITAR PRATO
data Prato = Prato {
    idPrato :: Int,
    nomePrato :: Text,
    precoPrato :: Double,
    notaPrato :: Int
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

--INSERIR RESTAURANTE
data Restaurante1 = Restaurante1 {
    nomeRestaurante1 :: Text,
    quantidadeFila1 :: Double,
    notaRestaurante1 :: Int
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

-- INSERIR CLIENTE
data ClienteRestaurante1 = ClienteRestaurante1 {
    nomeCliente1 :: Text,
    telefone1 :: Int,
    qntPessoas1 :: Int
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

-- INSERIR PRATO
data Prato1 = Prato1 {
    nomePrato1 :: Text,
    precoPrato1 :: Double,
    notaPrato1 :: Int
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

--DELETAR RESTAURANTE
data Restaurante2 = Restaurante2 {
    idRest2 :: Int,
    nomeRestaurante2 :: Text,
    quantidadeFila2 :: Double,
    notaRestaurante2 :: Int,
    delRes2 :: Int
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

--DELETAR CLIENTE
data ClienteRestaurante2 = ClienteRestaurante2 {
    idCliente2 :: Int,
    nomeCliente2 :: Text,
    telefone2 :: Int,
    qntPessoas2 :: Int,
    delCli2 :: Int
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

-- DELETAR PRATO
data Prato2 = Prato2 {
    idPrato2 :: Int,
    nomePrato2 :: Text,
    precoPrato2 :: Double,
    notaPrato2 :: Int,
    delPrato2 :: Int
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)
