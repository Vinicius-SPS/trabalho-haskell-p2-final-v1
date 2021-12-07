{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Backend where

import Common.Route
import Common.Api
import Obelisk.Backend
import Obelisk.Route
import Database.PostgreSQL.Simple
import Snap.Core
import qualified Data.Aeson as A
import Data.Text
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Text

-- TEMPLATE
migration :: Query
migration = "CREATE TABLE IF NOT EXISTS cliente\
  \ (id SERIAL PRIMARY KEY, nome TEXT NOT NULL)"

-- TEMPLATE
migrationProd :: Query
migrationProd = "CREATE TABLE IF NOT EXISTS produtos\
  \ (id SERIAL PRIMARY KEY, nome TEXT NOT NULL, valor REAL NOT NULL, qt INTEGER NOT NULL)"
-- CREATE TABLE RESTAURANTE
migrationRest :: Query
migrationRest = "CREATE TABLE IF NOT EXISTS tableRestaurante5\
  \ (idRest SERIAL PRIMARY KEY, nome TEXT NOT NULL, fila REAL NOT NULL, nota INTEGER NOT NULL, campo INTEGER NULL)"
-- CREATE TABLE CLIENTE
migrationCliente :: Query
migrationCliente = "CREATE TABLE IF NOT EXISTS tableCliente5\
  \ (idCliente SERIAL PRIMARY KEY, nome TEXT NOT NULL, telefone Integer NOT NULL, acompanhante INTEGER NOT NULL, campo INTEGER NULL)"
-- CREATE TABLE PRATO
migrationPrato :: Query
migrationPrato = "CREATE TABLE IF NOT EXISTS tablePrato5\
  \ (idPrato SERIAL PRIMARY KEY, nome TEXT NOT NULL, preco REAL NOT NULL, nota INTEGER NOT NULL, campo INTEGER NULL)"

getConn :: ConnectInfo
getConn = ConnectInfo ""
                      5432
                      ""
                      ""
                      ""

-- method GET $
backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
        dbcon <- connect getConn
        serve $ \case

        -- INSERIR

-- TEMPLATE
            BackendRoute_Produto :/ () -> do
                produto <- A.decode <$> readRequestBody 2000
                case produto of
                     Just prod -> do
                         liftIO $ do
                             execute_ dbcon migrationProd
                             execute dbcon "INSERT INTO produtos(nome,valor,qt) VALUES (?,?,?)" (prod :: Produto)
                         modifyResponse $ setResponseStatus 200 "OK"
                     _ -> modifyResponse $ setResponseStatus 500 "Erro"
-- TEMPLATE

-- INSERIR RESTAURANTE
            BackendRoute_Restaurante :/ () -> do
                restaurante <- A.decode <$> readRequestBody 2000
                case restaurante of
                     Just res -> do
                         liftIO $ do
                             execute_ dbcon migrationRest
                             execute dbcon "INSERT INTO tableRestaurante5(nome,fila,nota) VALUES (?,?,?)" (res :: Restaurante1)
                         modifyResponse $ setResponseStatus 200 "OK"
                     _ -> modifyResponse $ setResponseStatus 500 "Erro"
-- INSERIR RESTAURANTE

-- INSERIR CLIENTE
            BackendRoute_ClienteRestaurante :/ () -> do
                cliente <- A.decode <$> readRequestBody 2000
                case cliente of
                     Just cli-> do
                         liftIO $ do
                             execute_ dbcon migrationCliente
                             execute dbcon "INSERT INTO tableCliente5(nome,telefone,acompanhante) VALUES (?,?,?)" (cli :: ClienteRestaurante1)
                         modifyResponse $ setResponseStatus 200 "OK"
                     _ -> modifyResponse $ setResponseStatus 500 "Erro"
-- INSERIR CLIENTE

-- INSERIR PRATO
            BackendRoute_Prato :/ () -> do
                prato <- A.decode <$> readRequestBody 2000
                case prato of
                     Just prt -> do
                         liftIO $ do
                             execute_ dbcon migrationPrato
                             execute dbcon "INSERT INTO tablePrato5(nome,preco,nota) VALUES (?,?,?)" (prt :: Prato1)
                         modifyResponse $ setResponseStatus 200 "OK"
                     _ -> modifyResponse $ setResponseStatus 500 "Erro"
-- INSERIR PRATO


-- LISTAR


-- TEMPLATE
            BackendRoute_Listar :/ () -> do
                res :: [Produto] <- liftIO $ do
                        execute_ dbcon migrationProd
                        query_ dbcon "SELECT nome, valor, qt from produtos"
                modifyResponse $ setResponseStatus 200 "OK"
                writeLazyText (encodeToLazyText res)
-- TEMPLATE

-- LISTA RESTAURANTE
            BackendRoute_Listar_Restaurante :/ () -> do
                res :: [Restaurante] <- liftIO $ do
                        execute_ dbcon migrationRest
                        query_ dbcon "SELECT idRest, nome, fila, nota from tableRestaurante5 WHERE campo IS NULL"
                modifyResponse $ setResponseStatus 200 "OK"
                writeLazyText (encodeToLazyText res)
-- LISTA RESTAURANTE

-- LISTA CLIENTE
            BackendRoute_Listar_ClienteRestaurante :/ () -> do
                cli :: [ClienteRestaurante] <- liftIO $ do
                        execute_ dbcon migrationCliente
                        query_ dbcon "SELECT idCliente, nome, telefone, acompanhante from tableCliente5 WHERE campo IS NULL"
                modifyResponse $ setResponseStatus 200 "OK"
                writeLazyText (encodeToLazyText cli)
-- LISTA CLIENTE

-- LISTA PRATO
            BackendRoute_Listar_Prato :/ () -> do
                prt :: [Prato] <- liftIO $ do
                        execute_ dbcon migrationPrato
                        query_ dbcon "SELECT idPrato, nome, preco, nota from tablePrato5 WHERE campo IS NULL"
                modifyResponse $ setResponseStatus 200 "OK"
                writeLazyText (encodeToLazyText prt)
-- LISTA PRATO



        -- EDITAR

-- TEMPLATE
            BackendRoute_Editar :/ pid -> do
                produto <- A.decode <$> readRequestBody 2000
                case produto of
                     Just prod -> do
                         liftIO $ do
                             execute_ dbcon migrationProd
                             execute dbcon "UPDATE produtos SET nome =?,valor=?,qt=? WHERE id=?" (nomeProduto prod, valorProduto prod, qtProduto prod, pid)
                         modifyResponse $ setResponseStatus 203 "OK"
                     _ -> modifyResponse $ setResponseStatus 500 "Erro"
-- TEMPLATE

-- RESTAURANTE

            BackendRoute_Editar_Restaurante :/ () -> do
                restaurante <- A.decode <$> readRequestBody 2000
                case restaurante of
                     Just res -> do
                         liftIO $ do
                             execute_ dbcon migrationRest
                             execute dbcon "UPDATE tableRestaurante5 SET nome =?,fila=?,nota=? WHERE idRest=?" (nomeRestaurante res, quantidadeFila res, notaRestaurante res, idRest res)
                         modifyResponse $ setResponseStatus 203 "OK"
                     _ -> modifyResponse $ setResponseStatus 500 "Erro"

-- RESTAURANTE

-- CLIENTE
            BackendRoute_Editar_ClienteRestaurante :/ () -> do
                cliente <- A.decode <$> readRequestBody 2000
                case cliente of
                     Just cli -> do
                         liftIO $ do
                             execute_ dbcon migrationCliente
                             execute dbcon "UPDATE tableCliente5 SET nome =?,telefone=?,acompanhante=? WHERE idCliente=?" (nomeCliente cli, telefone cli, qntPessoas cli, idCliente cli)
                         modifyResponse $ setResponseStatus 203 "OK"
                     _ -> modifyResponse $ setResponseStatus 500 "Erro"
-- CLIENTE

-- PRATO
            BackendRoute_Editar_Prato :/ ()-> do
                prato <- A.decode <$> readRequestBody 2000
                case prato of
                     Just prt -> do
                         liftIO $ do
                             execute_ dbcon migrationPrato
                             execute dbcon "UPDATE tablePrato5 SET nome =?,preco=?,nota=? WHERE idPrato=?" (nomePrato prt, precoPrato prt, notaPrato prt, idPrato prt)
                         modifyResponse $ setResponseStatus 203 "OK"
                     _ -> modifyResponse $ setResponseStatus 500 "Erro"
-- PRATO

        -- DELETE

-- RESTAURANTE

            BackendRoute_Deletar_Restaurante :/ () -> do
                restaurante <- A.decode <$> readRequestBody 2000
                case restaurante of
                     Just res -> do
                         liftIO $ do
                             execute_ dbcon migrationRest
                             execute dbcon "UPDATE tableRestaurante5 SET nome =?,fila=?,nota=?,campo=? WHERE idRest=?" (nomeRestaurante2 res, quantidadeFila2 res, notaRestaurante2 res, delRes2 res, idRest2 res)
                         modifyResponse $ setResponseStatus 203 "OK"
                     _ -> modifyResponse $ setResponseStatus 500 "Erro"

-- RESTAURANTE

-- PRATO

            BackendRoute_Deletar_ClienteRestaurante :/ () -> do
                cliente <- A.decode <$> readRequestBody 2000
                case cliente of
                     Just cli -> do
                         liftIO $ do
                             execute_ dbcon migrationCliente
                             execute dbcon "UPDATE tableCliente5 SET nome =?,telefone=?,acompanhante=?,campo=? WHERE idCliente=?" (nomeCliente2 cli, telefone2 cli, qntPessoas2 cli, delCli2 cli,idCliente2 cli)
                         modifyResponse $ setResponseStatus 203 "OK"
                     _ -> modifyResponse $ setResponseStatus 500 "Erro"

-- PRATO
            BackendRoute_Deletar_Prato :/ () -> do
                prato <- A.decode <$> readRequestBody 2000
                case prato of
                     Just prt -> do
                         liftIO $ do
                             execute_ dbcon migrationPrato
                             execute dbcon "UPDATE tablePrato5 SET nome =?,preco=?,nota=?,campo=? WHERE idPrato=?" (nomePrato2 prt, precoPrato2 prt, notaPrato2 prt, delPrato2 prt,idPrato2 prt)
                         modifyResponse $ setResponseStatus 203 "OK"
                     _ -> modifyResponse $ setResponseStatus 500 "Erro"



        -- RESTO

-- TEMPLATE
            BackendRoute_Buscar :/ pid -> do
                res :: [Produto] <- liftIO $ do
                        execute_ dbcon migrationProd
                        query dbcon "SELECT nome,valor,qt from produtos where id=?" (Only (pid :: Int))
                if res /= [] then do
                        modifyResponse $ setResponseStatus 200 "OK"
                        writeLazyText (encodeToLazyText (Prelude.head res))
                else
                        modifyResponse $ setResponseStatus 404 "NOT FOUND"
-- TEMPLATE


        return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
