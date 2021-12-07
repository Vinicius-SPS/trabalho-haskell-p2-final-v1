{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo, BlockArguments #-}

module Frontend where

import Control.Monad
import Data.Maybe
import Text.Read (readMaybe)
import qualified Data.Text as T

import Control.Monad.Fix (MonadFix)
import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route

----------------

getPath :: R BackendRoute -> T.Text
getPath rt = renderBackendRoute checkedFullRouteEncoder rt

nomeRequest :: T.Text -> XhrRequest T.Text
nomeRequest s = postJson (getPath (BackendRoute_Insere :/ ())) (Nome s)


prodRequest :: Produto -> XhrRequest T.Text
prodRequest p = postJson (getPath (BackendRoute_Produto :/ ())) p

getListReq :: XhrRequest ()
getListReq = xhrRequest "GET" (getPath (BackendRoute_Listar :/ ())) def



--TRABALHO
restRequest :: Restaurante1 -> XhrRequest T.Text
restRequest p = postJson (getPath (BackendRoute_Restaurante :/ ())) p

cliRequest :: ClienteRestaurante1 -> XhrRequest T.Text
cliRequest p = postJson (getPath (BackendRoute_ClienteRestaurante :/ ())) p

pratoRequest :: Prato1 -> XhrRequest T.Text
pratoRequest p = postJson (getPath (BackendRoute_Prato :/ ())) p

getListRestaurante :: XhrRequest ()
getListRestaurante = xhrRequest "GET" (getPath (BackendRoute_Listar_Restaurante :/ ())) def

getListClienteRestaurante :: XhrRequest ()
getListClienteRestaurante = xhrRequest "GET" (getPath (BackendRoute_Listar_ClienteRestaurante :/ ())) def

getListPrato :: XhrRequest ()
getListPrato = xhrRequest "GET" (getPath (BackendRoute_Listar_Prato :/ ())) def

updateRest :: Restaurante -> XhrRequest T.Text
updateRest id = postJson (getPath (BackendRoute_Editar_Restaurante :/ ())) id

updateCliente :: ClienteRestaurante -> XhrRequest T.Text
updateCliente p = postJson (getPath (BackendRoute_Editar_ClienteRestaurante :/ ())) p

updatePrato :: Prato -> XhrRequest T.Text
updatePrato p = postJson (getPath (BackendRoute_Editar_Prato :/ ())) p

getProdReq :: Int -> XhrRequest ()
getProdReq id = xhrRequest "GET" (getPath (BackendRoute_Buscar :/ id)) def

delRest :: Restaurante2-> XhrRequest T.Text
delRest p = postJson (getPath (BackendRoute_Deletar_Restaurante :/ ())) p

delCliente :: ClienteRestaurante2 -> XhrRequest T.Text
delCliente p = postJson (getPath (BackendRoute_Deletar_ClienteRestaurante :/ ())) p

delPrato :: Prato2 -> XhrRequest T.Text
delPrato p = postJson (getPath (BackendRoute_Deletar_Prato :/ ())) p

---------------------------------------------------------------------------------------------------------------------------------------

-- TEMPLATE
---------------------------------------------------------------------------------------------------------------------------------------

req :: ( DomBuilder t m
       , Prerender js t m
       ) => m ()
req = do
    inputEl <- inputElement def
    (submitBtn,_) <- el' "button" (text "Inserir")
    let click = domEvent Click submitBtn
    let nm = tag (current $ _inputElement_value inputEl) click
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (nomeRequest <$> nm))
    return ()


tabProduto :: DomBuilder t m => Produto -> m ()
tabProduto pr = do
    el "tr" $ do
        el "td" (text $ nomeProduto pr)
        el "td" (text $ T.pack $ show $ valorProduto pr)
        el "td" (text $ T.pack $ show $ qtProduto pr)

reqLista :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => m ()
reqLista = do
    (btn, _) <- el' "button" (text "Listar")
    let click = domEvent Click btn
    prods :: Dynamic t (Event t (Maybe [Produto])) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const getListReq <$> click))
    dynP <- foldDyn (\ps d -> case ps of
                            Nothing -> []
                            Just p -> d++p) [] (switchDyn prods)
    el "table" $ do
        el "thead" $ do
            el "tr" $ do
                el "th" (text "Nome")
                el "th" (text "Valor")
                el "th" (text "Qt")

        el "tbody" $ do
            dyn_ (fmap sequence (ffor dynP (fmap tabProduto)))

reqProd :: ( DomBuilder t m
       , Prerender js t m
       ) => m ()
reqProd = do
    divClass "imagem-1" $ do
        divClass "container" $ do
            elClass "div" "box" $ do
                nome <- inputElement def
                vl <- numberInput
                qt <- numberInput
                let prod = fmap (\((n,v),i) -> Produto n v i) (zipDyn (zipDyn (_inputElement_value nome) vl) qt)
                (submitBtn,_) <- el' "button" (text "Inserir")
                let click = domEvent Click submitBtn
                let prodEvt = tag (current prod) click
                _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
                    (pure never)
                    (fmap decodeXhrResponse <$> performRequestAsync (prodRequest <$> prodEvt))
                return ()

---------------------------------------------------------------------------------------------------------------------------------------

--PAGINAS DE INSERIR
---------------------------------------------------------------------------------------------------------------------------------------
reqClienteRestaurante :: ( DomBuilder t m
       , Prerender js t m
       ) => m ()
reqClienteRestaurante = do
            elClass "div" "box" $ do
                elClass "p" "" $ text("INSERIR CLIENTE")
                elClass "p" "" $ text("Nome do Cliente")
                nomeCliente1 <- inputElement def
                elClass "p" "" $ text("Telefone")
                telefone1 <- numberInput
                elClass "p" "" $ text("Total de Acompanhantes")
                qntPessoas1 <- numberInput
                elClass "p" "" $ text("")
                let prod = fmap (\((n,v),i) -> ClienteRestaurante1 n v i) (zipDyn (zipDyn (_inputElement_value nomeCliente1) telefone1) qntPessoas1)
                (submitBtn,_) <- el' "button" (text "Inserir")
                let click = domEvent Click submitBtn
                let resEvt = tag (current prod) click
                _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
                    (pure never)
                    (fmap decodeXhrResponse <$> performRequestAsync (cliRequest <$> resEvt))
                return ()

reqRestaurante :: ( DomBuilder t m
       , Prerender js t m
       ) => m ()
reqRestaurante = do
            elClass "div" "box" $ do
                elClass "p" "" $ text("INSERIR RESTAURANTE")
                elClass "p" "" $ text("Nome do Restaurante")
                nomeRestaurante1 <- inputElement def
                elClass "p" "" $ text("Pessoas na fila")
                quantidadeFila1 <- numberInput
                elClass "p" "" $ text("Nota do Restaurante")
                notaRestaurante1 <- numberInput
                elClass "p" "" $ text(" ")
                let prod = fmap (\(((n,v),i)) -> Restaurante1 n v i) (zipDyn (zipDyn (_inputElement_value nomeRestaurante1) quantidadeFila1) notaRestaurante1)
                (submitBtn,_) <- el' "button" (text "Inserir")
                let click = domEvent Click submitBtn
                let resEvt = tag (current prod) click
                _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
                    (pure never)
                    (fmap decodeXhrResponse <$> performRequestAsync (restRequest <$> resEvt))
                return ()



reqPrato :: ( DomBuilder t m
       , Prerender js t m
       ) => m ()
reqPrato = do
            elClass "div" "box" $ do
                elClass "p" "" $ text("INSERIR PRATO")
                elClass "p" "" $ text("Nome do Prato")
                nomePrato1 <- inputElement def
                elClass "p" "" $ text("Preco")
                precoPrato1 <- numberInput
                elClass "p" "" $ text("Nota do Prato")
                notaPrato1 <- numberInput
                elClass "p" "" $ text(" ")
                let prod = fmap (\((n,v),i) -> Prato1 n v i) (zipDyn (zipDyn (_inputElement_value nomePrato1) precoPrato1) notaPrato1)
                (submitBtn,_) <- el' "button" (text "Inserir")
                let click = domEvent Click submitBtn
                let resEvt = tag (current prod) click
                _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
                    (pure never)
                    (fmap decodeXhrResponse <$> performRequestAsync (pratoRequest <$> resEvt))
                return ()
---------------------------------------------------------------------------------------------------------------------------------------

--PAGINAS DE LISTAR
---------------------------------------------------------------------------------------------------------------------------------------

--LISTAR PRATOS
tabListaPrato :: DomBuilder t m => Prato -> m ()
tabListaPrato pr = do
    el "tr" $ do
        el "td" (text $ T.pack $ show $ idPrato pr)
        el "td" (text $ nomePrato pr)
        el "td" (text $ T.pack $ show $ precoPrato pr)
        el "td" (text $ T.pack $ show $ notaPrato pr)

reqListaPrato :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => m ()
reqListaPrato = do
    divClass "imagem-prato" $ do
        divClass "container" $ do
            elClass "div" "box-listar" $ do
                (btn, _) <- el' "button" (text "Listar")
                let click = domEvent Click btn
                prods :: Dynamic t (Event t (Maybe [Prato])) <- prerender
                    (pure never)
                    (fmap decodeXhrResponse <$> performRequestAsync (const getListPrato <$> click))
                dynP <- foldDyn (\ps d -> case ps of
                                        Nothing -> []
                                        Just p -> d++p) [] (switchDyn prods)
                el "table" $ do
                    el "thead" $ do
                        el "tr" $ do
                            el "th" (text "Id |")
                            el "th" (text "Nome do Prato |")
                            el "th" (text "Preco |")
                            el "th" (text "Avaliacao")

                    el "tbody" $ do
                        dyn_ (fmap sequence (ffor dynP (fmap tabListaPrato)))


--LISTAR CLIENTES
tabListaClienteRestaurante :: DomBuilder t m => ClienteRestaurante -> m ()
tabListaClienteRestaurante pr = do
    el "tr" $ do
        el "td" (text $ T.pack $ show $ idCliente pr)
        el "td" (text $ nomeCliente pr)
        el "td" (text $ T.pack $ show $ qntPessoas pr)
        el "td" (text $ T.pack $ show $ telefone pr)

reqListaClienteRestaurante :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => m ()
reqListaClienteRestaurante = do
    divClass "imagem-cliente" $ do
        divClass "container" $ do
            elClass "div" "box-listar" $ do
                (btn, _) <- el' "button" (text "Listar")
                let click = domEvent Click btn
                prods :: Dynamic t (Event t (Maybe [ClienteRestaurante])) <- prerender
                    (pure never)
                    (fmap decodeXhrResponse <$> performRequestAsync (const getListClienteRestaurante <$> click))
                dynP <- foldDyn (\ps d -> case ps of
                                        Nothing -> []
                                        Just p -> d++p) [] (switchDyn prods)
                el "table" $ do
                    el "thead" $ do
                        el "tr" $ do
                            el "th" (text "Id |")
                            el "th" (text "Nome do Cliente |")
                            el "th" (text "Total de acompanhantes |")
                            el "th" (text "Telefone")

                    el "tbody" $ do
                        dyn_ (fmap sequence (ffor dynP (fmap tabListaClienteRestaurante)))

--LISTAR RESTAURANTE
tabRestaurante :: DomBuilder t m => Restaurante -> m ()
tabRestaurante pr = do
    el "tr" $ do
        el "td" (text $ T.pack $ show $ idRest pr)
        el "td" (text $ nomeRestaurante pr)
        el "td" (text $ T.pack $ show $ quantidadeFila pr)
        el "td" (text $ T.pack $ show $ notaRestaurante pr)

reqListaRestaurante :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => m ()
reqListaRestaurante = do
    divClass "imagem-restaurante" $ do
        divClass "container" $ do
            elClass "div" "box-listar" $ do
                (btn, _) <- el' "button" (text "Listar")
                let click = domEvent Click btn
                prods :: Dynamic t (Event t (Maybe [Restaurante])) <- prerender
                    (pure never)
                    (fmap decodeXhrResponse <$> performRequestAsync (const getListRestaurante <$> click))
                dynP <- foldDyn (\ps d -> case ps of
                                        Nothing -> []
                                        Just p -> d++p) [] (switchDyn prods)
                el "table" $ do
                    el "thead" $ do
                        el "tr" $ do
                            el "th" (text "Id |")
                            el "th" (text "Nome do Restaurante |")
                            el "th" (text "Pessoas na Fila |")
                            el "th" (text "Nota do Restaurante")

                    el "tbody" $ do
                        dyn_ (fmap sequence (ffor dynP (fmap tabRestaurante)))




---------------------------------------------------------------------------------------------------------------------------------------

--PAGINAS DE EDITAR
---------------------------------------------------------------------------------------------------------------------------------------
{-
reqDeletaRestaurante :: ( DomBuilder t m
       , Prerender js t m
       ) => m ()
reqDeletaRestaurante = elClass "div" "" $ do
    elClass "p" "" $ text("Pessoas na fila")
    idRest <- numberInput
    elClass "p" "" $ text("Nome do Restaurante")
    nomeRestaurante <- inputElement def
    elClass "p" "" $ text("Nota Restaurante")
    quantidadeFila <- numberInput
    elClass "p" "" $ text("Id Restaurante")
    notaRestaurante <- numberInput
    let prod = fmap (\(((v,n),i),q) -> Restaurante n v i q) (zipDyn (zipDyn (zipDyn (_inputElement_value nomeRestaurante) notaRestaurante) idRest) quantidadeFila)
    (submitBtn,_) <- el' "button" (text "Deletar")
    let click = domEvent Click submitBtn
    let resEvt = tag (current prod) click
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (delRest <$> resEvt))
    return ()
-}

reqAlteraRestaurante :: ( DomBuilder t m
       , Prerender js t m
       ) => m ()
reqAlteraRestaurante = do
            elClass "div" "box-editar" $ do
                elClass "p" "" $ text("EDITAR RESTAURANTE")
                elClass "p" "" $ text("Pessoas na fila")
                idRest <- numberInput
                elClass "p" "" $ text("Nome do Restaurante")
                nomeRestaurante <- inputElement def
                elClass "p" "" $ text("Nota Restaurante")
                quantidadeFila <- numberInput
                elClass "p" "" $ text("Id Restaurante")
                notaRestaurante <- numberInput
                let prod = fmap (\(((v,n),i),q) -> Restaurante n v i q) (zipDyn (zipDyn (zipDyn (_inputElement_value nomeRestaurante) notaRestaurante) idRest) quantidadeFila)
                (submitBtn,_) <- el' "button" (text "Editar")
                let click = domEvent Click submitBtn
                let resEvt = tag (current prod) click
                _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
                    (pure never)
                    (fmap decodeXhrResponse <$> performRequestAsync (updateRest <$> resEvt))
                return ()


reqAlteraClienteRestaurante :: ( DomBuilder t m
       , Prerender js t m
       ) => m ()
reqAlteraClienteRestaurante = do
            elClass "div" "box-editar" $ do
                elClass "p" "" $ text("Id Cliente")
                qntPessoas <- numberInput
                elClass "p" "" $ text("Nome do Cliente")
                nomeCliente <- inputElement def
                elClass "p" "" $ text("Telefone")
                idCliente <- numberInput
                elClass "p" "" $ text("Total de Acompanhantes")
                telefone <- numberInput
                let prod = fmap (\(((v,n),i),q) -> ClienteRestaurante n v i q) (zipDyn (zipDyn (zipDyn (_inputElement_value nomeCliente) qntPessoas) idCliente) telefone)
                (submitBtn,_) <- el' "button" (text "Editar")
                let click = domEvent Click submitBtn
                let resEvt = tag (current prod) click
                _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
                    (pure never)
                    (fmap decodeXhrResponse <$> performRequestAsync (updateCliente <$> resEvt))
                return ()

reqAlteraPrato :: ( DomBuilder t m
       , Prerender js t m
       ) => m ()
reqAlteraPrato = do
            elClass "div" "box-editar" $ do
                elClass "p" "" $ text("EDITAR PRATO")
                elClass "p" "" $ text("Id Prato")
                notaPrato <- numberInput
                elClass "p" "" $ text("Nome do Prato")
                nomePrato <- inputElement def
                elClass "p" "" $ text("Preco")
                idPrato <- numberInput
                elClass "p" "" $ text("Nota do Prato")
                precoPrato <- numberInput

                let prod = fmap (\(((v,n),i),q) -> Prato n v i q) (zipDyn (zipDyn (zipDyn (_inputElement_value nomePrato) notaPrato) idPrato) precoPrato)
                (submitBtn,_) <- el' "button" (text "Editar")
                let click = domEvent Click submitBtn
                let resEvt = tag (current prod) click
                _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
                    (pure never)
                    (fmap decodeXhrResponse <$> performRequestAsync (updatePrato <$> resEvt))
                return ()

reqDelRest :: ( DomBuilder t m
       , Prerender js t m
       ) => m ()
reqDelRest =  do
    elClass "div" "box-deletar" $ do
        elClass "p" "" $ text("EXCLUIR RESTAURANTE")
        elClass "p" "" $ text("Id Restaurante")
        notaRestaurante2 <- numberInput
        elClass "p" "" $ text("---")
        nomeRestaurante2 <- inputElement def
        elClass "p" "" $ text("---")
        idRest2 <- numberInput
        elClass "p" "" $ text("---")
        quantidadeFila2 <- numberInput
        elClass "p" "" $ text("Insira qualquer numero para confirmar a delecao")
        campoDel2 <- numberInput
        let prod = fmap (\((((v,n),i),q),u) -> Restaurante2 n v i q u) (zipDyn (zipDyn (zipDyn (zipDyn (_inputElement_value nomeRestaurante2) notaRestaurante2) idRest2) quantidadeFila2) campoDel2)
        (submitBtn,_) <- el' "button" (text "Deletar")
        let click = domEvent Click submitBtn
        let resEvt = tag (current prod) click
        _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
            (pure never)
            (fmap decodeXhrResponse <$> performRequestAsync (delRest <$> resEvt))
        return ()

reqDelCliente :: ( DomBuilder t m
       , Prerender js t m
       ) => m ()
reqDelCliente = do
    elClass "div" "box-deletar" $ do
        elClass "p" "" $ text("EXCLUIR CLIENTE")
        elClass "p" "" $ text("Id Cliente")
        qntPessoas2 <- numberInput
        elClass "p" "" $ text("---")
        nomeCliente2 <- inputElement def
        elClass "p" "" $ text("---")
        idCliente2 <- numberInput
        elClass "p" "" $ text("---")
        telefone2 <- numberInput
        elClass "p" "" $ text("Insira qualquer numero para confirmar a delecao")
        delCli2 <- numberInput
        let prod = fmap (\((((v,n),i),q),u) -> ClienteRestaurante2 n v i q u) (zipDyn (zipDyn (zipDyn (zipDyn (_inputElement_value nomeCliente2) qntPessoas2) idCliente2) telefone2) delCli2)
        (submitBtn,_) <- el' "button" (text "Deletar")
        let click = domEvent Click submitBtn
        let resEvt = tag (current prod) click
        _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
            (pure never)
            (fmap decodeXhrResponse <$> performRequestAsync (delCliente <$> resEvt))
        return ()

reqDelPrato :: ( DomBuilder t m
       , Prerender js t m
       ) => m ()
reqDelPrato = do
    elClass "div" "box-deletar" $ do
        elClass "p" "" $ text("EXCLUIR PRATO")
        elClass "p" "" $ text("Id Prato")
        notaPrato2 <- numberInput
        elClass "p" "" $ text("---")
        nomePrato2 <- inputElement def
        elClass "p" "" $ text("---")
        idPrato2 <- numberInput
        elClass "p" "" $ text("---")
        precoPrato2 <- numberInput
        elClass "p" "" $ text("Insira qualquer numero para confirmar a delecao")
        delPrato2 <- numberInput
        let prod = fmap (\((((v,n),i),q),u) -> Prato2 n v i q u) (zipDyn (zipDyn (zipDyn (zipDyn (_inputElement_value nomePrato2) notaPrato2) idPrato2) precoPrato2) delPrato2)
        (submitBtn,_) <- el' "button" (text "Deletar")
        let click = domEvent Click submitBtn
        let resEvt = tag (current prod) click
        _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
            (pure never)
            (fmap decodeXhrResponse <$> performRequestAsync (delPrato <$> resEvt))
        return ()

---------------------------------------------------------------------------------------------------------------------------------------

--DECLARACOES
---------------------------------------------------------------------------------------------------------------------------------------

btns :: (DomBuilder t m, PostBuild t m) => m ()
btns = el "div" $ do
  t <- inputElement def
  s <- inputElement def
  text " "
  dynText $ zipDynWith (<>) (_inputElement_value t)  (_inputElement_value s)

lis :: DomBuilder t m => m ()
lis = el "div" $ do
 el "p" $ text "Items:"
 el "ul" $ do
   el "li" $ text "Item 1"
   el "li" $ text "Item 2"
   el "li" $ text "Item 3"

revText :: T.Text -> T.Text
revText t = T.pack (reverse (T.unpack t))

buttonClick :: DomBuilder t m => m (Event t T.Text)
buttonClick = do
    t <- inputElement def
    (e,_) <- el' "button" (text "OK")
    return $ tagPromptlyDyn (fmap revText (_inputElement_value t)) (domEvent Click e)

numberInput :: (Read a, Num a) => DomBuilder t m => m (Dynamic t a)
numberInput = do
      n <- inputElement $ def
        & inputElementConfig_initialValue .~ "0"
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
      return $ fmap (fromMaybe 0 . readMaybe . T.unpack) $ _inputElement_value n

sumButton :: (DomBuilder t m) => m (Event t Double)
sumButton = do
    n1 <- numberInput
    text " "
    n2 <- numberInput
    text " "
    (e,_) <- el' "button" (text "OK")
    let dynDouble = zipDynWith (+) n1  n2
    return $ attachPromptlyDynWith const dynDouble (domEvent Click e)

sumEvt :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m ()
sumEvt = do
    evt <- sumButton
    s <- holdDyn 0 evt
    el "div" (dynText $ fmap (T.pack . show) s)

bttnEvt :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m ()
bttnEvt = do
    evt <- buttonClick
    hl <-  holdDyn "" evt
    el "div" (dynText hl)

configurarRestaurante :: ( DomBuilder t m
       , Prerender js t m
       ) => m ()
configurarRestaurante = do
    divClass "imagem-restaurante" $ do
        divClass "container" $ do
            reqRestaurante
            reqAlteraRestaurante
            reqDelRest

configurarCliente :: ( DomBuilder t m
       , Prerender js t m
       ) => m ()
configurarCliente = do
    divClass "imagem-cliente" $ do
        divClass "container" $ do
            reqClienteRestaurante
            reqAlteraClienteRestaurante
            reqDelCliente

configurarPrato :: ( DomBuilder t m
       , Prerender js t m
       ) => m ()
configurarPrato = do
    divClass "imagem-prato" $ do
        divClass "container" $ do
            reqPrato
            reqAlteraPrato
            reqDelPrato

data Pagina = Pagina1 | Pagina2 | Pagina3 | Pagina4 | Pagina5 | Pagina6 | Pagina7 | Pagina8 | Pagina9 |  Pagina10

clickLi :: DomBuilder t m => Pagina -> T.Text -> m (Event t Pagina)
clickLi p t = do
    (ev, _) <- el' "li" (elAttr "a" ("href" =: "#") (text t))
    return ((\_ -> p) <$> domEvent Click ev)

menuLi :: (DomBuilder t m, MonadHold t m) => m (Dynamic t Pagina)
menuLi = do
    evs <- el "ul" $ do
        p1 <- clickLi Pagina1 "Configurar Restaurante (INSERIR | EDITAR | DELETAR)"
        p2 <- clickLi Pagina2 "Configurar Cliente (INSERIR | EDITAR | DELETAR)"
        p3 <- clickLi Pagina3 "Configurar Prato (INSERIR | EDITAR | DELETAR)"
        p4 <- clickLi Pagina4 "Listar Restaurantes"
        p5 <- clickLi Pagina5 "Listar Cliente"
        p6 <- clickLi Pagina6 "Listar Prato"

        return (leftmost [p1,p2,p3,p4,p5,p6])
    holdDyn Pagina5 evs

currPag :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, Prerender js t m) => Pagina -> m ()
currPag p =
    case p of
         Pagina1 -> configurarRestaurante
         Pagina2 -> configurarCliente
         Pagina3 -> configurarPrato
         Pagina4 -> reqListaRestaurante
         Pagina5 -> reqListaClienteRestaurante
         Pagina6 -> reqListaPrato



mainPag :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, Prerender js t m) => m ()
mainPag = do
    pag <- el "div" menuLi
    dyn_ $ fmap currPag pag

countClick :: DomBuilder t m => m (Event t Int)
countClick = do
    (ev, _) <- el' "button" (text "+")
    return ((const 1) <$> domEvent Click ev)

pagClick :: (MonadHold t m,
             PostBuild t m,
             DomBuilder t m,
             MonadFix m) => m ()
pagClick = do
        evt <- countClick
        st <- accumDyn (+) 0 evt
        el "div" (dynText (fmap (T.pack . show) st))

footer :: DomBuilder t m => m()
footer = do
    divClass "borda footer" $ do
        elClass "ul" "footer-info-geral" $ do
            elClass "ul" "footer-mesa-livre" $ do
                elClass "h4" "empresa-footer" $ text "Mesa Livre"
                elClass "li" "footer-link" (text "Sobre")
                elClass "li" "footer-link" (text "Contato")
                elClass "li" "footer-link" (text "Suporte")

            elClass "ul" "footer-redes" $ do
                elClass "h4" "empresa-footer" $ text "Redes Sociais"
                elClass "li" "footer-link" (text "Facebook") -- como por imagem aqui
                elClass "li" "footer-link" (text "Twitter") -- como por imagem aqui

frontend :: Frontend (R FrontendRoute)
frontend = Frontend{
_frontend_head = do
      el "title" $ text "Mesa Livre"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "link" ("href" =: static @"css/bootstrap.min.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
--      elAttr "link" ("href" =: static @"css/style.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
, _frontend_body = do

    --elClass "p" "teste" $ text "Testando CSS!"   divClass "" $ do
    -- el "p" $ text $ T.pack commonStuff

    elClass "nav" "navbar navbar-default" $ do
        divClass "container-fluid" $ do
           divClass "container-header" $ do
                elClass "div" "navbar-header" $ do
                    elAttr "button" (
                                 --   "type" =: "button" <>
                                     "class" =: "navbar-toggle collapsed" <>
                                     "data-toggle" =: "collapse" <>
                                     "data-target" =: "#bs-example-navbar-collapse-1" <>
                                     "aria-expand" =: "false") $ do
                         elClass "span" "icon-bar" $ blank
                         elClass "span" "icon-bar" $ blank
                         elClass "span" "icon-bar" $ blank
                    elClass "a" "navbar-brand" $ text "Mesa Livre"
        --divClass "collapse navbar-collapse navbar-right" $ do
        mainPag


    footer


    elAttr "scrpit" ("href" =: static @"js/bootstrap.min.js") blank
    elAttr "script" ("href" =: static @"js/jquery-3.6.0.min.js") blank
    return ()
  }

