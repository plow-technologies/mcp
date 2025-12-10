{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : MCP.Server.StdIO
Description : MCP server implementation for stdin/stdout communication
Copyright   : (C) 2025 Matthias Pall Gissurarson
License     : MIT
Maintainer  : mpg@mpg.is
Stability   : experimental
Portability : GHC

This module provides MCP server implementation for stdin/stdout streams.
-}
module MCP.Server.StdIO (
    -- * Server Runner
    runServer,
    ServerConfig (..),
) where

import Control.Exception (catch, throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.State.Strict (get, put)
import Data.Aeson (decode, fromJSON, object, toJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as LBS
import Data.Functor.Contravariant (contramap)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Plow.Logging (IOTracer, traceWith)
import System.IO.Error (isEOFError)

import MCP.Protocol
import MCP.Server (MCPServer (..), MCPServerM, ServerConfig (..), ServerState (..), initialServerState, runMCPServer, sendError, sendResponse)
import MCP.Trace.Protocol (ProtocolTrace (..))
import MCP.Trace.Server (ServerTrace (..))
import MCP.Trace.StdIO (StdIOTrace (..))
import MCP.Types

-- | Convert a RequestId to Text for tracing
requestIdToText :: RequestId -> T.Text
requestIdToText (RequestId val) = T.pack (show val)

-- | Handle an incoming JSON-RPC message
handleMessage :: (MCPServer MCPServerM) => IOTracer StdIOTrace -> BSC.ByteString -> MCPServerM (Maybe ())
handleMessage tracer input = do
    let protoTracer = contramap StdIOProtocol tracer
        serverTracer = contramap StdIOServer tracer
    case decode (LBS.fromStrict input) :: Maybe JSONRPCMessage of
        Nothing -> do
            -- Trace parse error
            let inputText = TE.decodeUtf8With TEE.lenientDecode (LBS.toStrict $ LBS.take 100 $ LBS.fromStrict input)
            liftIO $ traceWith protoTracer $ ProtocolParseError "Failed to decode JSON-RPC message" (Just inputText)
            config <- ask
            sendError (configOutput config) (RequestId (toJSON ("unknown" :: T.Text))) $
                JSONRPCErrorInfo (-32700) "Parse error" Nothing
            return Nothing
        Just msg -> case msg of
            RequestMessage req -> do
                -- Trace request received
                let JSONRPCRequest _ reqId method _ = req
                liftIO $ traceWith protoTracer $ ProtocolRequestReceived (requestIdToText reqId) method
                handleRequest protoTracer serverTracer req
                return (Just ())
            NotificationMessage notif -> do
                -- Trace notification received
                let JSONRPCNotification _ method _ = notif
                liftIO $ traceWith protoTracer $ ProtocolNotificationReceived method
                handleNotification notif
                return (Just ())
            _ -> do
                config <- ask
                sendError (configOutput config) (RequestId (toJSON ("unknown" :: T.Text))) $
                    JSONRPCErrorInfo (-32600) "Invalid Request" Nothing
                return Nothing

-- | Handle a JSON-RPC request
handleRequest :: (MCPServer MCPServerM) => IOTracer ProtocolTrace -> IOTracer ServerTrace -> JSONRPCRequest -> MCPServerM ()
handleRequest protoTracer serverTracer (JSONRPCRequest _ reqId method params) = do
    config <- ask
    state <- get

    case method of
        "initialize" -> case params of
            Just p -> case fromJSON p of
                Aeson.Success initParams -> handleInitialize serverTracer reqId initParams
                Aeson.Error e ->
                    sendError (configOutput config) reqId $
                        JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
            Nothing ->
                sendError (configOutput config) reqId $
                    JSONRPCErrorInfo (-32602) "Missing params" Nothing
        "ping" -> handlePing reqId
        "resources/list" -> do
            if not (serverInitialized state)
                then
                    sendError (configOutput config) reqId $
                        JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success listParams -> do
                            result <- handleListResources listParams
                            sendResponse (configOutput config) reqId result
                        Aeson.Error e ->
                            sendError (configOutput config) reqId $
                                JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing -> do
                        result <- handleListResources (ListResourcesParams Nothing)
                        sendResponse (configOutput config) reqId result
        "resources/read" -> do
            if not (serverInitialized state)
                then
                    sendError (configOutput config) reqId $
                        JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success readParams -> do
                            result <- handleReadResource readParams
                            sendResponse (configOutput config) reqId result
                        Aeson.Error e ->
                            sendError (configOutput config) reqId $
                                JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing ->
                        sendError (configOutput config) reqId $
                            JSONRPCErrorInfo (-32602) "Missing params" Nothing
        "resources/templates/list" -> do
            if not (serverInitialized state)
                then
                    sendError (configOutput config) reqId $
                        JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success listParams -> do
                            result <- handleListResourceTemplates listParams
                            sendResponse (configOutput config) reqId result
                        Aeson.Error e ->
                            sendError (configOutput config) reqId $
                                JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing -> do
                        result <- handleListResourceTemplates (ListResourceTemplatesParams Nothing)
                        sendResponse (configOutput config) reqId result
        "prompts/list" -> do
            if not (serverInitialized state)
                then
                    sendError (configOutput config) reqId $
                        JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success listParams -> do
                            result <- handleListPrompts listParams
                            sendResponse (configOutput config) reqId result
                        Aeson.Error e ->
                            sendError (configOutput config) reqId $
                                JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing -> do
                        result <- handleListPrompts (ListPromptsParams Nothing)
                        sendResponse (configOutput config) reqId result
        "prompts/get" -> do
            if not (serverInitialized state)
                then
                    sendError (configOutput config) reqId $
                        JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success getParams -> do
                            result <- handleGetPrompt getParams
                            sendResponse (configOutput config) reqId result
                        Aeson.Error e ->
                            sendError (configOutput config) reqId $
                                JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing ->
                        sendError (configOutput config) reqId $
                            JSONRPCErrorInfo (-32602) "Missing params" Nothing
        "tools/list" -> do
            if not (serverInitialized state)
                then
                    sendError (configOutput config) reqId $
                        JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success listParams -> do
                            result <- handleListTools listParams
                            sendResponse (configOutput config) reqId result
                        Aeson.Error e ->
                            sendError (configOutput config) reqId $
                                JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing -> do
                        result <- handleListTools (ListToolsParams Nothing)
                        sendResponse (configOutput config) reqId result
        "tools/call" -> do
            if not (serverInitialized state)
                then
                    sendError (configOutput config) reqId $
                        JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success callParams -> do
                            result <- handleCallTool callParams
                            sendResponse (configOutput config) reqId result
                        Aeson.Error e ->
                            sendError (configOutput config) reqId $
                                JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing ->
                        sendError (configOutput config) reqId $
                            JSONRPCErrorInfo (-32602) "Missing params" Nothing
        "completion/complete" -> do
            if not (serverInitialized state)
                then
                    sendError (configOutput config) reqId $
                        JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success completeParams -> do
                            result <- handleComplete completeParams
                            sendResponse (configOutput config) reqId result
                        Aeson.Error e ->
                            sendError (configOutput config) reqId $
                                JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing ->
                        sendError (configOutput config) reqId $
                            JSONRPCErrorInfo (-32602) "Missing params" Nothing
        "logging/setLevel" -> case params of
            Just p -> case fromJSON p of
                Aeson.Success setLevelParams -> do
                    _ <- handleSetLevel setLevelParams
                    -- SetLevel response is just an empty object
                    sendResponse (configOutput config) reqId (object [])
                Aeson.Error e ->
                    sendError (configOutput config) reqId $
                        JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
            Nothing ->
                sendError (configOutput config) reqId $
                    JSONRPCErrorInfo (-32602) "Missing params" Nothing
        _ -> do
            -- Trace method not found
            liftIO $ traceWith protoTracer $ ProtocolMethodNotFound (requestIdToText reqId) method
            sendError (configOutput config) reqId $
                JSONRPCErrorInfo (-32601) "Method not found" Nothing

handleInitialize :: IOTracer ServerTrace -> RequestId -> InitializeParams -> MCPServerM ()
handleInitialize tracer reqId params = do
    config <- ask
    state <- get

    let InitializeParams{capabilities = clientCaps, clientInfo = clientImpl} = params

    put
        state
            { serverInitialized = True
            , clientCapabilities = Just clientCaps
            , serverInfo = Just (configServerInfo config)
            }

    -- Emit ServerInitialized trace
    let Implementation clientName _ _ = clientImpl
    liftIO $ traceWith tracer (ServerInitialized (Just clientName))

    -- Emit ServerCapabilityNegotiation trace
    let caps = extractCapabilityNames (serverCapabilities state)
    liftIO $ traceWith tracer (ServerCapabilityNegotiation caps)

    let result =
            InitializeResult
                { protocolVersion = mcpProtocolVersion
                , capabilities = serverCapabilities state
                , serverInfo = configServerInfo config
                , instructions = Nothing
                , _meta = Nothing
                }

    sendResponse (configOutput config) reqId result

-- | Extract capability names from ServerCapabilities
extractCapabilityNames :: ServerCapabilities -> [T.Text]
extractCapabilityNames (ServerCapabilities res prpts tls comps logCap _exp) =
    concat
        [ maybe [] (const ["resources"]) res
        , maybe [] (const ["prompts"]) prpts
        , maybe [] (const ["tools"]) tls
        , maybe [] (const ["completions"]) comps
        , maybe [] (const ["logging"]) logCap
        ]

handlePing :: RequestId -> MCPServerM ()
handlePing reqId = do
    config <- ask
    -- Ping response is just an empty object in MCP
    sendResponse (configOutput config) reqId (object [])

-- | Handle a JSON-RPC notification
handleNotification :: JSONRPCNotification -> MCPServerM ()
handleNotification _ = do
    return ()

-- | Run the MCP server with the given configuration
runServer :: (MCPServer MCPServerM) => ServerConfig -> IOTracer StdIOTrace -> IO ()
runServer config tracer = do
    let initialState = initialServerState (configCapabilities config)

    -- Emit ServerInit trace
    let Implementation{name = serverName, version = serverVersion} = configServerInfo config
    traceWith (contramap StdIOServer tracer) $ ServerInit serverName serverVersion

    let loop = do
            eofOrLine <-
                liftIO $
                    catch
                        (Right <$> BSC.hGetLine (configInput config))
                        (\e -> if isEOFError e then return (Left ()) else throwIO e)
            case eofOrLine of
                Left () -> return () -- EOF reached, exit gracefully
                Right line -> do
                    result <- handleMessage tracer line
                    case result of
                        Just () -> loop
                        Nothing -> return ()

    result <- runMCPServer config initialState loop

    -- Emit ServerShutdown trace
    traceWith (contramap StdIOServer tracer) ServerShutdown

    case result of
        Left err -> putStrLn $ "Server error: " ++ T.unpack err
        Right _ -> return () -- Don't print "Server terminated" for clean EOF
