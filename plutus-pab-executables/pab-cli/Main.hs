{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

-- Needed because of a warning of `load SearchPath [...]`
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where

import Cardano.Api qualified as C
import Cardano.Api.NetworkId.Extra (NetworkIdWrapper (NetworkIdWrapper, unNetworkIdWrapper))
import Cardano.ChainIndex.Server (ChainIndexConfig (ciBaseUrl))
import Cardano.ChainIndex.Types (ChainIndexUrl (ChainIndexUrl))
import Cardano.Node.Types (NodeMode (AlonzoNode),
                           PABServerConfig (pscBaseUrl, pscKeptBlocks, pscNetworkId, pscNodeMode, pscSlotConfig, pscSocketPath))
import Cardano.Wallet.Types (LocalWalletSettings (LocalWalletSettings),
                             WalletConfig (LocalWalletConfig, RemoteWalletConfig), WalletUrl (WalletUrl))
import Control.Concurrent.Async (waitAny, withAsync)
import Control.Exception (Exception)
import Control.Monad (forM_, unless, void)
import Control.Monad.Catch (MonadThrow (throwM), catch)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Char (toLower)
import Data.Default (def)
import Data.String (fromString)
import Data.Text qualified as Text
import Data.Yaml (encodeFile)
import Ledger (POSIXTime (POSIXTime), Slot (Slot))
import Ledger.Blockchain (BlockId (BlockId))
import Ledger.TimeSlot (SlotConfig (SlotConfig))
import Plutus.ChainIndex.Types (Point (Point, pointBlockId, pointSlot))
import Plutus.PAB.Types (Config (chainIndexConfig, dbConfig, developmentOptions, nodeServerConfig, pabWebserverConfig, walletServerConfig),
                         DbConfig (dbConfigFile),
                         DevelopmentOptions (DevelopmentOptions, pabResumeFrom, pabRollbackHistory),
                         WebserverConfig (baseUrl))
import Servant.Client (BaseUrl (BaseUrl), Scheme (Http))
import Shh (ExecReference (SearchPath), Stream (Truncate), capture, load, (&>), (|>))
import System.Console.Docopt (Arguments, Docopt, Option, command, docopt, exitWithUsage, exitWithUsageMessage,
                              getArgOrExitWith, getArgWithDefault, isPresent, longOption, parseArgsOrExit)
import System.Directory (Permissions (executable), doesFileExist, getPermissions)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Process (readProcess)
import Text.Read (readMaybe)

-- Specifications:
--
-- - old node socket should be removed when starting the cli
-- - node socket should exist before running chain-index
-- - should throw error if could not fetch cardano config files from hydra
--
-- Issues:
-- - Chain index throws an error when using Ctrl+C

-- Adding plutus-chain-index in the list throws an exception:
-- Attempted to load 'plutus-chain-index', but it is not executable
--
-- So using `cabal` instead.
load SearchPath ["sleep", "mkdir", "date", "jq", "curl", "rm", "cardano-node", "cardano-wallet", "plutus-chain-index"]

patterns :: Docopt
patterns = [docopt|
Hosted PAB CLI. This script allows the user to run the PAB in a hosted scenario, and provides good defaults for common usecases.

THIS IS AN EXPERIMENT! DO NOT USE IN A PRODUCTION ENVIRONNEMENT.

Tested in GNU+Linux operating systems.

For any possible enhancements and suggestions, submit an issue on https://github.com/input-output-hk/plutus-apps/issues.

Usage:
    pab-cli mocknode --wallet-port=<port> [options]
    pab-cli alonzonode wbe  (--mainnet | --testnet) --wallet-port=<port> [options]
    pab-cli alonzonode remotewallet (--mainnet | --testnet) [options]
    pab-cli -h|--help

Options:
    -h --help                     show this
    --pab-exe <executable>        PAB executable with builtin contracts
    --pab-output-dir <dir>        PAB output directory for config, logs, db, etc.
                                  [default: /tmp/pab]
    --pab-port <port>             PAB webserver port number
                                  [default: 9080]
    --chain-index-port <port>     chain index port number
                                  [default: 9083]
    --node-dir <dir>              node output directory config, logs, db, etc.
    --node-port <port>            node port number
                                  [default:9081]
|]

data AppOpts =
    AppOpts { appOptsPabExe         :: PABExe
            , appOptsPabOutputDir   :: PABDirectory
            , appOptsPabPort        :: PABPort
            , appOptsChainIndexPort :: ChainIndexPort
            , appOptsNodeOutputDir  :: NodeDirectory
            , appOptsNodePort       :: NodePort
            , appOptsCommand        :: ConfigCommand
            } deriving (Show)

data ConfigCommand =
    MockNodeCommand WalletPort
  | AlonzoNodeWBECommand NetworkName WalletPort
  | AlonzoNodeRemoteWalletCommand NetworkName
    deriving (Show)

newtype PABExe = PABExe FilePath
    deriving (Show)

newtype NodeDirectory = NodeDirectory FilePath
    deriving (Show)

newtype NodeSocketPath = NodeSocketPath FilePath
    deriving (Show)

newtype NodeDbPath = NodeDbPath FilePath
    deriving (Show)

newtype NodePort = NodePort Int
    deriving (Show)

newtype PABPort = PABPort Int
    deriving (Show)

newtype PABDirectory = PABDirectory FilePath
    deriving (Show)

newtype PABDbPath = PABDbPath FilePath
    deriving (Show)

-- The show instance should output 'Testnet' and 'Mainnet'. Used in 'fetchNodeConfigFiles'.
data NetworkName = Testnet | Mainnet
    deriving (Show)

newtype ChainIndexPort = ChainIndexPort Int
    deriving (Show)

newtype ChainIndexDbPath = ChainIndexDbPath FilePath
    deriving (Show)

newtype WalletPort = WalletPort Int
    deriving (Show)

newtype WBEDbDirectory = WBEDbDirectory FilePath
    deriving (Show)

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns

data CLIError = ChainIndexPortError
              | PABPortError
              | PABExeDoesNotExist
              | PABExeIsNotExecutable
              | NodePortError
              | WalletPortError

instance Exception CLIError

instance Show CLIError where
    show ChainIndexPortError   = "chain index port should be a positive number"
    show PABPortError          = "PAB port should be a positive number"
    show PABExeDoesNotExist    = "PAB executable does not exist"
    show PABExeIsNotExecutable = "PAB executable is not an executable file"
    show NodePortError         = "cardano node port should be a positive number"
    show WalletPortError       = "wallet node port should be a positive number"

argsToAppOpts :: (MonadIO m, MonadThrow m) => Arguments -> m AppOpts
argsToAppOpts args = do
    if args `isPresent` longOption "help"
    then
        liftIO $ exitWithUsage patterns
    else do
        chainIndexPort <- parseChainIndexPort args
        pabPort <- parsePABPort args
        let pabOutputDir =
                PABDirectory $ getArgWithDefault args "/tmp/pab" (longOption "pab-output-dir")
        pabExe@(PABExe pe) <- PABExe <$> liftIO (args `getArgOrExit` longOption "pab-exe")
        liftIO (doesFileExist pe)
            >>= \fileExists -> unless fileExists $ throwM PABExeDoesNotExist
        liftIO (getPermissions pe)
            >>= \filePermissions -> unless (executable filePermissions)
                                           $ throwM PABExeIsNotExecutable
        nodeOutputDir <- NodeDirectory <$> liftIO (args `getArgOrExit` longOption "node-dir")
        nodePort <- parseNodePort args

        if args `isPresent` command "mocknode"
        then do
            walletPort <- parseWalletPort args
            pure $ AppOpts pabExe
                           pabOutputDir
                           pabPort
                           chainIndexPort
                           nodeOutputDir
                           nodePort
                           (MockNodeCommand walletPort)
        else if args `isPresent` command "alonzonode"
        then if args `isPresent` command "wbe"
             then do
               walletPort <- parseWalletPort args
               network <- liftIO $ getNetworkArgs args
               pure $ AppOpts pabExe
                              pabOutputDir
                              pabPort
                              chainIndexPort
                              nodeOutputDir
                              nodePort
                              (AlonzoNodeWBECommand network walletPort)
             else if args `isPresent` command "remotewallet"
             then do
               network <- liftIO $ getNetworkArgs args
               pure $ AppOpts pabExe
                              pabOutputDir
                              pabPort
                              chainIndexPort
                              nodeOutputDir
                              nodePort
                              (AlonzoNodeRemoteWalletCommand network)
             else liftIO $ exitWithUsageMessage patterns "Usage not yet implemented!"
        else liftIO $ exitWithUsageMessage patterns "Usage not yet implemented!"

parsePABPort :: (MonadIO m, MonadThrow m) => Arguments -> m PABPort
parsePABPort  args = do
    pabPortStr <- liftIO $ args `getArgOrExit` longOption "pab-port"
    maybe (throwM PABPortError) (pure . PABPort) $ readMaybe pabPortStr

parseChainIndexPort :: (MonadIO m, MonadThrow m) => Arguments -> m ChainIndexPort
parseChainIndexPort args = do
    chainIndexPortStr <- liftIO $ args `getArgOrExit` longOption "chain-index-port"
    maybe (throwM ChainIndexPortError) (pure . ChainIndexPort) $ readMaybe chainIndexPortStr

parseNodePort :: (MonadIO m, MonadThrow m) => Arguments -> m NodePort
parseNodePort args = do
    nodePortStr <- liftIO $ args `getArgOrExit` longOption "node-port"
    maybe (throwM NodePortError) (pure . NodePort) $ readMaybe nodePortStr

parseWalletPort :: (MonadIO m, MonadThrow m) => Arguments -> m WalletPort
parseWalletPort args = do
    walletPortStr <- liftIO $ args `getArgOrExit` longOption "wallet-port"
    maybe (throwM WalletPortError) (pure . WalletPort) $ readMaybe walletPortStr

getNetworkArgs :: Arguments -> IO NetworkName
getNetworkArgs args
  | args `isPresent` longOption "mainnet" = pure Mainnet
  | args `isPresent` longOption "testnet" = pure Testnet
  | otherwise = exitWithUsageMessage patterns "network not yet implemented"

-- | These file names come from: https://hydra.iohk.io/build/7654130/download/1/index.html
nodeConfigFilenames :: [String]
nodeConfigFilenames =
    [ "config.json"
    , "byron-genesis.json"
    , "shelley-genesis.json"
    , "alonzo-genesis.json"
    , "topology.json"
    ]

nodeSocketPath :: NodeDirectory -> NodeSocketPath
nodeSocketPath (NodeDirectory nodeDir) = NodeSocketPath $ nodeDir <> "/node.sock"

pabDbPath :: PABDirectory -> PABDbPath
pabDbPath (PABDirectory pabDir) = PABDbPath $ pabDir <> "/pab-core.db"

nodeDbPath :: NodeDirectory -> NodeDbPath
nodeDbPath (NodeDirectory nodeDir) = NodeDbPath $ nodeDir <> "/db"

chainIndexDbPath :: NodeDirectory -> ChainIndexDbPath
chainIndexDbPath (NodeDirectory nodeDir) = ChainIndexDbPath $ nodeDir <> "/chain-index.db"

wbeDatabaseDir :: NodeDirectory -> WBEDbDirectory
wbeDatabaseDir (NodeDirectory nodeDir) = WBEDbDirectory $ nodeDir <> "/wbe"

main :: IO ()
main = do
    missingExecs <- missingExecutables
    unless (null missingExecs) $ do
        putStrLn $ "The following executables are missing from your PATH: "
                <> show missingExecs
        exitFailure

    args <- parseArgsOrExit patterns =<< getArgs
    appOpts <- argsToAppOpts args `catch` \(e :: CLIError) -> exitWithUsageMessage patterns (show e)

    -- Create PAB and node output directory
    let PABDirectory pabDir = appOptsPabOutputDir appOpts
    mkdir "-p" pabDir
    let NodeDirectory nodeDir = appOptsNodeOutputDir appOpts
    mkdir "-p" nodeDir
    -- Remote old node socket file
    let NodeSocketPath oldSocketPath = nodeSocketPath (appOptsNodeOutputDir appOpts)
    rm "-f" oldSocketPath

    runPAB appOpts

runPAB :: AppOpts -> IO ()
runPAB appOpts@AppOpts { appOptsPabOutputDir
                       , appOptsPabExe
                       , appOptsNodeOutputDir
                       , appOptsNodePort = NodePort nodePort
                       , appOptsCommand = MockNodeCommand (WalletPort walletPort)
                       } = do
    let (NodeSocketPath socketPath) = nodeSocketPath appOptsNodeOutputDir
        nodeServerConfig = def { pscBaseUrl = BaseUrl Http "localhost" nodePort ""
                               , pscSocketPath = socketPath
                               }

    let walletConfig = LocalWalletConfig
                     $ LocalWalletSettings
                     $ WalletUrl
                     $ BaseUrl Http "localhost" walletPort ""
        pabServerConfig = (pabWebserverBaseConfig nodeServerConfig appOpts)
                { walletServerConfig = walletConfig
                , nodeServerConfig = nodeServerConfig
                }
    startPabWebserverAndMockServers appOptsPabOutputDir appOptsPabExe pabServerConfig

runPAB appOpts@AppOpts { appOptsPabOutputDir
                       , appOptsPabExe
                       , appOptsNodeOutputDir
                       , appOptsNodePort
                       , appOptsChainIndexPort
                       , appOptsCommand = AlonzoNodeWBECommand networkName (WalletPort walletPort)
                       } = do
    -- Fetch node config files depending on network and save to node output directory.
    fetchNodeConfigFiles networkName appOptsNodeOutputDir

    nodeServerConfig <- getNodeConfig networkName appOptsNodeOutputDir
    let networkId = unNetworkIdWrapper $ pscNetworkId nodeServerConfig
    let startChainIndexAction = startChainIndex appOptsChainIndexPort
                                                (nodeSocketPath appOptsNodeOutputDir)
                                                (chainIndexDbPath appOptsNodeOutputDir) -- TODO
                                                networkId
        startCardanoNodeAction = startCardanoNode networkName appOptsNodeOutputDir appOptsNodePort
        startCardanoWalletAction = startCardanoWallet networkName appOptsNodeOutputDir

    let walletConfig = LocalWalletConfig
                     $ LocalWalletSettings
                     $ WalletUrl
                     $ BaseUrl Http "localhost" walletPort ""
        pabServerConfig = (pabWebserverBaseConfig nodeServerConfig appOpts)
                { walletServerConfig = walletConfig
                , nodeServerConfig = nodeServerConfig
                }
        startPabWebserverAction = startPabWebserver appOptsPabOutputDir appOptsPabExe pabServerConfig

    void $ withAsync startChainIndexAction $ \a1 -> do
        withAsync startCardanoNodeAction $ \a2 -> do
            withAsync startPabWebserverAction $ \a3 -> do
                withAsync startCardanoWalletAction $ \a4 -> do
                    waitAny [a1, a2, a3, a4]

runPAB appOpts@AppOpts { appOptsPabOutputDir
                       , appOptsPabExe
                       , appOptsNodeOutputDir
                       , appOptsNodePort
                       , appOptsChainIndexPort
                       , appOptsCommand = AlonzoNodeRemoteWalletCommand network
                       } = do
    -- Fetch node config files depending on network and save to node output directory.
    fetchNodeConfigFiles network appOptsNodeOutputDir

    nodeServerConfig <- getNodeConfig network appOptsNodeOutputDir
    let networkId = unNetworkIdWrapper $ pscNetworkId nodeServerConfig
        networkName = getNetworkName networkId
        startChainIndexAction = startChainIndex appOptsChainIndexPort
                                                (nodeSocketPath appOptsNodeOutputDir)
                                                (chainIndexDbPath appOptsNodeOutputDir)
                                                networkId
        startCardanoNodeAction = startCardanoNode networkName appOptsNodeOutputDir appOptsNodePort

    let pabServerConfig = (pabWebserverBaseConfig nodeServerConfig appOpts)
                { walletServerConfig = RemoteWalletConfig
                , nodeServerConfig = nodeServerConfig
                }
        startPabWebserverAction = startPabWebserver appOptsPabOutputDir appOptsPabExe pabServerConfig

    void $ withAsync startChainIndexAction $ \a1 -> do
        withAsync startCardanoNodeAction $ \a2 -> do
            withAsync startPabWebserverAction $ \a3 -> do
                waitAny [a1, a2, a3]

fetchNodeConfigFiles :: NetworkName -> NodeDirectory -> IO ()
fetchNodeConfigFiles networkName (NodeDirectory nodeDir) = do
    forM_ nodeConfigFilenames $ \partialFname -> do
        let configFname = map toLower (show networkName) <> "-" <> partialFname
            configFpath = nodeDir <> "/" <> configFname
        configFpathExists <- doesFileExist configFpath
        unless configFpathExists $ do
            curl "-s" ("https://hydra.iohk.io/build/7654130/download/1/" <> configFname) &> Truncate (fromString configFpath)

pabWebserverBaseConfig :: PABServerConfig -> AppOpts -> Config
pabWebserverBaseConfig
        nodeServerConfig
        AppOpts { appOptsPabOutputDir
                , appOptsPabPort = PABPort pabPort
                , appOptsChainIndexPort = ChainIndexPort chainIndexPort
                } = do
    let PABDbPath dbConfigFile = pabDbPath appOptsPabOutputDir
        chainIndexUrl = ChainIndexUrl $ BaseUrl Http "localhost" chainIndexPort ""
        pabWebserverConfig =
            def { baseUrl = BaseUrl Http "localhost" pabPort "" }
    def { dbConfig = def { dbConfigFile = Text.pack dbConfigFile }
        , chainIndexConfig = def { ciBaseUrl = chainIndexUrl }
        , nodeServerConfig = nodeServerConfig
        , pabWebserverConfig = pabWebserverConfig
        , developmentOptions =
            DevelopmentOptions
                { pabRollbackHistory = Just 1 -- Temporary in order not to run out of memory
                , pabResumeFrom =
                    Point { pointBlockId = BlockId $ fromString "613d23b8df670e3692f4e4ab59b179778519de15999ee1a51077fcc02a48abaa"

                          , pointSlot = Slot 45656608
                          }
                }
        }

startPabWebserver :: PABDirectory -> PABExe -> Config -> IO ()
startPabWebserver (PABDirectory pabOutputDir) (PABExe pabExe) pabServerConfig = do
    waitForNodeSocket (pscSocketPath $ nodeServerConfig pabServerConfig)
    let pabConfigFpath = pabOutputDir <> "/plutus-pab.yaml"
    encodeFile pabConfigFpath pabServerConfig
    void $ readProcess pabExe ["migrate", "--config", pabConfigFpath] ""
    void $ readProcess pabExe ["webserver", "--config", pabConfigFpath] ""

startPabWebserverAndMockServers :: PABDirectory -> PABExe -> Config -> IO ()
startPabWebserverAndMockServers
  (PABDirectory pabOutputDir) (PABExe pabExe) pabServerConfig = do
    let pabConfigFpath = pabOutputDir <> "/plutus-pab.yaml"
    encodeFile pabConfigFpath pabServerConfig
    void $ readProcess pabExe ["migrate", "--config", pabConfigFpath] ""
    void $ readProcess pabExe ["all-servers", "--config", pabConfigFpath] ""

startCardanoNode :: NetworkName -> NodeDirectory -> NodePort -> IO ()
startCardanoNode networkName (NodeDirectory nodeDir) (NodePort nodePort) = do
    let nodeConfigFpath = nodeDir <> "/" <> map toLower (show networkName) <> "-config.json"
        topologyFpath = nodeDir <> "/" <> map toLower (show networkName) <> "-topology.json"
        databaseFpath = nodeDir <> "/db"
        nodeSocketFpath = nodeDir <> "/node.sock"
    cardano_node "run"
                 "--config"
                 nodeConfigFpath
                 "--topology"
                 topologyFpath
                 "--database-path"
                 databaseFpath
                 "--socket-path"
                 nodeSocketFpath
                 "--port"
                 nodePort

startChainIndex
    :: ChainIndexPort
    -> NodeSocketPath
    -> ChainIndexDbPath
    -> C.NetworkId
    -> IO ()
startChainIndex (ChainIndexPort ciPort)
                (NodeSocketPath socketPath)
                (ChainIndexDbPath ciDbPath)
                networkId = do
    waitForNodeSocket socketPath
    plutus_chain_index
        "--socket-path"
        socketPath
        "--db-path"
        ciDbPath
        "--port"
        (show ciPort)
        "--network-id"
        (show $ C.unNetworkMagic $ C.toNetworkMagic networkId)
        "start-index"

startCardanoWallet :: NetworkName -> NodeDirectory -> IO ()
startCardanoWallet network nodeDir@(NodeDirectory nd) = do
    let (NodeSocketPath socketPath) = nodeSocketPath nodeDir
    waitForNodeSocket socketPath

    let byronGenesisFilePath = nd
                            <> "/"
                            <> fmap toLower (show network)
                            <> "-byron-genesis.json"
        WBEDbDirectory wbeDbDir = wbeDatabaseDir nodeDir
    mkdir "-p" wbeDbDir
    case network of
        Mainnet ->
            cardano_wallet "serve"
                           "--mainnet"
                           "--node-socket" socketPath
                           "--database" wbeDbDir
        Testnet ->
            cardano_wallet "serve"
                           "--testnet" byronGenesisFilePath
                           "--node-socket" socketPath
                           "--database" wbeDbDir

waitForNodeSocket :: FilePath -> IO ()
waitForNodeSocket np = go
  where
      go = do
          nodeSocketExists <- doesFileExist np
          unless nodeSocketExists $ sleep (5 :: Integer) >> go

getNodeConfig :: NetworkName -> NodeDirectory -> IO PABServerConfig
getNodeConfig network nodeDir@(NodeDirectory nd) = do
    let shelleyConfigFpath =
            nd
         <> "/"
         <> (map toLower $ show network)
         <> "-shelley-genesis.json"
    networkMagicM <- fmap (readMaybe . BSL.unpack)
                   $ jq "-r" ".networkMagic" shelleyConfigFpath |> capture
    networkMagic <- maybe (putStrLn "Can't parse .networkMagic from node config" >> exitFailure)
                          pure
                          networkMagicM
    let networkId =
            case network of
                Mainnet -> C.Mainnet
                Testnet -> C.Testnet $ C.NetworkMagic networkMagic
    securityParamM <- fmap (readMaybe . BSL.unpack)
                    $ jq "-r" ".securityParam" shelleyConfigFpath |> capture
    securityParam <- maybe (putStrLn "Can't parse .securityParam from node config" >> exitFailure)
                           pure
                           securityParamM
    slotLengthM <- fmap (fmap (* 1000) . readMaybe . BSL.unpack)
                 $ jq "-r" ".slotLength" shelleyConfigFpath |> capture
    slotLength <- maybe (putStrLn "Can't parse .slotLength from node config" >> exitFailure)
                        pure
                        slotLengthM
    systemStartM <- fmap BSL.unpack $ jq "-r" ".systemStart" shelleyConfigFpath |> capture
    systemStartPosixM <- fmap (readMaybe . BSL.unpack) $ date "-d" systemStartM "+%s" |> capture
    systemStartPosix <- maybe (putStrLn "Can't parse .systemStart from node config" >> exitFailure)
                        pure
                        systemStartPosixM

    let NodeSocketPath socketPath = nodeSocketPath nodeDir
    pure $ def { pscSocketPath = socketPath
               , pscKeptBlocks = securityParam
               , pscSlotConfig = SlotConfig slotLength (POSIXTime systemStartPosix)
               , pscNetworkId = NetworkIdWrapper networkId
               , pscNodeMode = AlonzoNode
               }

getNetworkName :: C.NetworkId -> NetworkName
getNetworkName C.Mainnet    = Mainnet
getNetworkName C.Testnet {} = Testnet
