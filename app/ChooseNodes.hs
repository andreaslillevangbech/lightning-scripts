module Main where

import Control.Exception (throwIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Yaml (decodeFileEither)
import qualified LnTools as L
import LnTools (Payee (..), Channel, NodeId)
import LnTools.Analysis (bestCapacity, bestConnected, shortestPath)
import LnTools.Network (Path, getGraph, parseChannels)

-- Command line options
import Control.Applicative ((<**>))
import qualified Options.Applicative as Opt

main :: IO ()
main = do
    config <- cliConfig
    channels <- fmap L.unChannels . throwEither =<< decodeFileEither (channelFile config)
    payees <- throwEither =<< decodeFileEither (payeeFile config)
    let out = analysis (maxJumps config) payees channels
    putStr . showResult $ out
        where throwEither = either throwIO pure


analysis :: Int -> [Payee] -> [Channel] -> Map NodeId (Map NodeId Path)
analysis maxJumps payees channels =
  shortestPath payees . bestCapacity . bestConnected $
  getGraph maxJumps (parseChannels channels) (node <$> payees)

showResult :: Map NodeId (Map NodeId Path) -> String
showResult out = 
    unlines . mconcat $ 
        [
            [ "Count: " <> show (Map.size out)
            , "Nodes:"
            ]
        , ("  - " <>) <$> Map.keys out
        ]

-- Command line options
data CliConfig = CliConfig
    { channelFile :: FilePath
    , payeeFile :: FilePath
    , maxJumps :: Int
    }
    deriving (Eq, Show)

cliConfig :: IO CliConfig
cliConfig = Opt.execParser $ Opt.info (opts <**> Opt.helper) desc
    where
        opts = CliConfig <$> channelOpt <*> payeeOpt <*> jumpsOpt
        desc = Opt.progDesc "Find nodes with good connection to potential payees"
        channelOpt = Opt.strOption $
            Opt.short 'c'
            <> Opt.long "channels"
            <> Opt.metavar "FILE"
            <> Opt.value defaultChannelFile
            <> Opt.help "Supply list of channels with FILE"
        payeeOpt = Opt.strOption $
            Opt.short 'p'
            <> Opt.long "payees"
            <> Opt.metavar "FILE"
            <> Opt.value defaultPayeeFile
            <> Opt.help "Supply list of payees with FILE"
        jumpsOpt = Opt.option Opt.auto $
            Opt.short 'j'
            <> Opt.long "max-jumps"
            <> Opt.metavar "K"
            <> Opt.value defaultJumps
            <> Opt.help "Maximum number of jumps K to get to payee"

        defaultChannelFile = "channels.json"
        defaultPayeeFile = "payees.yaml"
        defaultJumps = 3


