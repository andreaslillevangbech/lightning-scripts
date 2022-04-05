{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (throwIO)
import Data.Map.Lazy (Map)
import qualified Data.Map.Strict as Map
import Data.Yaml (decodeFileEither)
import qualified LnTools as L
import LnTools (Payee (..), Channel, NodeId)
import LnTools.Analysis (bestCapacity, bestConnected, shortestPath)
import LnTools.Network (Path, getGraph, parseChannels, Graph (..))
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Text (Text)

-- Command line options
import Control.Applicative ((<**>))
import qualified Options.Applicative as Opt

import Debug.Trace

main :: IO ()
main = do
    config <- cliConfig
    channels <- fmap L.unChannels . throwEither =<< decodeFileEither (channelFile config)
    payees <- throwEither =<< decodeFileEither (payeeFile config)
    let out = analysis (maxJumps config) payees channels
    TIO.putStr . showResult $ out
        where throwEither = either throwIO pure

analysis :: 
    Int -> 
    [Payee] -> 
    [Channel] -> 
    Map NodeId (Map NodeId Path)
analysis maxJumps payees channels =
  shortestPath payees 
    . (trace <$> (<>) "full capacity:\n" . T.unpack . T.unlines . Map.keys <*> id)
    . bestCapacity 
    . (trace <$> (<>) "best connected:\n" . show . length . Map.keys <*> id)
    . bestConnected 
    . (trace <$> (<>) "Max Sizes: " . show . maximum . map Map.size . Map.elems <*> id)
    . theGraph
    $ getGraph maxJumps (parseChannels channels) (node <$> payees)

showResult :: Map NodeId (Map NodeId Path) -> Text
showResult out = let count = Map.size out in
    if count == 0 then "No nodes with paths to all Payees\n" else
    T.unlines . mconcat $ 
        [
            [ "Result count: " <> (T.pack . show $ count)
            , "Path info: " <> (T.pack . show $ Map.elems out)
            , "Best nodes:"
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
        defaultJumps = 4
