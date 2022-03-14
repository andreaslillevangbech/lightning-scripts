{-# LANGUAGE OverloadedStrings #-}

module LnTools (
    NodeId,
    Channel (..),
    Channels (..),
    Payee (..),
) where

import Data.Word (Word64)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=))

type NodeId = String

-- Output from 'lncli describegraph'
data Channel = Channel
    { channelId :: Word64
    , node1 :: NodeId
    , node2 :: NodeId
    , capacity :: Word64
    } deriving Show

instance Eq Channel where
    (==) a b = channelId a == channelId b

instance Ord Channel where
    compare a b = compare (channelId a) (channelId b)

instance FromJSON Channel where
    parseJSON = withObject "Channel" $ \obj ->
        Channel 
            <$> (read <$> obj .: "channel_id")
            <*> obj .: "node1_pub"
            <*> obj .: "node2_pub"
            <*> (read <$> obj .: "capacity")

-- The program will be supplied with a list of channels
newtype Channels = Channels { unChannels :: [Channel] }
    deriving (Eq, Show)

instance FromJSON Channels where
    parseJSON = withObject "Channels" $ fmap Channels . (.: "edges")

-- For each node we want to pay, give a weight with the priority of paying that node
data Payee = Payee
    { node :: NodeId
    , weight :: Double 
    } deriving (Eq, Show)

instance FromJSON Payee where
    parseJSON = withObject "Payee" $ \obj ->
        Payee
            <$> obj .: "node_id"
            <*> obj .: "weight"

