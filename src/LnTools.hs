{-# LANGUAGE OverloadedStrings #-}

module LnTools (
    NodeId,
    Channel (..),
    Channels (..),
    Payee (..),
) where

import Data.Word (Word64)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=))
import Data.Text (Text)

type NodeId = Text

-- Output from c-lightning 'listchannels'
data Channel = Channel
    { channelId :: Text
    , source :: NodeId
    , destination :: NodeId
    , capacity :: Word64
    }
    deriving (Eq, Show, Ord)

instance FromJSON Channel where
    parseJSON = withObject "Channel" $ \obj ->
        Channel 
            <$> obj .: "short_channel_id"
            <*> obj .: "source"
            <*> obj .: "destination"
            <*> obj .: "satoshis"

-- Output from 'lncli describegraph'
-- data ChannelLND = ChannelLND
--     { channelId :: Word64
--     , node1 :: NodeId
--     , node2 :: NodeId
--     , capacity :: Word64
--     } deriving (Eq, Show, Ord)
-- instance FromJSON Channel where
--     parseJSON = withObject "Channel" $ \obj ->
--         Channel 
--             <$> (read <$> obj .: "channel_id")
--             <*> obj .: "node1_pub"
--             <*> obj .: "node2_pub"
--             <*> (read <$> obj .: "capacity")

-- The program will be supplied with a list of channels
newtype Channels = Channels { unChannels :: [Channel] }
    deriving (Eq, Show)

instance FromJSON Channels where
    parseJSON = withObject "Channels" $ fmap Channels . (.: "channels")

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

