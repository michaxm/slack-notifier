module SendMessage where

import qualified Data.Text as T
import Network (PortID(..), PortNumber)
import Network.Protocol.XMPP
import qualified Data.XML.Types as XML (Element(..), Node(..), Content(..), Name(..))

sendMessage :: String -> String -> String -> Maybe PortID -> String -> String -> IO ()
sendMessage fromAccount fromPassword xmppServer xmppPort toAccount message = 
  let Just fromJID = parseJID (T.pack fromAccount)
      fromUsername = case strNode <$> jidNode fromJID of
        Just x -> x
        Nothing -> error $ "account must include a username"
      server = Server (JID Nothing (jidDomain fromJID) Nothing) xmppServer (maybe (PortNumber 5222) id xmppPort)
      Just toJID = parseJID (T.pack toAccount)
  in do
    res <- runClient server fromJID fromUsername (T.pack fromPassword) $ do
      _ <- bindJID fromJID
      let say = putStanza . mkMsg toJID
        in say $ (T.unwords) [(T.pack message)]
    case res of
     Left err -> error $ "XMPP error:" ++ show err
     Right _  -> return ()


--borrowed from hxmppc
mkMsg :: JID -> T.Text -> Message
mkMsg toJID txt =
  Message { messageType     = MessageNormal
          , messageTo       = Just toJID
          , messageFrom     = Nothing -- done by the server
          , messageID       = Nothing
          , messageLang     = Just $ T.pack "en"
          , messagePayloads = [p txt]
          }

p :: T.Text -> XML.Element
p = pBody . return . contentText

pBody :: [XML.Node] -> XML.Element
pBody = XML.Element (XML.Name (T.pack "body") (Just $ T.pack "jabber:client") Nothing) []

contentText :: T.Text -> XML.Node
contentText = XML.NodeContent . XML.ContentText
