import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Data.Aeson as A
import Data.Maybe
import Data.Sequence as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Control.Concurrent.MVar
import qualified Data.HashMap.Strict as M
import Text.Blaze.Html.Renderer.Text
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import GHC.Generics
import Network.Wai.Middleware.Static
import Web.Scotty as S
import Control.Monad.IO.Class
import Network.HTTP.Types as HT
import Control.Applicative
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8

data Player = Black | White deriving (Eq, Show, Generic)
other :: Player -> Player
other Black = White
other White = Black

-- front of the list is the top of the list
type Column = Seq (Maybe Player)
type Board = Seq Column

ncolumns = 7
nrows = 6
connect = 4

defaultBoard :: Board
defaultBoard = S.replicate ncolumns (S.replicate nrows Nothing)

data Game = Game {currentTurn :: Player, board :: Board, winner :: Maybe Player} deriving (Show, Generic)
defaultGame = Game {currentTurn = White , board = defaultBoard, winner = Nothing}

newtype Move = Move Int deriving (Generic, Show)
type Coord = (Int, Int)

instance ToJSON Player
instance ToJSON Game
instance FromJSON Move
instance ToJSON Move

instance FromJSON a => FromJSON (Seq a) where
        parseJSON v = S.fromList <$> parseJSON v

instance ToJSON a => ToJSON (Seq a) where
        toJSON v = toJSON (F.toList v)

play :: Player -> Move -> Game -> Game
play _ _ g@(Game {winner=(Just _)}) = g
play p _ g@(Game {currentTurn=cT}) | cT /= p = g
play _ m g@(Game {currentTurn=p, board=board}) = let nextBoard = addMove board m p
                                                     nextPlayer = if nextBoard == board then p else other p in
                                                     checkWinner (g {board = nextBoard, currentTurn = nextPlayer})

checkWinner :: Game -> Game
checkWinner w@(Game {winner=(Just _)})  = w
checkWinner g@(Game {board = b}) = let spaces = [(x,y) | x <- [0..6], y <- [0..5]]
                                       maybeWinner = F.msum $ F.msum $ fmap (fmap (allSameColor . fmap (nestedLookup b)) . fourFromSpot) spaces in
                                       g {winner = maybeWinner}

allSameColor :: Seq (Maybe Player) -> Maybe Player
allSameColor (S.viewl -> xxs@(x@(Just _) :< _)) | xxs == S.viewl (S.replicate connect x) = x
allSameColor _ = Nothing

nestedLookup :: Board -> Coord -> Maybe Player
nestedLookup b (x,y) | x < 0 || y < 0 || S.length b < x + 1 || S.length (index b x) < y + 1 = Nothing
                     | otherwise = index (index b x) y
directions :: Seq (Int, Int)
directions = S.fromList [(dx,dy) | dx <- [-1,0,1], dy <- [-1,0,1], (dx,dy) /= (0,0)]

fourFromSpot :: Coord -> Seq (Seq Coord)
fourFromSpot (x,y) = fmap (\(dx,dy) -> S.fromList [(x+dx*f,y+dy*f) | f <- [0..3]]) directions

addMove :: Board -> Move -> Player -> Board
addMove b (Move c) p = adjust (\s -> let (nothings, justs) = spanl isNothing s in
                                         case S.viewl nothings of
                                             EmptyL -> justs
                                             _ :< rest -> rest >< Just p <| justs) c b

htmlForGameId :: Int -> Player -> H.Html
htmlForGameId gId controller =
        let gameConfig = LC8.unpack . A.encode . toJSON $ M.fromList [(T.pack "gameId",toJSON gId), (T.pack "controller",toJSON controller)] :: String in
            H.html $ do
                H.head $
                    H.style "body {background-color: #fdf6e3}"
                H.body $ do
                    H.div H.! HA.id "app" $ ""
                    H.script H.! HA.src "http://fb.me/react-0.9.0.js" $ ""
                    H.script H.! HA.src "out/goog/base.js" H.! HA.type_ "text/javascript" $ ""
                    H.script H.! HA.src "connect4.js" H.! HA.type_ "text/javascript" $ ""
                    H.script H.! HA.type_ "text/javascript" $ H.toHtml $ "window.gameConfig = " ++ gameConfig ++ ";"
                    H.script H.! HA.type_ "text/javascript" $ "goog.require(\"connect4.core\")"

instance S.Parsable Player where
        parseParam "Black" = return Black
        parseParam "White" = return White
        parseParam e = Left e
        

main = do
    db <- newMVar (0, M.empty)
    scotty 3000 $ do
        middleware static
        get "/" $ do
            (n, gameMap) <- liftIO $ takeMVar db
            let newId = succ n :: Int
            liftIO $ putMVar db (newId, M.insert newId defaultGame gameMap)
            redirect $ TL.pack $ "/" ++ show newId
        get "/:gameId" $ do
            gameId <- S.param "gameId"
            currentPlayer <- liftIO $ readMVar db >>= \(_, gM) -> return . fmap currentTurn $ M.lookup gameId gM 
            case currentPlayer of
                Nothing -> redirect "/"
                Just c -> S.html . renderHtml $ htmlForGameId gameId c
        get "/rest/:gameId" $ do
            gameId <- S.param "gameId"
            (_, games) <- liftIO $ readMVar db
            S.json $ M.lookup gameId games
        post "/rest/:gameId/:controller" $ do
            gameId <- S.param "gameId"
            controller <- S.param "controller"
            [move] <- jsonData
            (c, games) <- liftIO $ takeMVar db
            case play controller move <$> M.lookup gameId games of
                Nothing -> do
                    liftIO $ putMVar db (c, games)
                    status . HT.mkStatus 400 . C8.pack $ "No game found for gameId " ++ show gameId
                Just game' -> liftIO (putMVar db (c, M.insert gameId game' games)) >> S.json game'
