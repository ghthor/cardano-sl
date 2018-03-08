{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
module Cardano.Wallet.API.Request.Sort where

import qualified Prelude
import           Universum

import           Cardano.Wallet.API.Indices
import           Cardano.Wallet.API.Types
import           Cardano.Wallet.API.V1.Types
import           Cardano.Wallet.TypeLits (KnownSymbols, symbolVals)
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Buildable
import           Data.Typeable (Typeable)
import           Formatting (bprint, build, builder, stext, (%))
import qualified Generics.SOP as SOP
import           GHC.TypeLits (Symbol)

import           Network.HTTP.Types (parseQueryText)
import           Network.Wai (Request, rawQueryString)
import           Pos.Core as Core
import           Pos.Util.Servant (HasLoggingServer (..), LoggingApiRec, addParamLogInfo)
import           Serokell.Util.ANSI (Color (..), colorizeDull)
import           Servant
import           Servant.Server.Internal

-- | Represents a sort operation on the data model.
-- Examples:
--   *    `sort_by=balance`.
data SortBy (sym :: [Symbol]) (r :: *) deriving Typeable

-- | The direction for the sort operation.
data SortDirection =
      SortAscending
    | SortDescending
    deriving Show

instance Buildable SortDirection where
    build SortAscending  = "asc."
    build SortDescending = "desc."

-- | A sort operation on an index @ix@ for a resource 'a'.
data SortOperation ix a =
      SortByIndex SortDirection (Proxy ix)
    -- ^ Standard sort by index (e.g. sort_on=balance).
    | SortIdentity
    -- ^ Do not perform sorting on this resource.

instance Show (SortOperation ix a) where
    show (SortByIndex dir _) = "SortByIndex[" <> show dir <> "]"
    show SortIdentity        = "SortIdentity"

-- | A "bag" of sort operations, where the index constraint are captured in
-- the inner closure of 'SortOp'.
data SortOperations a where
    NoSorts  :: SortOperations a
    SortOp   :: (Indexable' a, IsIndexOf' a ix, ToIndex a ix)
             => SortOperation ix a
             -> SortOperations a
             -> SortOperations a

instance Show (SortOperations a) where
    show = show @_ @[Text] . flattenSortOperations show

-- | Handy helper function to convert 'SortOperation'(s) into list.
flattenSortOperations :: (forall ix. SortOperation ix a -> b)
                      -> SortOperations a
                      -> [b]
flattenSortOperations _     NoSorts       = mempty
flattenSortOperations trans (SortOp f fs) = trans f : flattenSortOperations trans fs

-- | This is a slighly boilerplat-y type family which maps symbols to
-- indices, so that we can later on reify them into a list of valid indices.
-- In case we want to sort on _all_ the indices, it might make sense having an
-- entry like the following:
--
--    SortParams '["wallet_id", "balance"] Wallet = IndicesOf Wallet
--
-- In the case of a 'Wallet', for example, sorting by @wallet_id@ doesn't have
-- much sense, so we restrict ourselves.
type family SortParams (syms :: [Symbol]) (r :: *) :: [*] where
    SortParams '["balance"] Wallet = '[Core.Coin]

-- | Handy typeclass to reconcile type and value levels by building a list of 'SortOperation' out of
-- a type level list.
class ToSortOperations (ixs :: [*]) a where
  toSortOperations :: Request -> [Text] -> proxy ixs -> SortOperations a

instance Indexable' a => ToSortOperations ('[]) a where
  toSortOperations _ _ _ = NoSorts

instance ( Indexable' a
         , IsIndexOf' a ix
         , ToIndex a ix
         , ToSortOperations ixs a
         )
         => ToSortOperations (ix ': ixs) a where
  toSortOperations req [] _     =
      let newOp = SortIdentity
      in SortOp (newOp :: SortOperation ix a) (toSortOperations req [] (Proxy :: Proxy ixs))
  toSortOperations req (key:xs) _ =
      case List.lookup "sort_by" (parseQueryText $ rawQueryString req) of
          Nothing       ->
              let newOp = SortIdentity
              in SortOp (newOp :: SortOperation ix a) (toSortOperations req xs (Proxy @ ixs))
          Just Nothing  ->
              let newOp = SortIdentity
              in SortOp (newOp :: SortOperation ix a) (toSortOperations req xs (Proxy @ ixs))
          Just (Just v) ->
              case parseSortOperation (Proxy @a) (Proxy @ix) (key, v) of
                  Left _      ->
                      let newOp = SortIdentity
                      in SortOp (newOp :: SortOperation ix a) (toSortOperations req xs (Proxy @ ixs))
                  Right newOp -> newOp `SortOp` toSortOperations req xs (Proxy @ ixs)

-- | Servant's 'HasServer' instance telling us what to do with a type-level specification of a sort operation.
instance ( HasServer subApi ctx
         , SortParams syms res ~ ixs
         , KnownSymbols syms
         , ToSortOperations ixs res
         , SOP.All (ToIndex res) ixs
         ) => HasServer (SortBy syms res :> subApi) ctx where

    type ServerT (SortBy syms res :> subApi) m = SortOperations res -> ServerT subApi m
    hoistServerWithContext _ ct hoist' s = hoistServerWithContext (Proxy @subApi) ct hoist' . s

    route Proxy context subserver =
        let allParams = map toText $ symbolVals (Proxy @syms)
            delayed = addParameterCheck subserver . withRequest $ \req ->
                        return $ toSortOperations req allParams (Proxy @ixs)

        in route (Proxy :: Proxy subApi) context delayed

instance {-# OVERLAPPING #-}
         ( HasLoggingServer config subApi ctx
         , SortParams syms res ~ ixs
         , KnownSymbols syms
         , ToSortOperations ixs res
         , SOP.All (ToIndex res) ixs
         ) =>
         HasLoggingServer config (SortBy syms res :> subApi) ctx where
    routeWithLog =
        mapRouter @(SortBy syms res :> LoggingApiRec config subApi) route $
            \(paramsInfo, f) sortOps ->
            (updateParamsInfo sortOps paramsInfo, f sortOps)
      where
        updateParamsInfo sortOps =
            addParamLogInfo $ \_sl -> colorizeDull White $ buildSortOps sortOps
        allParams = map toText $ symbolVals (Proxy @syms)
        buildSortOps sortOps =
            let builtOps = flattenSortOperations buildSortOp sortOps
                namedOps = zipWith mergeNameAndOp allParams builtOps
                activeOps = catMaybes namedOps
            in  case activeOps of
                    [] -> "no sort"
                    ss -> pretty . mconcat $ intersperse ", " ss
        buildSortOp sortOp =
            case sortOp of
                SortIdentity           -> Nothing
                SortByIndex sortType _ -> Just $ bprint build sortType
        mergeNameAndOp _ Nothing      = Nothing
        mergeNameAndOp name (Just op) = Just $ bprint (stext%" "%builder) name op

-- | Parse the incoming HTTP query param into a 'SortOperation', failing if the input is not a valid operation.
parseSortOperation :: forall a ix. (ToIndex a ix)
                   => Proxy a
                   -> Proxy ix
                   -> (Text, Text)
                   -> Either Text (SortOperation ix a)
parseSortOperation _ ix@Proxy (key,value) = case parseQuery of
    Nothing -> Left "Not a valid sort."
    Just f  -> Right f
  where
    parseQuery :: Maybe (SortOperation ix a)
    parseQuery =
        let (predicate, rest1) = (T.take 4 value, T.drop 4 value)
            (ixTxt, closing)   = T.breakOn "]" rest1
            in case (predicate, closing, ixTxt == key) of
               ("ASC[", "]", True) -> Just $ SortByIndex SortAscending  ix
               ("DES[", "]", True) -> Just $ SortByIndex SortDescending ix
               (_, _, True)        -> Just $ SortByIndex SortDescending ix -- default sorting.
               _                   -> Nothing
