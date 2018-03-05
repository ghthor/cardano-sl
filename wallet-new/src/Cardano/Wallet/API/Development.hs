module Cardano.Wallet.API.Development where

import           Cardano.Wallet.API.Response (ValidJSON, WalletResponse)
import           Cardano.Wallet.API.Types (Tags)
import           Pos.Wallet.Web.Methods.Misc (WalletStateSnapshot)
import           Servant.API.ContentTypes (OctetStream)

import           Servant

type API
    = Tags '["Development"] :>
    (
         "dump-wallet-state"  :> Summary "Dump wallet state."
                              :> Get '[OctetStream] (WalletResponse WalletStateSnapshot)
    :<|> "secret-keys"        :> Summary "Clear wallet state and delete all the secret keys."
                              :> DeleteNoContent '[ValidJSON] NoContent
    )
