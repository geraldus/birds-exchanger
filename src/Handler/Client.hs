module Handler.Client (
      module Verification
    , module Orders
    , module Settings
    , module Stocks
) where

import           Handler.Client.EmailVerification            as Verification

import           Handler.Client.Order.Create                 as Orders
import           Handler.Client.Orders                       as Orders

import           Handler.Client.Settings                     as Settings

import           Handler.Client.Stocks.Purchase              as Stocks
import           Handler.Client.Stocks.Purchase.Confirmation as Stocks
import           Handler.Client.Stocks.Purchase.Details      as Stocks
