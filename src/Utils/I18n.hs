module Utils.I18n (
    transferMethodMsg
) where

import           Import

import           Local.Persist.TransferMethod


transferMethodMsg :: TransferMethod -> AppMessage
transferMethodMsg (CryptoTM c) = MsgTMCryptoTransfer c
transferMethodMsg (FiatTM m _) = case m of
    SberBankCard2CardFTM    -> MsgTMSberBankCard2Card
    AlphaBankCard2CardFTM   -> MsgTMAlphaBankCard2Card
    TinkoffBankCard2CardFTM -> MsgTMTinkoffBankCard2Card
    PayPalTransferFTM       -> MsgTMPayPalTransfer
    QiwiFTM                 -> MsgTMQiwi
