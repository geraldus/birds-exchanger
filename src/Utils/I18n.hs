module Utils.I18n (
    transferMethodMsg
) where

import Import

import Local.Persist.Currency


transferMethodMsg :: TransferMethod -> AppMessage
transferMethodMsg (CryptoTM _) = MsgTMCryptoTransfer
transferMethodMsg (FiatTM m _) = case m of
    SberBankCard2CardFTM -> MsgTMSberBankCard2Card
    AlphaBankCard2CardFTM -> MsgTMAlphaBankCard2Card
    TinkoffBankCard2CardFTM -> MsgTMTinkoffBankCard2Card
    PayPalTransferFTM -> MsgTMPayPalTransfer
    QiwiFTM -> MsgTMQiwi