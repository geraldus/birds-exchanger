module Type.Fee where


import Prelude


data Fee
    = Percent Double -- ^ example 100% = Percent 100.0
    | CentsFixed Int