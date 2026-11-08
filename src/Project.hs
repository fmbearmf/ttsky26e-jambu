{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Project where

import Clash.Prelude

createDomain vSystem{vName="TTDom", vResetPolarity=ActiveLow}

{-# ANN topEntity
    (Synthesize
        { t_name = "tt_um_jambu_fmbearmf"
        , t_inputs = [ PortName "clk"
                     , PortName "rst_n"
                     , PortName "ena"
                     , PortName "ui_in"
                     , PortName "uio_in"
                     ]
        , t_output = PortProduct ""
                     [ PortName "uo_out"
                     , PortName "uio_out"
                     , PortName "uio_oe"
                     ]
        }) #-}

topEntity
    :: Clock TTDom
    -> Reset TTDom
    -> Enable TTDom
    -> Signal TTDom (BitVector 8)
    -> Signal TTDom (BitVector 8)
    -> ( Signal TTDom (BitVector 8)
       , Signal TTDom (BitVector 8)
       , Signal TTDom (BitVector 8)
       )
topEntity clk rst ena ui_in uio_in =
    exposeClockResetEnable jambu clk rst ena ui_in uio_in

jambu
    :: (HiddenClockResetEnable TTDom)
    => Signal TTDom (BitVector 8)
    -> Signal TTDom (BitVector 8)
    -> ( Signal TTDom (BitVector 8)
       , Signal TTDom (BitVector 8)
       , Signal TTDom (BitVector 8)
       )
jambu uiIn uioIn = (uoOut, pure 0, pure 0)
    where
        uoOut = uiIn + uiIn
