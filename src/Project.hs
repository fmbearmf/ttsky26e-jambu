{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

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

data PermutationAction
    = ActKeySetup
    | ActNonceWord
    | ActADFull
    | ActADPart
    | ActEncFull
    | ActEncPart
    | ActDecFull
    | ActDecPart
    | ActFinPart1
    | ActFinPart2
    deriving (Generic, NFDataX, Show, Eq)

data FsmState
    = StateIdle
    | StatePermutation
    | StateOut
    deriving (Generic, NFDataX, Show, Eq)

data JambuState = JambuState
    { fsmState      :: !FsmState
    , permAction    :: !PermutationAction
    , s0            :: !(BitVector 32)
    , s1            :: !(BitVector 32)
    , s2            :: !(BitVector 32)
    , s3            :: !(BitVector 32)
    , k0            :: !(BitVector 32)
    , k1            :: !(BitVector 32)
    , k2            :: !(BitVector 32)
    , k3            :: !(BitVector 32)
    , stepCnt       :: !(Index 33)
    , stepTotal     :: !(Index 33)
    , byteBuf       :: !(BitVector 32)
    , bufCnt        :: !(Index 4)
    , keyByteCnt    :: !(Index 16)
    , noneWordCnt   :: !(Index 4)
    , dataWord      :: !(BitVector 32)
    , partialLen    :: !(Index 4)
    , outBuf        :: !(BitVector 64)
    , outLen        :: !(Index 9)
    , tag0          :: !(BitVector 32)
    } deriving (Generic, NFDataX, Show)

initState :: JambuState
initState = JambuState
    { fsmState      = StateIdle
    , permAction    = ActKeySetup
    , s0            = 0
    , s1            = 0
    , s2            = 0
    , s3            = 0
    , k0            = 0
    , k1            = 0
    , k2            = 0
    , k3            = 0
    , stepCnt       = 0
    , stepTotal     = 0
    , byteBuf       = 0
    , bufCnt        = 0
    , keyByteCnt    = 0
    , noneWordCnt   = 0
    , dataWord      = 0
    , partialLen    = 0
    , outBuf        = 0
    , outLen        = 0
    , tag0          = 0
    }

-- 32 rounds of NLFSR in parallel
step32
    :: (BitVector 32, BitVector 32, BitVector 32, BitVector 32)
    -> BitVector 32
    -> (BitVector 32, BitVector 32, BitVector 32, BitVector 32)
step32 (w0, w1, w2, w3) kWord = (w1, w2, w3, feedback)
    where
        t1 = (w1 `shiftR` 15) .|. (w2 `shiftL` 17)
        t2 = (w2 `shiftR` 6)  .|. (w3 `shiftL` 26)
        t3 = (w2 `shiftR` 21) .|. (w3 `shiftL` 11)
        t4 = (w2 `shiftR` 27) .|. (w3 `shiftL` 5)

        feedback = w0 `xor` t1 `xor` complement (t2 .&. t3) `xor` t4 `xor` kWord

insertByte :: BitVector 32 -> Index 4 -> BitVector 8 -> BitVector 32
insertByte buf 0 b = (buf .&. 0xFFFF_FF00) .|. zeroExtend b
insertByte buf 1 b = (buf .&. 0xFFFF_00FF) .|. (zeroExtend b `shiftL` 8)
insertByte buf 2 b = (buf .&. 0xFF00_FFFF) .|. (zeroExtend b `shiftL` 16)
insertByte buf 3 b = (buf .&. 0x00FF_FFFF) .|. (zeroExtend b `shiftL` 24)
insertByte buf _ _ = buf

maskLen :: Index 4 -> BitVector 32
maskLen 1 = 0x0000_00FF
maskLen 2 = 0x0000_FFFF
maskLen 3 = 0x00FF_FFFF
maskLen _ = 0xFFFF_FFFF

partialLenWord :: Index 4 -> BitVector 32
partialLenWord 1 = 1
partialLenWord 2 = 2
partialLenWord 3 = 3
partialLenWord _ = 0

partialLenOut :: Index 4 -> Index 9
partialLenOut 1 = 1
partialLenOut 2 = 2
partialLenOut 3 = 3
partialLenOut _ = 0

jambuStep
    :: JambuState
    -> (BitVector 8, BitVector 8)
    -> (JambuState, (BitVector 8, BitVector 8))
jambuStep st (uiIn, uioIn) = (nextState, (uoOut, uioOut))
    where
        inValid = (uioIn .&. 0x01) /= 0
        inLast  = (uioIn .&. 0x02) /= 0
        cmd     = (uioIn `shiftR` 2) .&. 0x07

        isReady  = (fsmState st == StateIdle)
        outValid = (fsmState st == StateOut)
        outLast  = outValid && (outLen st == 1)
        uoOut    = if outValid then resize (outBuf st .&. 0xFF) else 0

        uioOut   = (if outLast  then 0x80 else 0)
               .|. (if outValid then 0x40 else 0)
               .|. (if isReady  then 0x20 else 0)

        kWord = case (stepCnt st `mod` 4) of
            0 -> k0 st
            1 -> k1 st
            2 -> k2 st
            _ -> k3 st

        (ns0, ns1, ns2, ns3) = step32 (s0 st, s1 st, s2 st, s3 st) kWord
        isPermutationDone = (stepCnt st + 1 == stepTotal st)

        nextState = case fsmState st of
            StateIdle
                | inValid && cmd == 7 -> initState -- reset command
                | inValid && cmd == 1 -> -- key command
                    let wordIndex = keyByteCnt st `div` 4
                        byteIndex = resize (keyByteCnt st `mod` 4) :: Index 4
                        (nk0, nk1, nk2, nk3) = case wordIndex of
                            0 -> (insertByte (k0 st) byteIndex uiIn, k1 st, k2 st, k3 st)
                            1 -> (k0 st, insertByte (k1 st) byteIndex uiIn, k2 st, k3 st)
                            2 -> (k0 st, k1 st, insertByte (k2 st) byteIndex uiIn, k3 st)
                            _ -> (k0 st, k1 st, k2 st, insertByte (k3 st) byteIndex uiIn)
                    in if keyByteCnt st == 15
                        then st { fsmState   = StatePermutation
                                , permAction = ActKeySetup
                                , k0         = nk0
                                , k1         = nk1
                                , k2         = nk2
                                , k3         = nk3
                                , s0         = 0
                                , s1         = 0
                                , s2         = 0
                                , s3         = 0
                                , stepCnt    = 0
                                , stepTotal  = 32
                                , keyByteCnt = 0
                                }
                        else st { k0 = nk0, k1 = nk1, k2 = nk2, k3 = nk3
                                , keyByteCnt = keyByteCnt st + 1
                                }

                | inValid && cmd == 2 -> -- nonce command
                    let newBuf = insertByte (byteBuf st) (bufCnt st) uiIn
                    in if bufCnt st == 3
                        then st { fsmState   = StatePermutation
                                , permAction = ActNonceWord
                                , s1         = s1 st `xor` 0x10
                                , dataWord   = newBuf
                                , stepCnt    = 0
                                , stepTotal  = 20
                                , bufCnt     = 0
                                }
                        else st { byteBuf = newBuf, bufCnt = bufCnt st + 1 }

                | inValid && cmd == 3 -> -- ad command
                    let newBuf = insertByte (byteBuf st) (bufCnt st) uiIn
                    in if bufCnt st == 3
                        then st { fsmState   = StatePermutation
                                , permAction = ActADFull
                                , s1         = s1 st `xor` 0x30
                                , dataWord   = newBuf
                                , stepCnt    = 0
                                , stepTotal  = 20
                                , bufCnt     = 0
                                }
                        else if inLast
                        then let lenP = bufCnt st + 1
                            in st { fsmState   = StatePermutation
                                  , permAction = ActADPart
                                  , s1         = s1 st `xor` 0x30
                                  , dataWord   = newBuf .&. maskLen lenP
                                  , partialLen = lenP
                                  , stepCnt    = 0
                                  , stepTotal  = 20
                                  , bufCnt     = 0
                                  }
                        else st { byteBuf = newBuf, bufCnt = bufCnt st + 1 }

                | inValid && cmd == 4 -> -- encrypt command
                    let newBuf = insertByte (byteBuf st) (bufCnt st) uiIn
                    in if bufCnt st == 3
                        then st { fsmState   = StatePermutation
                                , permAction = ActEncFull
                                , s1         = s1 st `xor` 0x50
                                , dataWord   = newBuf
                                , stepCnt    = 0
                                , stepTotal  = 32
                                , bufCnt     = 0
                                }
                        else if inLast
                        then let lenP = bufCnt st + 1
                            in st { fsmState   = StatePermutation
                                  , permAction = ActEncPart
                                  , s1         = s1 st `xor` 0x50
                                  , dataWord   = newBuf .&. maskLen lenP
                                  , partialLen = lenP
                                  , stepCnt    = 0
                                  , stepTotal  = 32
                                  , bufCnt     = 0
                                  }
                        else st { byteBuf = newBuf, bufCnt = bufCnt st + 1 }

                | inValid && cmd == 5 -> -- decrypt command
                    let newBuf = insertByte (byteBuf st) (bufCnt st) uiIn
                    in if bufCnt st == 3
                    then st { fsmState   = StatePermutation
                            , permAction = ActDecFull
                            , s1         = s1 st `xor` 0x50
                            , dataWord   = newBuf
                            , stepCnt    = 0
                            , stepTotal  = 32
                            , bufCnt     = 0
                            }
                    else if inLast
                    then let lenP = bufCnt st + 1
                        in st { fsmState   = StatePermutation
                              , permAction = ActDecPart
                              , s1         = s1 st `xor` 0x50
                              , dataWord   = newBuf .&. maskLen lenP
                              , partialLen = lenP
                              , stepCnt    = 0
                              , stepTotal  = 32
                              , bufCnt     = 0
                              }
                    else st { byteBuf = newBuf, bufCnt = bufCnt st + 1 }

                | inValid && cmd == 6 -> -- finalize command
                    st { fsmState   = StatePermutation
                       , permAction = ActFinPart1
                       , s1         = s1 st `xor` 0x70
                       , stepCnt    = 0
                       , stepTotal  = 32
                       }

                | otherwise -> st

            StatePermutation
                | not isPermutationDone ->
                    st { stepCnt = stepCnt st + 1
                       , s0      = ns0
                       , s1      = ns1
                       , s2      = ns2
                       , s3      = ns3
                       }
                | otherwise -> case permAction st of
                    ActKeySetup ->
                        st { fsmState = StateIdle
                           , s0       = ns0
                           , s1       = ns1
                           , s2       = ns2
                           , s3       = ns3
                           }
                    ActNonceWord ->
                        let nextNonceCnt    = noneWordCnt st + 1
                        in st { fsmState    = StateIdle
                              , s0          = ns0
                              , s1          = ns1
                              , s2          = ns2
                              , s3          = ns3 `xor` dataWord st
                              , noneWordCnt = if nextNonceCnt == 3 then 0 else nextNonceCnt
                              }
                    ActADFull ->
                        st { fsmState = StateIdle
                           , s0       = ns0
                           , s1       = ns1
                           , s2       = ns2
                           , s3       = ns3 `xor` dataWord st
                           }
                    ActADPart ->
                        st { fsmState = StateIdle
                           , s0       = ns0
                           , s1       = ns1 `xor` partialLenWord (partialLen st)
                           , s2       = ns2
                           , s3       = ns3 `xor` dataWord st
                           }
                    ActEncFull ->
                        let ctWord = dataWord st `xor` ns2
                        in st { fsmState = StateOut
                              , s0       = ns0
                              , s1       = ns1
                              , s2       = ns2
                              , s3       = ns3 `xor` dataWord st
                              , outBuf   = zeroExtend ctWord
                              , outLen   = 4
                              }
                    ActEncPart ->
                        let ctWord = dataWord st `xor` ns2
                        in st { fsmState = StateOut
                              , s0       = ns0
                              , s1       = ns1 `xor` partialLenWord (partialLen st)
                              , s2       = ns2
                              , s3       = ns3 `xor` dataWord st
                              , outBuf   = zeroExtend ctWord
                              , outLen   = partialLenOut (partialLen st)
                              }
                    ActDecFull ->
                        let ptWord = dataWord st `xor` ns2
                        in st { fsmState = StateOut
                              , s0       = ns0
                              , s1       = ns1
                              , s2       = ns2
                              , s3       = ns3 `xor` ptWord
                              , outBuf   = zeroExtend ptWord
                              , outLen   = 4
                              }
                    ActDecPart ->
                        let ptWord = (dataWord st `xor` ns2) .&. maskLen (partialLen st)
                        in st { fsmState = StateOut
                              , s0       = ns0
                              , s1       = ns1 `xor` partialLenWord (partialLen st)
                              , s2       = ns2
                              , s3       = ns3 `xor` ptWord
                              , outBuf   = zeroExtend ptWord
                              , outLen   = partialLenOut (partialLen st)
                              }
                    ActFinPart1 ->
                        st { fsmState   = StatePermutation
                           , permAction = ActFinPart2
                           , s0         = ns0
                           , s1         = ns1 `xor` 0x70
                           , s2         = ns2
                           , s3         = ns3
                           , tag0       = ns2
                           , stepCnt    = 0
                           , stepTotal  = 20
                           }
                    ActFinPart2 ->
                        let fullTag = (zeroExtend ns2 `shiftL` 32) .|. zeroExtend (tag0 st)
                        in st { fsmState = StateOut
                              , s0       = ns0
                              , s1       = ns1
                              , s2       = ns2
                              , s3       = ns3
                              , outBuf   = fullTag
                              , outLen   = 8
                              }
            StateOut
                | outLen st == 1 -> st { fsmState = StateIdle, outLen = 0 }
                | otherwise      -> st { outLen = outLen st - 1, outBuf = outBuf st `shiftR` 8 }

jambu
    :: (HiddenClockResetEnable TTDom)
    => Signal TTDom (BitVector 8)
    -> Signal TTDom (BitVector 8)
    -> ( Signal TTDom (BitVector 8)
       , Signal TTDom (BitVector 8)
       , Signal TTDom (BitVector 8)
       )
jambu uiIn uioIn = (uoOut, uioOut, pure 0xE0)
    where
        (uoOut, uioOut) = unbundle (mealy jambuStep initState (bundle (uiIn, uioIn)))
