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
    , bitCnt        :: !(Unsigned 5)
    , byteBuf       :: !(BitVector 32)
    , bufCnt        :: !(Index 4)
    , keyByteCnt    :: !(Index 16)
    , noneWordCnt   :: !(Index 4)
    , partialLen    :: !(Index 4)
    , outLen        :: !(Index 5)
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
    , bitCnt        = 0
    , byteBuf       = 0
    , bufCnt        = 0
    , keyByteCnt    = 0
    , noneWordCnt   = 0
    , partialLen    = 0
    , outLen        = 0
    }

stepBit
    :: (BitVector 32, BitVector 32, BitVector 32, BitVector 32)
    -> BitVector 1
    -> (BitVector 32, BitVector 32, BitVector 32, BitVector 32)
stepBit (w0, w1, w2, w3) keyBit = (n0, n1, n2, n3)
  where
    fb = (w0 `xor` (w1 `shiftR` 15)
          `xor` complement ((w2 `shiftR` 6) .&. (w2 `shiftR` 21))
          `xor` (w2 `shiftR` 27)
          `xor` zeroExtend keyBit) .&. 1
    n0 = (w0 `shiftR` 1) .|. (w1 `shiftL` 31)
    n1 = (w1 `shiftR` 1) .|. (w2 `shiftL` 31)
    n2 = (w2 `shiftR` 1) .|. (w3 `shiftL` 31)
    n3 = (w3 `shiftR` 1) .|. (fb `shiftL` 31)

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

partialLenOut :: Index 4 -> Index 5
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
        outLast  = outValid && (outLen st == 1) && (permAction st /= ActFinPart1)
        uoOut    = if outValid then resize (byteBuf st .&. 0xFF) else 0

        uioOut   = (if outLast  then 0x80 else 0)
               .|. (if outValid then 0x40 else 0)
               .|. (if isReady  then 0x20 else 0)

        keyBit :: BitVector 1
        keyBit = case (stepCnt st `mod` 4) of
            0 -> truncateB (k0 st `shiftR` fromIntegral (bitCnt st))
            1 -> truncateB (k1 st `shiftR` fromIntegral (bitCnt st))
            2 -> truncateB (k2 st `shiftR` fromIntegral (bitCnt st))
            _ -> truncateB (k3 st `shiftR` fromIntegral (bitCnt st))

        (ns0, ns1, ns2, ns3) = stepBit (s0 st, s1 st, s2 st, s3 st) keyBit

        wordDone = bitCnt st == 31
        longPerm = case permAction st of
            ActKeySetup -> True
            ActEncFull  -> True
            ActEncPart  -> True
            ActDecFull  -> True
            ActDecPart  -> True
            ActFinPart1 -> True
            _           -> False
        isPermutationDone = wordDone &&
            (if longPerm then stepCnt st == 31 else stepCnt st == 19)

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
                                , bitCnt     = 0
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
                                , byteBuf    = newBuf
                                , stepCnt    = 0
                                , bitCnt     = 0
                                , bufCnt     = 0
                                }
                        else st { byteBuf = newBuf, bufCnt = bufCnt st + 1 }

                | inValid && cmd == 3 -> -- ad command
                    let newBuf = insertByte (byteBuf st) (bufCnt st) uiIn
                    in if bufCnt st == 3
                        then st { fsmState   = StatePermutation
                                , permAction = ActADFull
                                , s1         = s1 st `xor` 0x30
                                , byteBuf    = newBuf
                                , stepCnt    = 0
                                , bitCnt     = 0
                                , bufCnt     = 0
                                }
                        else if inLast
                        then let lenP = bufCnt st + 1
                            in st { fsmState   = StatePermutation
                                  , permAction = ActADPart
                                  , s1         = s1 st `xor` 0x30
                                  , byteBuf    = newBuf .&. maskLen lenP
                                  , partialLen = lenP
                                  , stepCnt    = 0
                                , bitCnt     = 0
                                  , bufCnt     = 0
                                  }
                        else st { byteBuf = newBuf, bufCnt = bufCnt st + 1 }

                | inValid && cmd == 4 -> -- encrypt command
                    let newBuf = insertByte (byteBuf st) (bufCnt st) uiIn
                    in if bufCnt st == 3
                        then st { fsmState   = StatePermutation
                                , permAction = ActEncFull
                                , s1         = s1 st `xor` 0x50
                                , byteBuf    = newBuf
                                , stepCnt    = 0
                                , bitCnt     = 0
                                , bufCnt     = 0
                                }
                        else if inLast
                        then let lenP = bufCnt st + 1
                            in st { fsmState   = StatePermutation
                                  , permAction = ActEncPart
                                  , s1         = s1 st `xor` 0x50
                                  , byteBuf    = newBuf .&. maskLen lenP
                                  , partialLen = lenP
                                  , stepCnt    = 0
                                , bitCnt     = 0
                                  , bufCnt     = 0
                                  }
                        else st { byteBuf = newBuf, bufCnt = bufCnt st + 1 }

                | inValid && cmd == 5 -> -- decrypt command
                    let newBuf = insertByte (byteBuf st) (bufCnt st) uiIn
                    in if bufCnt st == 3
                    then st { fsmState   = StatePermutation
                            , permAction = ActDecFull
                            , s1         = s1 st `xor` 0x50
                            , byteBuf    = newBuf
                            , stepCnt    = 0
                                  , bitCnt     = 0

                            , bufCnt     = 0
                            }
                    else if inLast
                    then let lenP = bufCnt st + 1
                        in st { fsmState   = StatePermutation
                              , permAction = ActDecPart
                              , s1         = s1 st `xor` 0x50
                              , byteBuf    = newBuf .&. maskLen lenP
                              , partialLen = lenP
                              , stepCnt    = 0
                                  , bitCnt     = 0

                              , bufCnt     = 0
                              }
                    else st { byteBuf = newBuf, bufCnt = bufCnt st + 1 }

                | inValid && cmd == 6 -> -- finalize command
                    st { fsmState   = StatePermutation
                       , permAction = ActFinPart1
                       , s1         = s1 st `xor` 0x70
                       , stepCnt    = 0
                                  , bitCnt     = 0

                       }

                | otherwise -> st

            StatePermutation ->
                let done  = isPermutationDone
                    act   = permAction st
                    isAD  = act == ActADFull || act == ActADPart
                    isEnc = act == ActEncFull || act == ActEncPart
                    isDec = act == ActDecFull || act == ActDecPart
                    isCrypt = isEnc || isDec
                    isPart  = act == ActADPart || act == ActEncPart || act == ActDecPart
                    inject = act == ActNonceWord || isAD || isCrypt
                    msgWord = byteBuf st

                    result = msgWord `xor` ns2
                    plainWord = if act == ActDecPart
                                then result .&. maskLen (partialLen st)
                                else if isDec then result else msgWord

                    newS1 = if done && isPart
                            then ns1 `xor` partialLenWord (partialLen st)
                            else ns1
                    newS3 = if done && inject then ns3 `xor` plainWord else ns3
                    emit = done && (isCrypt || act == ActFinPart1 || act == ActFinPart2)
                    newBuf = if isCrypt then (if isEnc then result else plainWord) else ns2
                    newLen = if isPart then partialLenOut (partialLen st) else 4
                    newNonceCnt = if noneWordCnt st == 2 then 0 else noneWordCnt st + 1
                in st { fsmState = if done then (if emit then StateOut else StateIdle) else StatePermutation
                      , s0 = ns0
                      , s1 = newS1
                      , s2 = ns2
                      , s3 = newS3
                      , stepCnt = if wordDone && not done then stepCnt st + 1 else stepCnt st
                      , bitCnt = bitCnt st + 1
                      , byteBuf = if emit then newBuf else byteBuf st
                      , outLen = if emit then newLen else outLen st
                      , noneWordCnt = if done && act == ActNonceWord
                                      then newNonceCnt else noneWordCnt st
                      }

            StateOut
                | outLen st == 1 && permAction st == ActFinPart1 ->
                    st { fsmState = StatePermutation
                       , permAction = ActFinPart2
                       , s1 = s1 st `xor` 0x70
                       , stepCnt = 0
                       , bitCnt = 0
                       , outLen = 0
                       }
                | outLen st == 1 -> st { fsmState = StateIdle, outLen = 0 }
                | otherwise      -> st { outLen = outLen st - 1, byteBuf = byteBuf st `shiftR` 8 }

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
