{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

-- This module could be in core, but 'verifySscPayload' is kind of
-- complex and I think it can't be moved there.

-- | Pure verification for block. See "Pos.Util.Verification" for more info.

module Pos.Block.Verification () where

import           Universum

import           Formatting (build, sformat, (%))

import           Pos.Binary.Core ()
import           Pos.Core.Block (Block)
import           Pos.Core.Block.Blockchain (Blockchain (..), GenericBlock (..),
                                            GenericBlockHeader (..), gbExtra)
import           Pos.Core.Block.Genesis (GenesisBlockchain)
import           Pos.Core.Block.Main (Body (..), ConsensusData (..), MainBlockchain,
                                      MainExtraHeaderData (..), MainToSign (..),
                                      mainBlockEBDataProof)
import           Pos.Core.Block.Union (BlockHeader (..), BlockSignature (..))
import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.Delegation (LightDlgIndices (..))
import           Pos.Core.Slotting (SlotId (..))
import           Pos.Core.Ssc ()
import           Pos.Core.Txp ()
import           Pos.Core.Update ()
import           Pos.Crypto (HasCryptoConfiguration, ProxySignature (..), SignTag (..), checkSig,
                             hash, isSelfSignedPsk, proxyVerify)
import           Pos.Ssc.Functions (verifySscPayload)
import           Pos.Util.Some (Some (Some))
import           Pos.Util.Verification (PVer, PVerifiable (..), pverFail, pverField)

----------------------------------------------------------------------------
-- Some parts
----------------------------------------------------------------------------

instance PVerifiable BlockSignature where
    pverifySelf sig =
        when (selfSignedProxy sig) $
           pverFail "BlockSignature: can't use self-signed proxy psk to issue the block"
      where
        selfSignedProxy (BlockSignature _)                      = False
        selfSignedProxy (BlockPSignatureLight (psigPsk -> psk)) = isSelfSignedPsk psk
        selfSignedProxy (BlockPSignatureHeavy (psigPsk -> psk)) = isSelfSignedPsk psk

instance PVerifiable (ConsensusData MainBlockchain) where
    pverify = pverField "mcdSignature" . pverify . _mcdSignature

instance PVerifiable MainExtraHeaderData where
    pverify = pverField "mehSoftwareVersion" . pverify . _mehSoftwareVersion

instance HasCryptoConfiguration => PVerifiable (Body MainBlockchain) where
    pverify MainBody{..} = do
        pverField "txPayload" $ pverify _mbTxPayload
        pverField "sscPayload" $ pverify _mbSscPayload
        pverField "dlgPayload" $ pverify _mbDlgPayload
        pverField "updatePayload" $ pverify _mbUpdatePayload

----------------------------------------------------------------------------
-- Headers
----------------------------------------------------------------------------

-- I think only HasCryptoConfiguration is needed here @volhovm
instance HasConfiguration => PVerifiable (GenericBlockHeader MainBlockchain) where
    pverifySelf UnsafeGenericBlockHeader{..} =
        -- Internal consistency: is the signature in the consensus data really for
        -- this block?
        unless (verifyBlockSignature _mcdSignature) $
            pverFail "GenericBlockHeaderMain: can't verify signature"
      where
        verifyBlockSignature (BlockSignature sig) =
            checkSig SignMainBlock leaderPk signature sig
        verifyBlockSignature (BlockPSignatureLight proxySig) = do
            let epochId = siEpoch slotId
            proxyVerify
                SignMainBlockLight
                proxySig
                (\(LightDlgIndices (epochLow, epochHigh)) ->
                     epochLow <= epochId && epochId <= epochHigh)
                signature
        verifyBlockSignature (BlockPSignatureHeavy proxySig) =
            proxyVerify SignMainBlockHeavy proxySig (const True) signature

        signature = MainToSign _gbhPrevBlock _gbhBodyProof slotId difficulty _gbhExtra
        MainConsensusData
            { _mcdLeaderKey = leaderPk
            , _mcdSlot = slotId
            , _mcdDifficulty = difficulty
            , ..
            } = _gbhConsensus
    pverify it = do
        -- Previous header hash is always valid.
        -- Body proof is just a bunch of hashes, which is always valid __by itself__.
        -- Consensus data and extra header data require validation.
        pverify (_gbhConsensus it)
        pverify (_gbhExtra it)
        pverifySelf it

-- Genesis headers are not verifiable.
instance HasConfiguration => PVerifiable BlockHeader where
    pverifySelf (BlockHeaderGenesis _) = pass
    pverifySelf (BlockHeaderMain x)    = pverifySelf x
    pverify (BlockHeaderGenesis _) = pass
    pverify (BlockHeaderMain x)    = pverify x

----------------------------------------------------------------------------
-- Block
----------------------------------------------------------------------------

-- | Check whether 'BodyProof' corresponds to 'Body.
checkBodyProof ::
       ( Blockchain p
       , Buildable (BodyProof p)
       , Eq (BodyProof p)
       )
    => Body p
    -> BodyProof p
    -> PVer ()
checkBodyProof body proof = do
    let calculatedProof = mkBodyProof body
    let errMsg =
            sformat ("Incorrect proof of body. "%
                     "Proof in header: "%build%
                     ", calculated proof: "%build)
            proof calculatedProof
    unless (calculatedProof == proof) $ pverFail errMsg

instance PVerifiable (GenericBlock GenesisBlockchain) where
    pverifySelf UnsafeGenericBlock{..} = checkBodyProof _gbBody (_gbhBodyProof _gbHeader)

instance HasConfiguration => PVerifiable (GenericBlock MainBlockchain) where
    pverifySelf block@UnsafeGenericBlock{..} = do
        -- No need to verify the main extra body data. It's an 'Attributes ()'
        -- which is valid whenever it's well-formed.
        --
        -- Check internal consistency: the body proofs are all correct.
        checkBodyProof _gbBody (_gbhBodyProof _gbHeader)
        -- Check that the headers' extra body data hash is correct.
        -- This isn't subsumed by the body proof check.
        unless (hash (block ^. gbExtra) == (block ^. mainBlockEBDataProof)) $
            pverFail "Hash of extra body data is not equal to its representation in the header."
        -- Ssc and Dlg consistency checks which require the header, and so can't
        -- be done in 'verifyMainBody'.
        either (pverFail . pretty) pure $
            verifySscPayload
                (Right (Some _gbHeader))
                (_mbSscPayload _gbBody)
    pverify b = do
        pverField "gbHeader" $ pverify (_gbHeader b)
        pverField "gbBody" $ pverify (_gbBody b)
        pverifySelf b

instance HasConfiguration => PVerifiable Block where
    pverifySelf = either pverifySelf pverifySelf
    pverify = either pverify pverify
