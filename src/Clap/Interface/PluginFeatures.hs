{-# LANGUAGE LambdaCase #-}

module Clap.Interface.PluginFeatures where

import Text.Read

data PluginFeature
    -- Category
    = Instrument
    | AudioEffect
    | NoteEffect
    | NoteDetector
    | Analyzer 

    -- Sub-category   
    | Synthesizer
    | Sampler
    | Drum
    | DrumMachine
    
    | Filter
    | Phaser
    | Equalizer
    | DeEsser
    | PhaseVocoder
    | Granular
    | FrequencyShifter
    | PitchShifter
    
    | Distortion
    | TransientShaper
    | Compressor
    | Limiter
    
    | Flanger
    | Chorus
    | Delay
    | Reverb
    
    | Tremolo
    | Glitch
    
    | Utility
    | PitchCorrection
    | Restoration
    
    | MultiEffects

    | Mixing
    | Mastering
    -- Audio capabilities
    | Mono
    | Stereo
    | Surround
    | Ambisonic

    | Other String
    deriving (Eq, Show)

instance Read PluginFeature where
    readPrec = do
        lexeme <- lexP
        pure $ case lexeme of
          Char char -> readString [char]
          String string -> readString string
          Punc string -> readString string
          Ident string -> readString string
          Symbol string -> readString string
          Number number -> readString $ show number
          EOF -> Other ""
        where
            readString = \case       
                "instrument" -> Instrument
                "audio-effect" -> AudioEffect
                "note-effect" -> NoteEffect
                "note-detector" -> NoteDetector
                "analyzer" -> Analyzer
                "synthesizer" -> Synthesizer
                "sampler" -> Sampler
                "drum" -> Drum
                "drum-machine" -> DrumMachine
                "filter" -> Filter
                "phaser" -> Phaser
                "equalizer" -> Equalizer
                "de-esser" -> DeEsser
                "phase-vocoder" -> PhaseVocoder
                "granular" -> Granular
                "frequency-shifter" -> FrequencyShifter
                "pitch-shifter" -> PitchShifter
                "distortion" -> Distortion
                "transient-shaper" -> TransientShaper
                "compressor" -> Compressor
                "limiter" -> Limiter
                "flanger" -> Flanger
                "chorus" -> Chorus
                "delay" -> Delay
                "reverb" -> Reverb
                "tremolo" -> Tremolo
                "glitch" -> Glitch
                "utility" -> Utility
                "pitch-correction" -> PitchCorrection
                "restoration" -> Restoration
                "multi-effects" -> MultiEffects
                "mixing" -> Mixing
                "mastering" -> Mastering
                other -> Other other
    readListPrec = readListPrecDefault
