module Clap.Interface.PluginFeatures where

import Text.Read

data PluginFeature
    -- Category
    = Instrument
    | AudioEffect
    | NoteEffect
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
        Ident string <- lexP
        case string of
            "instrument" -> pure Instrument
            "audio-effect" -> pure AudioEffect
            "note-effect" -> pure NoteEffect
            "analyzer" -> pure Analyzer
            "synthesizer" -> pure Synthesizer
            "sampler" -> pure Sampler
            "drum" -> pure Drum
            "drum-machine" -> pure DrumMachine
            "filter" -> pure Filter
            "phaser" -> pure Phaser
            "equalizer" -> pure Equalizer
            "de-esser" -> pure DeEsser
            "phase-vocoder" -> pure PhaseVocoder
            "granular" -> pure Granular
            "frequency-shifter" -> pure FrequencyShifter
            "pitch-shifter" -> pure PitchShifter
            "distortion" -> pure Distortion
            "transient-shaper" -> pure TransientShaper
            "compressor" -> pure Compressor
            "limiter" -> pure Limiter
            "flanger" -> pure Flanger
            "chorus" -> pure Chorus
            "delay" -> pure Delay
            "reverb" -> pure Reverb
            "tremolo" -> pure Tremolo
            "glitch" -> pure Glitch
            "utility" -> pure Utility
            "pitch-correction" -> pure PitchCorrection
            "restoration" -> pure Restoration
            "multi-effects" -> pure MultiEffects
            "mixing" -> pure Mixing
            "mastering" -> pure Mastering
            other -> pure $ Other other
    readListPrec = readListPrecDefault