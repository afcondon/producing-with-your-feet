-- | The pages this app ships with. The shape of a page, and everything that
-- | is true of any page, is Helm's (`Helm.MC6.ControlBank`); these are this
-- | app's choices of what to put on one.
module Data.MC6.BuiltInBanks
  ( exampleControlBank
  , ambientControlBank
  , builtInControlBanks
  ) where

import Helm.MC6.ControlBank (ControlBank, ccMomentaryMessages, ccToggleMessages, emptySwitch)

-- | The **ambient / evolving** control page: MOOD, Onward, Lost+Found, Habit.
-- |
-- | The group `docs/DESIGN-BANKS.md` names, and the reason this page exists
-- | rather than a wall of twelve bypasses: a group page is a thing you are
-- | currently doing, where a list of every pedal is a list of things.
-- |
-- | **The six expander switches are the three dual-engage pedals, two channels
-- | each, and that is not a coincidence — it is why the group fits.** MOOD,
-- | Onward and Lost+Found each have two independently bypassable channels and
-- | no way to be reduced to one switch; three times two is exactly the six that
-- | `G`–`L` provide. Habit engages with one, so it goes on the unit's own six
-- | with room to spare. That split is also the ergonomic one: the FS3X switches
-- | are sloped and reachable, the unit's are neither, and the thing you reach
-- | for mid-phrase is a channel rather than a whole pedal.
-- |
-- | **`L` and `R` here are the pedal's own two switches, named by what they
-- | do rather than by side**, because the engage table stores them as `a` and
-- | `b` and does not say which is which. If a pair is the wrong way round
-- | under your foot, swapping two lines below is the whole fix.
-- |
-- | Sixteen: the first free bank of the pedal half. Fifteen is
-- | `control-default` and anything below `Reserved.pedalRangeFrom` is refused
-- | as `ControlTooLow`.
ambientControlBank :: ControlBank
ambientControlBank =
  { id: "control-ambient"
  , name: "Ambient"
  , description: "MOOD, Onward, Lost+Found channels on the expanders; Habit on the unit"
  , mc6BankNumber: 16
  , returnSwitchIndex: 0
  , switches:
      -- A-F, the unit's own: the whole-pedal moves, which are the ones you can
      -- afford to reach up for.
      [ { label: "Ht Byp",  longName: "Habit Bypass",        toToggle: true, messages: ccToggleMessages 15 102 }
      , { label: "MD Both", longName: "MOOD Both Channels",  toToggle: true, messages: ccToggleMessages 3 55 }
      , emptySwitch
      , emptySwitch
      , emptySwitch
      , emptySwitch
      -- G-L, the expanders: a channel each, in pedal order.
      , { label: "Ow Freez", longName: "Onward Freeze",      toToggle: true, messages: ccToggleMessages 2 102 }
      , { label: "Ow Gltch", longName: "Onward Glitch",      toToggle: true, messages: ccToggleMessages 2 103 }
      , { label: "MD ML",    longName: "MOOD Micro-Looper",  toToggle: true, messages: ccToggleMessages 3 102 }
      , { label: "MD Wet",   longName: "MOOD Wet Channel",   toToggle: true, messages: ccToggleMessages 3 103 }
      , { label: "L+F Lft",  longName: "Lost+Found Left",    toToggle: true, messages: ccToggleMessages 6 103 }
      , { label: "L+F Rt",   longName: "Lost+Found Right",   toToggle: true, messages: ccToggleMessages 6 102 }
      ]
  }


-- | Hard-coded example: direct pedal controls, in the pedal half of the bank
-- | table (`Data.MC6.Reserved`).
exampleControlBank :: ControlBank
exampleControlBank =
  { id: "control-default"
  , name: "Default Controls"
  , description: "Habit loop, Brig infinite, MOOD freeze, Clean/Mercury7 swell, Lex speed, Brig tap"
  -- Fifteen: the first bank of the pedal half of the table. It sat on 20,
  -- where the probe bank also sat — a collision that does not fail but
  -- *uploads*, the second write landing on top of the first, and which nothing
  -- compared the two lists to notice. `Data.MC6.Reserved` holds the map now,
  -- and `pedalRangeFrom` is the line this sits just above.
  , mc6BankNumber: 15
  , returnSwitchIndex: 6
  , switches:
      [ { label: "Ht Loop",  longName: "Habit Loop Toggle",     toToggle: true,  messages: ccToggleMessages 15 24 }
      , { label: "Ht Clear", longName: "Habit Clear",           toToggle: false, messages: ccMomentaryMessages 15 26 }
      , { label: "Br Infin", longName: "Brig Infinite Toggle",  toToggle: true,  messages: ccToggleMessages 14 97 }
      , { label: "MD Freez", longName: "MOOD Freeze Toggle",    toToggle: true,  messages: ccToggleMessages 3 105 }
      , { label: "Cl Swell", longName: "Clean Swell Toggle",    toToggle: true,  messages: ccToggleMessages 4 103 }
      , { label: "M7 Swell", longName: "Mercury7 Swell Toggle", toToggle: true,  messages: ccToggleMessages 12 28 }
      , { label: "< Back",   longName: "Back to Board Bank",    toToggle: false, messages: [] }  -- filled by a global, if one holds G
      , { label: "Lx Speed", longName: "Lex Speed Toggle",      toToggle: true,  messages: ccToggleMessages 8 22 }
      , { label: "Br Tap",   longName: "Brig Tap Tempo",        toToggle: false, messages: ccMomentaryMessages 14 93 }
      , emptySwitch
      , emptySwitch
      , emptySwitch
      ]
  }

-- | The pages this app ships with, which a person's own list may not hold.
-- |
-- | A saved list replaces the defaults wholesale (`Engine.Storage`), so a page
-- | added to the app after the list was first saved never appeared: Ambient,
-- | added 2026-08-30 with the scene that answers to it, was invisible on
-- | 2026-11-04. Merging them back in silently would resurrect a page deleted
-- | on purpose, so the MC6 tab *offers* each one that is missing instead.
builtInControlBanks :: Array ControlBank
builtInControlBanks = [ exampleControlBank, ambientControlBank ]
