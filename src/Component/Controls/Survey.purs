-- | The whole MC6 at once: thirty banks, twelve switches each, coloured by what
-- | they *mean*.
-- |
-- | Morningstar's editor shows you one bank at a time as a list of channels and
-- | numbers, which is why nobody can hold the instrument in their head. The
-- | thesis here is that the two things worth seeing are both global — how much
-- | of the device is in use, and how you get from one page to another — and
-- | neither is visible one bank at a time.
-- |
-- | Three rules this view is built on, all of them inherited rather than
-- | invented:
-- |
-- |   * **Empty and unknown are different.** An unknown bank is drawn hatched
-- |     and carries no switches, because `Survey` gives it none. Painting it as
-- |     twelve empty pads would be a claim about twenty-odd banks nobody has
-- |     looked at.
-- |   * **Colour encodes scope, not identity.** The verb ladder in
-- |     `Data.MC6.Verb` orders switches by how much of the rig they change, so
-- |     the legend reads bottom-to-top as reach, and a bank that is mostly
-- |     violet is a bank of expensive presses.
-- |   * **Intent and observation are held apart.** `slots` is what we meant;
-- |     `observedNames` is what the hardware says is there. Showing both is the
-- |     only way disagreement can be seen at all, and disagreement is the whole
-- |     reason reading the device was worth building.
-- |
-- | Rendering is a pure function of a `SurveyProps` rather than a component:
-- | the state it needs is three fields, and a slot boundary would buy nothing
-- | but plumbing.
module Component.Controls.Survey
  ( SurveyProps
  , render
  , verbColor
  , verbName
  , verbDetail
  , provenanceLabel
  , physicalOrder
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.MC6.Survey (BankCard, NavEdge, Provenance(..), bankCount, deadEnds, knownBanks, navEdges, reachableFrom, stranded, universalEdges)
import Data.MC6.Verb (ActionShape(..), NavTarget(..), Verb(..))
import Data.Maybe (Maybe(..))
import Data.Pedal (PedalId(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..), ElemName(..), Namespace(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- | Just enough SVG to draw a graph, without taking on a dependency for it.
-- |
-- | `halogen-svg-elems` would be the obvious reach, but the navigation map needs
-- | six element types and eight attributes, and Halogen already ships the
-- | namespace-aware constructor these are one line each on top of.
svgNS :: Namespace
svgNS = Namespace "http://www.w3.org/2000/svg"

svgEl :: forall r w i. String -> Array (HH.IProp r i) -> Array (HH.HTML w i) -> HH.HTML w i
svgEl name = HH.elementNS svgNS (ElemName name)

sAttr :: forall r i. String -> String -> HH.IProp r i
sAttr k v = HP.attr (AttrName k) v

sNum :: forall r i. String -> Number -> HH.IProp r i
sNum k v = sAttr k (show v)

type SurveyProps i =
  { cards :: Array BankCard
  -- | Where the board pages live — the survey's notion of home, and the root
  -- | of the reachability walk.
  , homeBank :: Int
  , readStatus :: Maybe String
  , elideUnknown :: Boolean
  -- | The bank currently being worked on, if any. The survey does not own the
  -- | detail view any more — selecting a card hands off to the bank zone below
  -- | it — so all this does is mark which card you are inside of.
  , selected :: Maybe Int
  -- | Shrink to a strip. Once you are inside a bank the instrument is context
  -- | rather than subject, and it should stop competing for the page.
  , compact :: Boolean
  , onToggleElide :: i
  , onSelect :: Int -> i
  , onRead :: i
  }

-- ──── Palette ────

-- | Colour by verb, ordered by scope: the wider a switch's reach, the further
-- | the hue travels from the neutral ground.
verbColor :: Verb -> String
verbColor = case _ of
  Blank -> "#e6e6ea"
  Navigation _ -> "#5b7fa8"
  Action _ -> "#d97706"
  PedalPreset _ -> "#15803d"
  Scene _ -> "#7e22ce"
  Raw -> "#9ca3af"

provenanceLabel :: Provenance -> String
provenanceLabel = case _ of
  Observed -> "read from the device"
  Authored -> "written by this app"
  Declared -> "declared in config"
  Unknown -> "never looked"

provenanceMark :: Provenance -> String
provenanceMark = case _ of
  Observed -> "\x25cf"   -- filled: the only one that is not a belief
  Authored -> "\x25cb"   -- hollow
  Declared -> "\x25cc"   -- dotted
  Unknown -> "\x00b7"    -- barely there

-- | The one-line gloss under a slot in the expanded view.
verbDetail :: Verb -> String
verbDetail = case _ of
  Blank -> "\x2014"
  Navigation (ToBank n) -> "jump to bank " <> show n <> " (editor " <> show (n + 1) <> ")"
  Navigation BankUp -> "bank up"
  Navigation BankDown -> "bank down"
  Navigation TogglePage -> "toggle page"
  Action a -> "CC " <> show a.cc <> " \x00b7 " <> shapeLabel a.shape
  PedalPreset p -> "program change " <> show p.program
  Scene s -> "recall board " <> show s.cc
  Raw -> "messages we can send but not name"

shapeLabel :: ActionShape -> String
shapeLabel = case _ of
  Momentary -> "hold"
  Toggling -> "latch"
  OneShot -> "one shot"

-- | Where the twelve switches actually are underfoot: four rows of three.
-- |
-- | The MC6 itself is the first two rows and its top row is D E F, not A B C —
-- | the indices run along the bottom row first. Then an FS3X per row after
-- | that. Drawing them in index order would make the card a list rather than a
-- | picture of the pedal, and the point of a card this small is that you
-- | recognise the shape without reading it.
-- |
-- | The editor's own 9-switch grid uses the same convention for its first nine.
physicalOrder :: Array Int
physicalOrder = [ 3, 4, 5, 0, 1, 2, 6, 7, 8, 9, 10, 11 ]

-- ──── Render ────

render :: forall w i. SurveyProps i -> HH.HTML w i
render props =
  HH.div [ HP.class_ (H.ClassName ("survey" <> if props.compact then " compact" else "")) ]
    ( [ renderHeader props
      , HH.div [ HP.class_ (H.ClassName "survey-grid") ]
          (map (renderCard props) (visible props))
      ]
      -- The legend, the graph and the warnings are about the instrument as a
      -- whole. Once you are inside one bank they are no longer what you are
      -- looking at, and keeping them would push the actual work below the fold.
      <> if props.compact then [] else
           [ renderLegend, renderNavMap props, renderWarnings props ]
    )

-- | Which cards to draw. Eliding hides only the *unknown* ones — a bank we have
-- | read and found empty stays visible, because "read, and empty" is a fact
-- | worth seeing and "never read" is not the same thing.
visible :: forall i. SurveyProps i -> Array BankCard
visible props =
  if props.elideUnknown
    then Array.filter (\c -> c.provenance /= Unknown) props.cards
    else props.cards

renderHeader :: forall w i. SurveyProps i -> HH.HTML w i
renderHeader props =
  let known = Array.length (knownBanks props.cards)
      observed = Array.length (Array.filter (\c -> c.provenance == Observed) props.cards)
  in HH.div [ HP.class_ (H.ClassName "survey-header") ]
    [ HH.h3_ [ HH.text "The instrument" ]
    , HH.p [ HP.class_ (H.ClassName "survey-summary") ]
        [ HH.text (show known <> " of " <> show bankCount <> " banks known, "
                    <> show observed <> " read from the device") ]
    , HH.div [ HP.class_ (H.ClassName "survey-actions") ]
        [ HH.button
            [ HP.class_ (H.ClassName "controls-btn-small")
            , HE.onClick \_ -> props.onRead
            ]
            [ HH.text "Read MC6" ]
        , HH.label [ HP.class_ (H.ClassName "survey-toggle") ]
            [ HH.input
                [ HP.type_ HP.InputCheckbox
                , HP.checked props.elideUnknown
                , HE.onChecked \_ -> props.onToggleElide
                ]
            , HH.text " hide banks never looked at"
            ]
        ]
    , case props.readStatus of
        Nothing -> HH.text ""
        Just msg -> HH.p [ HP.class_ (H.ClassName "survey-status") ] [ HH.text msg ]
    ]

renderLegend :: forall w i. HH.HTML w i
renderLegend =
  HH.div [ HP.class_ (H.ClassName "survey-legend") ]
    [ HH.div [ HP.class_ (H.ClassName "survey-legend-row") ]
        (map swatch
          [ Tuple "empty" Blank
          , Tuple "navigation" (Navigation BankUp)
          , Tuple "action" (Action { pedalId: PedalId "", cc: 0, shape: OneShot })
          , Tuple "pedal preset" (PedalPreset { pedalId: PedalId "", program: 0 })
          , Tuple "scene" (Scene { cc: 0 })
          , Tuple "raw" Raw
          ])
    , HH.p [ HP.class_ (H.ClassName "survey-legend-note") ]
        [ HH.text "Left to right is reach: how much of the rig one press changes." ]
    , HH.div [ HP.class_ (H.ClassName "survey-legend-row") ]
        (map provSwatch [ Observed, Authored, Declared, Unknown ])
    ]
  where
  swatch (Tuple label verb) =
    HH.span [ HP.class_ (H.ClassName "survey-legend-item") ]
      [ HH.span
          [ HP.class_ (H.ClassName "survey-swatch")
          , HP.style ("background: " <> verbColor verb)
          ] []
      , HH.text label
      ]
  provSwatch p =
    HH.span [ HP.class_ (H.ClassName "survey-legend-item") ]
      [ HH.span [ HP.class_ (H.ClassName "survey-prov-mark") ] [ HH.text (provenanceMark p) ]
      , HH.text (provenanceLabel p)
      ]

renderCard :: forall w i. SurveyProps i -> BankCard -> HH.HTML w i
renderCard props card =
  let isHome = card.bankNumber == props.homeBank
      isOpen = props.selected == Just card.bankNumber
      classes = String.joinWith " "
        ([ "survey-card" ]
          <> (if card.provenance == Unknown then [ "unknown" ] else [])
          <> (if isHome then [ "home" ] else [])
          <> (if isOpen then [ "open" ] else []))
      outgoing = map _.to (Array.filter (\e -> e.from == card.bankNumber) (navEdges props.cards))
  in HH.div
    [ HP.class_ (H.ClassName classes)
    , HE.onClick \_ -> props.onSelect card.bankNumber
    , HP.title (provenanceLabel card.provenance)
    ]
    [ HH.div [ HP.class_ (H.ClassName "survey-card-head") ]
        [ HH.span [ HP.class_ (H.ClassName "survey-card-num") ]
            [ HH.text (show card.bankNumber) ]
        -- Editor numbering shown quietly beside the wire number. Both are
        -- needed and confusing them writes to a neighbouring bank.
        , HH.span [ HP.class_ (H.ClassName "survey-card-editor") ]
            [ HH.text ("/" <> show (card.bankNumber + 1)) ]
        , HH.span [ HP.class_ (H.ClassName "survey-card-prov") ]
            [ HH.text (provenanceMark card.provenance) ]
        ]
    , HH.div [ HP.class_ (H.ClassName "survey-card-name") ]
        [ HH.text (if card.name == "" then "\x2014" else card.name) ]
    , if Array.null card.slots
        then HH.div [ HP.class_ (H.ClassName "survey-card-unread") ] [ HH.text "not read" ]
        else HH.div [ HP.class_ (H.ClassName "survey-card-slots") ]
               (Array.mapMaybe (\i -> renderSlot card i <$> Array.index card.slots i) physicalOrder)
    , if Array.null outgoing
        then HH.text ""
        else HH.div [ HP.class_ (H.ClassName "survey-card-jumps") ]
               [ HH.text ("\x2192 " <> String.joinWith " " (map show (Array.nub outgoing))) ]
    , case card.agrees of
        Just false ->
          HH.div [ HP.class_ (H.ClassName "survey-card-disagree") ]
            [ HH.text "device disagrees" ]
        _ -> HH.text ""
    ]

renderSlot :: forall w i. BankCard -> Int -> Verb -> HH.HTML w i
renderSlot card idx verb =
  HH.span
    [ HP.class_ (H.ClassName "survey-slot")
    , HP.style ("background: " <> verbColor verb)
    , HP.title (show (idx + 1) <> ": " <> verbDetail verb
                 <> observedSuffix (Array.index card.observedNames idx))
    ] []
  where
  observedSuffix = case _ of
    Just nm | nm /= "" -> "  (device says \x201c" <> nm <> "\x201d)"
    _ -> ""

-- ──── The navigation graph, actually drawn ────

-- | Thirty nodes in the same six-across arrangement as the cards, with an arrow
-- | for every programmed bank jump.
-- |
-- | This is the payoff for making navigation a verb instead of leaving it as an
-- | untyped message: once jumps are edges, "can I get there" and "can I get
-- | back" are things you can look at rather than things you have to remember.
renderNavMap :: forall w i. SurveyProps i -> HH.HTML w i
renderNavMap props =
  let universal = universalEdges props.cards
      -- Deduplicated on the pair actually drawn, so a jump that exists on two
      -- switches of the same bank is one arc, not two on top of each other.
      edges = Array.nubBy (\a b -> compare (Tuple a.from a.to) (Tuple b.from b.to))
                (navEdges props.cards)
  in if Array.null edges
    then HH.div [ HP.class_ (H.ClassName "survey-navmap-empty") ]
      [ HH.text "No bank jumps are programmed yet, so there is no navigation to draw." ]
    else HH.div [ HP.class_ (H.ClassName "survey-navmap") ]
      [ HH.h4_ [ HH.text "Navigation" ]
      , svgEl "svg"
          [ sAttr "viewBox" ("0 0 " <> show mapW <> " " <> show mapH)
          , sAttr "class" "survey-navmap-svg"
          ]
          ( [ svgEl "defs" [] [ arrowMarker, arrowMarkerFaint ] ]
              -- Furniture first, so a jump peculiar to one page is drawn over
              -- the everywhere-jumps rather than under them.
              <> map (edgeLine universal) (Array.filter (isUniversal universal) edges)
              <> map (edgeLine universal) (Array.filter (not <<< isUniversal universal) edges)
              <> map (nodeDot props) props.cards
          )
      , if Set.isEmpty universal then HH.text "" else
          HH.p [ HP.class_ (H.ClassName "survey-legend-note") ]
            [ HH.text "Faint arrows are the same switch going to the same bank almost everywhere \x2014 the instrument's furniture rather than the shape of a set." ]
      ]

isUniversal :: Set (Tuple Int Int) -> NavEdge -> Boolean
isUniversal universal e = Set.member (Tuple e.slot e.to) universal

mapCols :: Int
mapCols = 6

mapW :: Number
mapW = 360.0

mapH :: Number
mapH = 220.0

nodeX :: Int -> Number
nodeX n = 34.0 + Int.toNumber (n `mod` mapCols) * 60.0

nodeY :: Int -> Number
nodeY n = 26.0 + Int.toNumber (n / mapCols) * 42.0

arrowMarker :: forall w i. HH.HTML w i
arrowMarker =
  svgEl "marker"
    [ sAttr "id" "survey-arrow"
    , sAttr "markerWidth" "8"
    , sAttr "markerHeight" "8"
    , sAttr "refX" "7"
    , sAttr "refY" "3"
    , sAttr "orient" "auto"
    , sAttr "markerUnits" "strokeWidth"
    ]
    [ svgEl "path"
        [ sAttr "d" "M0,0 L7,3 L0,6 Z"
        , sAttr "fill" edgeInk
        ] []
    ]

arrowMarkerFaint :: forall w i. HH.HTML w i
arrowMarkerFaint =
  svgEl "marker"
    [ sAttr "id" "survey-arrow-faint"
    , sAttr "markerWidth" "8"
    , sAttr "markerHeight" "8"
    , sAttr "refX" "7"
    , sAttr "refY" "3"
    , sAttr "orient" "auto"
    , sAttr "markerUnits" "strokeWidth"
    ]
    [ svgEl "path"
        [ sAttr "d" "M0,0 L7,3 L0,6 Z"
        , sAttr "fill" edgeInk
        , sAttr "opacity" "0.28"
        ] []
    ]

edgeInk :: String
edgeInk = "#5b7fa8"

-- | A jump, drawn as a quadratic arc so that a pair of banks pointing at each
-- | other reads as two arcs rather than one ambiguous line.
-- |
-- | The control point is pushed off the chord by a fraction of the
-- | perpendicular, which also keeps a jump between adjacent banks from
-- | disappearing under the two node dots.
edgeLine :: forall w i. Set (Tuple Int Int) -> NavEdge -> HH.HTML w i
edgeLine universal e =
  let x1 = nodeX e.from
      y1 = nodeY e.from
      x2 = nodeX e.to
      y2 = nodeY e.to
      cx = (x1 + x2) / 2.0 + (y2 - y1) * 0.18
      cy = (y1 + y2) / 2.0 - (x2 - x1) * 0.18
      faint = isUniversal universal e
  in svgEl "path"
    [ sAttr "d" ("M" <> show x1 <> "," <> show y1
                  <> " Q" <> show cx <> "," <> show cy
                  <> " " <> show x2 <> "," <> show y2)
    , sAttr "stroke" edgeInk
    , sAttr "stroke-width" (if faint then "0.8" else "1.2")
    , sAttr "opacity" (if faint then "0.28" else "1")
    , sAttr "fill" "none"
    , sAttr "marker-end" (if faint then "url(#survey-arrow-faint)" else "url(#survey-arrow)")
    ] []

nodeDot :: forall w i. SurveyProps i -> BankCard -> HH.HTML w i
nodeDot props card =
  let n = card.bankNumber
      known = card.provenance /= Unknown
      isHome = n == props.homeBank
      fill
        | isHome = "#111827"
        | known = edgeInk
        | otherwise = "#e6e6ea"
  in svgEl "g" []
    [ svgEl "circle"
        [ sNum "cx" (nodeX n)
        , sNum "cy" (nodeY n)
        , sNum "r" (if isHome then 8.0 else 6.0)
        , sAttr "fill" fill
        ] []
    , svgEl "text"
        [ sNum "x" (nodeX n)
        , sNum "y" (nodeY n + 3.0)
        , sAttr "class" "survey-navmap-label"
        , sAttr "text-anchor" "middle"
        , sAttr "fill" (if known || isHome then "#ffffff" else "#6b7280")
        ]
        [ HH.text (show n) ]
    ]

-- ──── What the graph knows that you would otherwise find out mid-take ────

renderWarnings :: forall w i. SurveyProps i -> HH.HTML w i
renderWarnings props =
  let strandedBanks = Array.filter (_ /= props.homeBank) (stranded props.homeBank props.cards)
      ends = Array.filter (_ /= props.homeBank) (deadEnds props.cards)
      reach = Set.size (reachableFrom props.homeBank props.cards)
  in if Array.null strandedBanks && Array.null ends
    then HH.text ""
    else HH.div [ HP.class_ (H.ClassName "survey-warnings") ]
      ( [ HH.p [ HP.class_ (H.ClassName "survey-warn-reach") ]
            [ HH.text (show reach <> " banks reachable from bank " <> show props.homeBank) ]
        ]
        <> (if Array.null strandedBanks then [] else
              [ HH.p [ HP.class_ (H.ClassName "survey-warn") ]
                  [ HH.text ("Programmed but unreachable: "
                              <> String.joinWith ", " (map show strandedBanks)
                              <> ". Nothing jumps to these, so they can only be got to by hand.") ]
              ])
        <> (if Array.null ends then [] else
              [ HH.p [ HP.class_ (H.ClassName "survey-warn soft") ]
                  [ HH.text ("No way out programmed: "
                              <> String.joinWith ", " (map show ends)
                              <> ". The MC6's own bank up/down still works, so this is a page you leave by remembering a gesture.") ]
              ])
      )

-- | The verb in one word, for a column heading's worth of space.
verbName :: Verb -> String
verbName = case _ of
  Blank -> "empty"
  Navigation _ -> "navigation"
  Action _ -> "action"
  PedalPreset _ -> "pedal preset"
  Scene _ -> "scene"
  Raw -> "raw"
