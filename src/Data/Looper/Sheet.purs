-- | The board and the recipes, as one printable document.
-- |
-- | ## Why a whole HTML document rather than a print stylesheet
-- |
-- | The obvious thing is `@media print` on the app: open the Twister modal, hit
-- | print, hide the chrome. It does not work here for a reason that has bitten
-- | this project twice already — **a background tab is a dead looper**. The
-- | sheet exists so that the paper beside the rig can be read while the app's
-- | tab keeps focus and keeps handling Twister messages. A print view *of* the
-- | app is a print view you have to be looking at.
-- |
-- | So this is a self-contained document, opened in its own tab, that the
-- | browser can turn into paper or a PDF with the dialog it already has. No PDF
-- | library, no server round trip, nothing to install.
-- |
-- | ## Every word of it is generated
-- |
-- | The same rule as `Component.Looper.TwisterMap` and `Data.Looper.Recipes`:
-- | a printed layout typed out beside the table it describes is right when
-- | written and wrong the first time a control moves — wrong in the one place
-- | nobody thinks to check, because it is only documentation. This reads
-- | `Data.Looper.Twister.pages` and `Data.Looper.Recipes.recipes` and knows
-- | nothing else. Move an encoder and the next print is right.
-- |
-- | That is also why it is a pure `String` rather than something built in the
-- | DOM: the suite can read it, and does.
module Data.Looper.Sheet
  ( sheet
  , escape
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Looper.Recipes as Recipes
import Data.Looper.Twister as TW
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.String.CodeUnits as CU

-- | The whole document, ready for `Foreign.Sheet.openSheet`.
-- |
-- | Three parts, in the order you would use them: the four boards at a glance
-- | (the sheet to tape to the desk), then a page of detail for each of them,
-- | then the recipes. The browser's own dialog picks which of those you
-- | actually want on paper, which is why they are one document rather than
-- | three buttons.
sheet :: String
sheet =
  "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">"
    <> "<title>Itajara — the board and the recipes</title>"
    <> "<style>" <> css <> "</style></head><body>"
    <> "<button class=\"noprint print-me\" onclick=\"window.print()\">Print this</button>"
    <> header
    <> glance
    <> "<div class=\"break\"></div>"
    <> detail
    <> "<div class=\"break\"></div>"
    <> recipes
    <> "</body></html>"

header :: String
header =
  "<header><h1>Itajara — the board and the recipes</h1>"
    <> p "sub"
         "Printed from the app, and generated from the same tables it draws \
         \itself from. If this sheet and the app disagree, the app is right and \
         \this one is stale — print it again."
    <> p "sub" pagerNote
    <> "<div class=\"key\"><b>A loop's colour on the Loops page:</b>"
    <> foldMap keyDot TW.phaseKey
    <> keyOne "off" "empty"
    <> "</div></header>"
  where
  keyDot k = keyOne k.tone k.phase
  keyOne tone label =
    "<span class=\"key-item\"><i class=\"dot tone-" <> tone <> "\"></i>"
      <> escape label <> "</span>"

-- | The one thing about this surface you cannot work out by looking at it, and
-- | the number in it is computed rather than stated.
pagerNote :: String
pagerNote =
  "The pager is the bottom-right encoder on every page and reads an absolute \
  \position: " <> show TW.pageStep <> " units a page, so a page is a quarter \
  \turn. Press it to go home to the Loops page. Two encoders carry a mode on \
  \the press and a value on the turn — bars on page 1 and tape on page 4 — and \
  \each is lit when its mode is on."

-- | All four boards, names only. The sheet you keep.
glance :: String
glance =
  "<section class=\"glance\"><h2>The four pages</h2><div class=\"boards\">"
    <> foldMap board TW.pages
    <> "</div></section>"
  where
  board pg =
    "<div class=\"board\"><h3>" <> pageTitle pg <> "</h3><div class=\"grid\">"
      <> foldMap small pg.cells
      <> "</div></div>"
  small c =
    if c.name == "" then "<div class=\"cell blank\">·</div>"
    else "<div class=\"cell\">" <> dot c.tone <> "<span>" <> escape c.name
           <> "</span></div>"

-- | A page each, with what a press and a turn actually do.
detail :: String
detail =
  "<section class=\"detail\">"
    <> foldMap one TW.pages
    <> "</section>"
  where
  one pg =
    "<div class=\"page-detail\"><h2>" <> pageTitle pg <> "</h2>"
      <> p "note" pg.note
      <> "<div class=\"grid big\">"
      <> foldMap group (runs pg.cells)
      <> "</div></div>"

  group g = case Array.head g.cells, Array.last g.cells of
    Just first, Just lastCell
      | Array.length g.cells > 1 ->
          described "cell big wide" (escape first.name <> " – " <> escape lastCell.name) first
    Just only, _ -> cell only
    _, _ -> ""

  cell c =
    if c.name == "" then "<div class=\"cell big blank\">·</div>"
    else described "cell big" (escape c.name) c

  described klass title c =
    "<div class=\"" <> klass <> "\"><div class=\"cell-head\">" <> dot c.tone
      <> "<b>" <> title <> "</b></div>"
      <> line "press" c.press
      <> line "turn" c.turn
      <> maybe "" (\s -> "<div class=\"shows\">" <> escape s <> "</div>") c.shows
      <> "</div>"

  line label = maybe "" \what ->
    "<div class=\"line\"><i>" <> label <> "</i> " <> escape what <> "</div>"

-- | Consecutive cells that describe themselves identically, gathered.
-- |
-- | **Because eight loops printed eight identical paragraphs.** The Loops page
-- | and The set both open with eight encoders whose press, turn and readout are
-- | word for word the same — that is the property they are built on, that the
-- | spatial map is learned once — and a detail grid that repeated it eight
-- | times spent most of a sheet saying nothing new, in the smallest type on it.
-- |
-- | Consecutive and general rather than a rule about loops: this knows nothing
-- | about what a loop is, only that two neighbouring cells say the same thing,
-- | which is the fact that actually makes the repetition pointless. A ninth
-- | identical control would fold in on its own.
runs :: Array TW.Cell -> Array { cells :: Array TW.Cell }
runs = Array.foldl step []
  where
  step acc c = case Array.last acc of
    Just g | Just prev <- Array.last g.cells, same prev c ->
      Array.snoc (Array.dropEnd 1 acc) { cells: Array.snoc g.cells c }
    _ -> Array.snoc acc { cells: [ c ] }
  -- The description, not the cell: two cells differ in their index and in their
  -- name, and neither of those is a reason to print the same three lines twice.
  same a b =
    a.press == b.press && a.turn == b.turn && a.shows == b.shows
      && a.tone == b.tone && a.name /= "" && b.name /= ""

-- | Page 4 rather than page 3, everywhere a person reads it. The tables count
-- | banks from zero; nothing printed should.
pageTitle :: TW.Page -> String
pageTitle pg = "Page " <> show (pg.bank + 1) <> " — " <> escape pg.name

recipes :: String
recipes =
  "<section class=\"recipes\"><h2>Recipes — what to press, and what should \
  \happen</h2>"
    <> p "sub" Recipes.preamble
    <> foldMap one Recipes.recipes
    <> "</section>"
  where
  one r =
    "<div class=\"recipe\"><h3>" <> escape r.name <> "</h3>"
      <> p "why" r.why
      <> "<ol>" <> foldMap st r.steps <> "</ol>"
      <> maybe "" (p "recipe-note") r.note
      <> "</div>"
  st s =
    "<li>"
      <> (if s.at == "" then "" else "<b>" <> escape s.at <> "</b> ")
      <> escape s.act
      <> maybe "" (\e -> "<div class=\"expect\">" <> escape e <> "</div>") s.expect
      <> "</li>"

p :: String -> String -> String
p klass body = "<p class=\"" <> klass <> "\">" <> escape body <> "</p>"

-- | A control's colour, where it has a fixed one. The loop encoders take theirs
-- | from the phase, so they get the hollow ring the key explains rather than a
-- | filled dot that would claim a colour they do not have.
dot :: Maybe String -> String
dot = case _ of
  Nothing -> "<i class=\"dot phase\"></i>"
  Just t -> "<i class=\"dot tone-" <> t <> "\"></i>"

-- | The four characters that could close a tag or an attribute early.
-- |
-- | Nothing in the tables is hostile — it is all written in this repo — but an
-- | em dash is not the only thing that has ever surprised a serialiser here,
-- | and a document that silently loses a control's name because someone wrote
-- | `<` in it would be the worst possible failure for a printed reference. The
-- | suite checks the round trip.
escape :: String -> String
escape = CU.toCharArray >>> map one >>> Array.fold
  where
  one c = case c of
    '&' -> "&amp;"
    '<' -> "&lt;"
    '>' -> "&gt;"
    '"' -> "&quot;"
    _ -> String.singleton (String.codePointFromChar c)

-- | Ink, not screen. Small type, tight grids, and the swatches from
-- | `TW.swatch` so the paper and the device are claiming the same colours.
css :: String
css =
  "*{box-sizing:border-box}\
  \body{margin:0;padding:16mm 12mm;font:10pt/1.35 -apple-system,'Helvetica Neue',\
  \Arial,sans-serif;color:#1c1a17;background:#fff}\
  \h1{font-size:16pt;margin:0 0 6px;letter-spacing:-0.01em}\
  \h2{font-size:12pt;margin:0 0 8px;letter-spacing:-0.01em}\
  \h3{font-size:10.5pt;margin:0 0 6px}\
  \p{margin:0 0 8px}\
  \.sub,.note,.why{color:#5b544c;font-size:8.5pt;max-width:52em}\
  \header{border-bottom:1px solid #d8d2c8;padding-bottom:10px;margin-bottom:12px}\
  \.key{margin-top:6px;font-size:8.5pt}\
  \.key-item{display:inline-flex;align-items:center;gap:4px;margin-right:12px}\
  \.dot{width:9px;height:9px;border-radius:50%;display:inline-block;\
  \flex:0 0 auto;border:1px solid rgba(0,0,0,.2)}\
  \.dot.phase{background:repeating-linear-gradient(45deg,#fff,#fff 2px,#bbb 2px,#bbb 4px)}\
  \.boards{display:grid;grid-template-columns:1fr 1fr;gap:10mm 8mm}\
  \.board{break-inside:avoid}\
  \.grid{display:grid;grid-template-columns:repeat(4,1fr);gap:3px}\
  \.cell{border:1px solid #d8d2c8;border-radius:3px;padding:4px 5px;font-size:8pt;\
  \display:flex;align-items:center;gap:4px;min-height:26px}\
  \.cell.blank{color:#c9c2b8;justify-content:center}\
  \.cell.big{display:block;min-height:74px;font-size:7.5pt;line-height:1.3}\
  \.cell.wide{grid-column:1/-1;min-height:0}\
  \.cell-head{display:flex;align-items:center;gap:4px;margin-bottom:3px}\
  \.line i{color:#8a8178;font-style:normal;text-transform:uppercase;\
  \font-size:6pt;letter-spacing:.06em;margin-right:2px}\
  \.shows{color:#8a8178;margin-top:3px;font-size:6.8pt}\
  \.page-detail{break-inside:avoid;margin-bottom:9mm}\
  \.recipe{break-inside:avoid;margin-bottom:7mm}\
  \.recipe ol{margin:0;padding-left:1.2em;font-size:9pt}\
  \.recipe li{margin-bottom:3px}\
  \.expect{color:#3a6d4a;font-style:italic;font-size:8pt}\
  \.recipe-note{border-left:2px solid #d8d2c8;padding-left:8px;font-size:8.5pt;\
  \color:#5b544c;margin-top:5px}\
  \.break{break-after:page;page-break-after:always}\
  \.print-me{position:fixed;top:8px;right:8px;font:inherit;font-size:9pt;\
  \padding:6px 12px;border:1px solid #b9b1a6;border-radius:4px;background:#fff;\
  \cursor:pointer}\
  \@media print{.noprint{display:none}body{padding:0}}\
  \@page{size:A4 portrait;margin:12mm}"
    <> foldMap tone TW.tones
    <> ".tone-off{background:#efece7}"
  where
  tone t = ".tone-" <> TW.toneName t <> "{background:" <> TW.swatch t <> "}"
