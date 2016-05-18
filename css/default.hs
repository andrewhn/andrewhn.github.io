{-# LANGUAGE OverloadedStrings #-}

import Clay
import qualified Clay.Media as Mq

siteWidth :: Size Abs
siteWidth = px 700

defaults :: Css
defaults = html ? do
  fontFamily ["Lora", "Georgia"] [serif]
  margin (px 0) (px 0) (px 0) (px 0)
  position relative
  fontSize (pct 100)
  -- TODO: pandoc doesn't support :width: modifier for ..image?
  img ? do
    width (pct 100)
  narrow $ width (pct 100)
  wide $ do
    width siteWidth
    marginLeft auto
    marginRight auto

wide, narrow :: Css -> Css
wide = query Clay.all [Mq.minWidth siteWidth]
narrow = query Clay.all [Mq.maxWidth siteWidth]

siteHeader :: Css
siteHeader = ".site-header" ? do
  a ? do
    textDecoration none
    color black
  padding (em 0.75) (em 0) (em 1.5) (em 0)
  textAlign (alignSide sideCenter)
  width (pct 100)
  hgroup ? do
    padding (em 1.25) (em 0) (em 0.975) (em 0)
    display block
  ".site-name" ? do
    lineHeight (em 1.3)
    fontSize (em 3)
    marginBottom (em 0.25)
    marginTop (em 0.25)
  ".site-description" ? do
    fontSize (em 0.8)
    margin (em 0.7) (em 0) (em 0.4) (em 0)
  ".main-navigation" ? do
    fontFamily ["Coustard", "Georgia"] [serif]
    fontWeight $ weight 400
    display block
    borderStyle solid
    borderWidth4 (px 1) (px 0) (px 1) (px 0)
    borderColor "#eee"
    ".nav-list" ? do
      listStyle none none none
      padding (px 0) (px 0) (px 0) (px 0)
      margin (px 0) (px 0) (px 0) (px 0)
      ".nav-item" ? do
        position relative
        margin (em 0.75) (em 0) (em 0.75) (em 0)
        padding (em 0) (em 1) (em 0) (em 1)
        maxHeight (em 1.5)
        fontSize (em 0.9)
        display inlineBlock

articles :: Css
articles = article ? do
  fontFamily ["Lora", "Georgia"] [serif]
  display block
  padding (em 1.25) (em 0) (em 2.5) (em 0)
  a ? do
    color "#7678ed"
    textDecoration none
  ".header" ? do
    fontFamily ["Coustard", "Georgia"] [serif]
    textAlign (alignSide sideCenter)
    ".title" ? do
      fontSize (em 2)
      fontWeight $ weight 400
      margin (em 0) (em 0) (em 0.4) (em 0)
      a ? do
        textDecoration none
        color black
  ".footer" ? do
    textAlign $ alignSide sideCenter
    fontSize (em 0.8)
    letterSpacing (px 1)
    color "#aaa"
  ".content" ? do
    lineHeight (em 1.6)
  ".full-content" ? do
    fontSize (px 18)
    lineHeight (em 1.6)
  ".read-more" ? do
    display block
    textAlign $ alignSide sideCenter
    letterSpacing (px 1)
    fontSize (em 1)
    a ? do
      color "#aaa"
      textDecoration none

siteFooter:: Css
siteFooter= "#footer" ? do
  a ? do
    textDecoration none
    color black
  fontSize (em 0.8)
  marginTop (px 12)
  padding (px 12) (px 0) (px 12) (px 0)
  textAlign (alignSide sideRight)
  borderStyle solid
  borderWidth4 (px 1) (px 0) (px 0) (px 0)
  borderColor "#eee"

syntaxHighlighting :: Css
syntaxHighlighting = "div.sourceCode" ? do
  overflowX scroll
  "span.kw" ? do
    fontWeight bold
    color "#007020"
  "span.dt" ? color "#902000"
  "span.dv" ? color "#40a070"
  "span.bn" ? color "#40a070"
  "span.fl" ? color "#40a070"
  "span.ch" ? color "#4070a0"
  "span.st" ? color "#4070a0"
  "span.co" ? do
    color "#60a0b0"
    fontStyle italic
  "span.ot" ? color "#007020"
  "span.al" ? do
    color red
    fontWeight bold
  "span.fu" ? color "#06287e"
  "span.er" ? do
    color red
    fontWeight bold

syntaxPadding :: Css
syntaxPadding = "table.sourceCode" Clay.**
                "tr.sourceCode" Clay.**
                "td.lineNumbers" Clay.**
                "td.sourceCode" Clay.**
                "table.sourceCode" Clay.**
                "pre" ? do
   margin (px 0) (px 0) (px 0) (px 0)
   padding (px 0) (px 0) (px 0) (px 0)
   border solid (px 0) white
   verticalAlign baseline

lineNumbers :: Css
lineNumbers = "td.lineNumbers" ? do
  borderRight solid (px 1) "#AAAAAA"
  textAlign (alignSide sideRight)
  color "#AAAAAA"
  paddingRight (px 5)
  paddingLeft (px 5)

main :: IO ()
main = putCss $ do
  defaults
  siteHeader
  articles
  siteFooter
  syntaxHighlighting
  syntaxPadding
  lineNumbers
