import Lean.Parser
import Lean.Data.RBMap

namespace Server.Jsx
open Lean

def Attributes := RBMap String String compare
instance : ToString Attributes := ⟨λ as => as.fold (λ s n v => s ++ s!" {n}=\"{v}\"") ""⟩

mutual
inductive Element
  | Element
    (name : String)
    (attributes : Attributes)
    (content : Array Content)
  | Fragment
    (content : Array Content)

inductive Content
  | Element (element : Element)
  | Comment (comment : String)
  | Character (content : String)
  deriving Inhabited
end

mutual

private partial def eToString : Element → String
  | Element.Fragment c =>
    c.map cToString |>.foldl (· ++ ·) ""
  | Element.Element n a c =>
    if c.isEmpty then
      s!"<{n}{a}/>"
    else
      s!"<{n}{a}>{c.map cToString |>.foldl (· ++ ·) ""}</{n}>"

private partial def cToString : Content → String
  | Content.Element e => eToString e
  | Content.Comment c => s!"<!--{c}-->"
  | Content.Character c => c

end
instance : ToString Element := ⟨eToString⟩
instance : ToString Content := ⟨cToString⟩

declare_syntax_cat jsxElement
declare_syntax_cat jsxChild

open Lean
open Parser PrettyPrinter

instance : Coe String Content where
  coe := Content.Character

instance : Coe Element Content where
  coe := Content.Element

instance : Coe (Array Element) (Array Content) where
  coe x := x.map Coe.coe

def jsxText : Parser :=
  withAntiquot (mkAntiquot "jsxText" `jsxText) {
    fn := fun c s =>
      let startPos := s.pos
      let s := takeWhile1Fn (fun c => not ("[{<>}]$".contains c)) "expected jsx text" c s
      mkNodeToken `jsxText startPos true c s }

@[combinator_formatter jsxText]
def jsxText.formatter : Formatter := pure ()

@[combinator_parenthesizer jsxText]
def jsxText.parenthesizer : Parenthesizer := pure ()

syntax jsxAttrName   := rawIdent <|> str
syntax jsxAttrVal    := str <|> group("{" term "}")
syntax jsxSimpleAttr := jsxAttrName "=" jsxAttrVal
syntax jsxAttrSpread := "[" term "]"
syntax jsxAttr       := jsxSimpleAttr <|> jsxAttrSpread

syntax jsxTag := rawIdent <|> str

syntax "<" jsxTag jsxAttr* "/>" : jsxElement
syntax "<" jsxTag jsxAttr* ">" jsxChild* "</" jsxTag ">" : jsxElement
syntax "<>" jsxChild* "</>" : jsxElement

scoped syntax jsxText      : jsxChild
scoped syntax "{" term "}" : jsxChild
scoped syntax "[" term "]" : jsxChild
scoped syntax jsxElement   : jsxChild

scoped syntax:max jsxElement : term

def parseAttrVal : Syntax → MacroM (TSyntax `term)
  | `(jsxAttrVal| {$value}) => pure value
  | `(jsxAttrVal| $value:str) => pure value
  | _ => Macro.throwUnsupported

def parseNameVal : Syntax → MacroM (TSyntax `str)
  | `(jsxAttrName| $name:str) => pure $ name
  | `(jsxAttrName| $name:ident) => pure $ (Lean.Quote.quote (toString name.getId))
  | _ => Macro.throwUnsupported

def parseAttr (last: TSyntax `term) : Syntax → MacroM (TSyntax `term)
  | `(jsxAttr| [$t]) =>
    `((RBMap.foldl $t (λm (k, v) => m.insert k v) $last))
  | `(jsxAttr| $name = $val) => do
    let name ← parseNameVal name
    let val ← parseAttrVal val
    `((RBMap.insert $last $name $val))
  | _ => Macro.throwUnsupported

def parseAttrs (attrs : Array Syntax) : MacroM (TSyntax `term) := do
  attrs.foldlM parseAttr (← `(RBMap.empty))

def name (text: Syntax) : (TSyntax `str) := Lean.Quote.quote (toString text.getId)

def tagGen : TSyntax `Server.Jsx.jsxTag → MacroM String
  | `(jsxTag| $str:str) => pure str.getString
  | `(jsxTag| $id:ident) => pure id.getId.toString
  | _ => Macro.throwUnsupported

def nameT (text: TSyntax α) : (TSyntax `str) := Lean.Quote.quote (toString text.raw[0].getAtomVal)

def parseJsxChild (last: TSyntax `term) : TSyntax `jsxChild → MacroM (TSyntax `term)
  | `(jsxChild| $element:jsxElement) => `(Array.push $last (Server.Jsx.Content.Element $element:jsxElement))
  | `(jsxChild| { $term:term }) => `(Array.push $last $term)
  | `(jsxChild| [ $term:term ]) => `($last ++ $term)
  | `(jsxChild| $text:jsxText) => `(Array.push $last (Server.Jsx.Content.Character $(nameT text)))
  | _ => Macro.throwUnsupported

def toFragment : Option Element → Element
  | some res => res
  | none => Element.Fragment #[]

macro_rules
  | `(<>$children*</>) => do
    let array ← `(Array.empty)
    let children ← children.foldlM parseJsxChild array
    `(Server.Jsx.Element.Fragment $children)
  | `(<$tag $attrs* />) => do
    let tag ← tagGen tag
    `(Server.Jsx.Element.Element $(Lean.Quote.quote tag) $(← parseAttrs attrs) #[])
  | `(<$ope $attrs* >$children*</$close>) => do
    let ope ← tagGen ope
    let close ← tagGen close
    if ope ≠ close then
      Macro.throwError s!"expected </{ope}>"
    let array ← `(Array.empty)
    let children ← children.foldlM parseJsxChild array
    `(Server.Jsx.Element.Element $(Lean.Quote.quote ope) $(← parseAttrs attrs) $children)
