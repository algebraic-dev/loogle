import Lean

namespace Loogle

open Lean

/-- A minimal representation of HTML -/
inductive Html where
  | text (escape : Bool) (string : String)
  | tag (name : String) (attrs : Array (String × String)) (contents : Html)
  | seq (contents : Array Html)
deriving Repr, Inhabited

/-- The empty HTML document -/
def Html.empty : Html := .seq #[]

/-- Appends two HTML documents -/
def Html.append : Html → Html → Html
  | .seq xs, .seq ys => .seq (xs ++ ys)
  | .seq xs, other => .seq (xs.push other)
  | other, .seq ys => .seq (#[other] ++ ys)
  | x, y => .seq #[x, y]

instance : Append Html := ⟨Html.append⟩

/-- Converts an array of HTML elements into a single element -/
def Html.fromArray (htmls : Array Html) : Html :=
  .seq <| htmls.foldl glue #[]
where
  glue
    | arr, .seq hs => arr.append hs
    | arr, other => arr.push other

-- HTML void tags (self-closing)
private def voidTags : List String :=
  ["area", "base", "br", "col", "embed", "hr", "img", "input",
   "link", "meta", "param", "source", "track", "wbr"]

-- Tags that must have closing tags even when empty
private def mustClose : List String :=
  ["script", "style", "title", "textarea"]

-- Tags after which we insert newlines for readability
private def newlineAfter : List String :=
  ["html", "head", "body", "div", "p", "br", "meta", "script",
   "link", "style", "header", "footer", "nav", "section", "article"]

declare_syntax_cat tag_name
scoped syntax rawIdent : tag_name

declare_syntax_cat html
declare_syntax_cat attrib

scoped syntax (name := attrNamed) ident " = " str : attrib
scoped syntax (name := attrTerm) ident " = " "{{" term "}}" : attrib
scoped syntax (name := attrBool) ident : attrib

scoped syntax (name := attrNamedStr) str " = " str : attrib
scoped syntax (name := attrTermStr) str " = " "{{" term "}}" : attrib
scoped syntax (name := attrBoolStr) str : attrib


def _root_.Lean.TSyntax.tagName : TSyntax `tag_name → String
  | ⟨.node _ _ #[.atom _ x]⟩ => x
  | ⟨.node _ _ #[.ident _ _ x ..]⟩ => x.eraseMacroScopes.toString
  | _ => "unknown"

scoped syntax "{{" term "}}" : html
scoped syntax "[[[" term "]]]" : html
scoped syntax "<" tag_name attrib* ">" html* "</" tag_name ">" : html
scoped syntax "<" tag_name attrib* "/" ">" : html
scoped syntax str : html

scoped syntax "{{" html+ "}}" : term

open Elab Term Meta in
def elabAttrs (stxs : Array (TSyntax `attrib)) : TermElabM Expr := do
  let attrType ← mkAppM ``Prod #[.const ``String [], .const ``String []]
  let mut attrs : Expr ← mkArrayLit attrType []
  for stx in stxs do
    match stx with
    | `(attrib| $name:ident = $val:str) =>
      attrs ← mkAppM ``Array.push #[attrs, ← mkAppM ``Prod.mk #[toExpr name.getId.toString, toExpr val.getString]]
    | `(attrib| $name:ident = {{ $val:term }}) =>
      attrs ← mkAppM ``Array.push #[attrs, ← mkAppM ``Prod.mk #[toExpr name.getId.toString, ← elabTermEnsuringType val (some (.const ``String []))]]
    | `(attrib| $name:ident) =>
      attrs ← mkAppM ``Array.push #[attrs, ← mkAppM ``Prod.mk #[toExpr name.getId.toString, toExpr ""]]
    | `(attrib| $name:str = $val:str) =>
      attrs ← mkAppM ``Array.push #[attrs, ← mkAppM ``Prod.mk #[toExpr name.getString, toExpr val.getString]]
    | `(attrib| $name:str = {{ $val:term }}) =>
      attrs ← mkAppM ``Array.push #[attrs, ← mkAppM ``Prod.mk #[toExpr name.getString, ← elabTermEnsuringType val (some (.const ``String []))]]
    | `(attrib| $name:str) =>
      attrs ← mkAppM ``Array.push #[attrs, ← mkAppM ``Prod.mk #[toExpr name.getString, toExpr ""]]
    | _ => withRef stx throwUnsupportedSyntax
  return attrs

open Elab Term Meta in
partial def elabHtml (stx : TSyntax `html) : TermElabM Expr := withRef stx do
  match stx with
  | `(html| {{ $e:term }}) =>
    elabTermEnsuringType e (some (.const ``Html []))
  | `(html| [[[ $e:term ]]]) =>
    elabTermEnsuringType (← `(.seq $e)) (some (.const ``Html []))
  | `(html| $text:str) =>
    mkAppM ``Html.text #[toExpr true, toExpr text.getString]
  | `(html| <%$tk $tag:tag_name $[$extra]* >%$tk' $[$content:html]* </ $tag':tag_name>) => do
    if tag.tagName != tag'.tagName then
      throwErrorAt tag' m!"Mismatched closing tag, expected `{tag.tagName}` but got `{tag'.tagName}`"
    if tag.tagName ∈ voidTags then
      throwErrorAt tag m!"`<{tag.tagName}>` doesn't allow contents"
    let attrs ← elabAttrs extra
    let content ←
      if h : content.size = 1 then
        elabHtml content[0]
      else
        let content ← content.mapM elabHtml
        let content ← mkArrayLit (.const ``Html []) content.toList
        mkAppM ``Html.fromArray #[content]
    mkAppM ``Html.tag #[toExpr tag.tagName, attrs, content]
  | `(html| <$tag:tag_name $[$extra]* />) =>
    let attrs ← elabAttrs extra
    mkAppM ``Html.tag #[toExpr tag.tagName, attrs, ← mkAppM ``Html.empty #[]]
  | _ => throwUnsupportedSyntax

elab_rules : term
  | `(term| {{ $h:html }}) => withRef h <| elabHtml h
  | `(term| {{ $[$h:html]* }}) => do
    let h ← h.mapM fun (x : TSyntax `html) => withRef x <| elabHtml x
    Meta.mkAppM ``Html.fromArray #[← Meta.mkArrayLit (.const ``Html []) h.toList]

/-- Converts HTML into a readable string -/
partial def Html.asString (html : Html) (indent : Nat := 0) (breakLines := true) : String :=
  match html with
  | .text true str =>
    str.trimAscii.replace "<" "&lt;" |>.replace ">" "&gt;"
  | .text false str =>
    str.trimAscii.toString
  | .tag name attrs (.seq #[]) =>
    if name ∈ mustClose then
      "<" ++ name ++ attrsAsString attrs ++ "></" ++ name ++ ">" ++ breakline name
    else
      "<" ++ name ++ attrsAsString attrs ++ ">" ++ breakline name
  | .tag name attrs body =>
    let bodyStr := Html.asString body (indent := indent + 2) (breakLines := breakLines)
    let needsNewline := breakLines && name ∈ newlineAfter && !isInlineContent body
    "<" ++ name ++ attrsAsString attrs ++ ">" ++
    (if needsNewline then newline (indent + 2) else "") ++
    bodyStr ++
    (if needsNewline then newline indent else "") ++
    s!"</{name}>" ++ breakline name
  | .seq elts =>
    String.join (elts.toList.map (Html.asString · (indent := indent) (breakLines := breakLines)))
where
  newline i := "\n" ++ String.ofList (List.replicate i ' ')
  breakline tag := if breakLines && tag ∈ newlineAfter then newline indent else ""

  -- Check if content is inline (just text, no nested tags)
  isInlineContent : Html → Bool
    | .text _ _ => true
    | .seq #[] => true
    | .seq #[.text _ _] => true
    | _ => false

  attrsAsString xs := String.join <| xs.toList.map (fun ⟨k, v⟩ =>
    let k := if k == "className" then "class" else k
    s!" {k}=\"{escapeAttr v}\"")

  escapeAttr (str : String) := str |>.replace "&" "&amp;" |>.replace "\"" "&quot;"

instance : Coe String Html where
  coe := .text false

end Loogle
