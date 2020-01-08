# Nix PureScript Concur Frontend Starter

A starter template
for developing a frontend
with Nix,
PureScript,
and Concur.
Heavily modified
from the [PureScript Concur Starter](https://github.com/purescript-concur/purescript-concur-starter).

## What to change for a new project

After you clone this repository:

* update the default.nix nixpkgs version by updating 'rev'
* update the default.nix easy-purescript-nix version by updating 'rev'
* change the default.nix pname
* change the default.nix meta fields
* update node packages by running node2nix as per the readme
* update purescript packages by running spago2nix as per the readme
* change the index.html title
* change the source code

## Installing Node packages

Add package to
./pkgs.json
and rebuild the node packages derivation with
`node2nix -i pkgs.json -c node-packages.nix -o node-registry.nix`.

## Installing PureScript packages

Add package to
./spago.dhall
and rebuild the spago packages derivation with
`spago2nix generate`.

## Development

Enter a Nix shell with `nix-shell`.
Packages are installed when entering the Nix shell.
New packages can be installed without exiting Nix shell
with `eval "$configurePhase"`.

### Build code

> eval "$buildPhase"

### Test code

> eval "$checkPhase"

### Run Dev Server

> parcel start index.html

#### Hot code reload with PureScript code

The parcel dev server
will watch for Javascript changes,
and automatically reload the web page
with your changes.
Your IDE should watch for PureScript changes
and automatically compile to Javascript.
Together,
the webpage should automatically reload
with your PureScript changes.

## Production build

Run `nix-build`.
./result/dist/ contains
the compiled frontend
and ./result/bin/ contains
a simple hosting script.

## Styling elements

This template uses the CSS library
for typed CSS in PureScript.
A simple module to render this CSS
for Concur elements
is included
in src/CSS/Render/Concur/React.purs
.
Concur elements can use inline CSS like

```purs
...
import CSS.Render.Concur.React (style)
...

myEl = h1 [ style $ fontSize $ px 32.0 *> margin (px 0.0) (px 0.0) (px 0.0) (px 0.0) ]
```

Reusable Concur components can be written like

```purs
...
import CSS.Render.Concur.React (styledEl)
...

myStyle ∷ CSS
myStyle = do
  fontSize $ px 32.0
  margin (px 0.0) (px 0.0) (px 0.0) (px 0.0)

myComponent ∷ ∀ a. Widget HTML a
myComponent = styledEl h1 myStyle
```

Inline CSS is fast and easy
but does not allow pseudo-selectors
like :active.
The CSS library can also make stylesheets like

```purs
myClass ∷ String
myClass = "myclass"

myStylesheet ∷ CSS
myStylesheet = (element "" `with` Refinement [ Class myClass ]) do
  fontSize $ px 32.0
  margin (px 0.0) (px 0.0) (px 0.0) (px 0.0)

myEl ∷ ∀ a. Widget HTML a
myEl = h1 [ className myClass ] [ text "Heading" ]
```

Stylesheet CSS is useful
for CSS features
that inline CSS does not support.
Examples of styling elements
and rendering a stylesheet
are included in the source.
