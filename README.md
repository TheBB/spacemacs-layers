# spacemacs-layers

Miscellaneous layers for Spacemacs

## Installation
To install these layers, clone this repository into your `.emacs.d/private`
directory. Then add the layers you want to enable to
`dotspacemacs-configuration-layers` in your dotfile.

For this to work, you at least Spacemacs version 0.103.

## Contents

### evil-little-word

Provides the [little word](https://github.com/tarao/evil-plugins) text objects
for Spacemacs, enabling easier handling of CamelCase word boundaries. In recent
versions of Spacemacs, you can toggle `subword-mode` with `SPC t c`, allowing
regular word motions and text objects to work the same way. With this layer you
don't need a toggle.

### evil-indent-textobject

Spacemacs ships with the `evil-indent-textobject` plugin, which provides text
objects based on text indentation. Unfortunately, this package has some
[broken behaviour](https://github.com/cofi/evil-indent-textobject/issues/1) and
hasn't received updates for almost two years at the time of writing.

This layer provides a modified and improved version, adding these text objects:

- ii, ai: Block of text with same or higher indentation
- iI, aI: Block of text with same or higher indentation, including the first
  line above with smaller indentation
- iJ, aJ: Block of text with same or higher indentation, including the first
  lines above and below with smaller indentation

### encoding

For the moment, this layer only provides a binding on `SPC x e a` to find
non-ASCII characters in a buffer.
