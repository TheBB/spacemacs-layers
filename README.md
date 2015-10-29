# spacemacs-layers

Miscellaneous layers for Spacemacs

Layers prefixed with `bb` are opinionated personal configuration layers. All
others are *intended* to be generally useful for others (although in reality
they may of course not be).

## Installation
To install these layers, clone this repository into your `.emacs.d/private`
directory. Then add the layers you want to enable to
`dotspacemacs-configuration-layers` in your dotfile.

For this to work, you need at least Spacemacs version 0.103.

## Contents

### encoding

For the moment, this layer only provides a binding on `SPC x e a` to find
non-ASCII characters in a buffer.

### evil-little-word

Provides the [little word](https://github.com/tarao/evil-plugins) text objects
for Spacemacs, enabling easier handling of CamelCase word boundaries. In recent
versions of Spacemacs, you can toggle `subword-mode` with `SPC t c`, allowing
regular word motions and text objects to work the same way. With this layer you
don't need a toggle.

### evil-shift-width

This is a small layer that facilitates letting `evil-shift-width` change
depending on mode. Configure the value of the variable `evil-shift-width-alist`,
which is an alist mapping modes to shift-widths. The modes can be either
symbols, lists of symbols or the symbol `t` (default), and the values can be
either integers or forms to be evaluated.

Tip: `python-mode` guesses the correct offset to use for each file. Put
`(python-mode . python-indent-offset)` in `evil-shift-width-alist` to update
`evil-shift-width` accordingly.

Tip: You can add `evil-shift-width/set-width` to any hook or as an advice to any
function that might change offset behaviour.

### modify-theme

This layer is deprecated and will be removed soon. Use the `theming` layer which
is now in Spacemacs 0.105 instead.

### no-dots

By default it's impossible to ignore the dotted directories `.` and `..` in
`helm-find-files`, even if you use `helm-boring-file-regexp-list`. This layer
hacks it in, anyway.

Note that this works regardless of the value of `helm-ff-skip-boring-files` and
`helm-boring-file-regexp-list`. That functionality will continue to work as
before.
