# spacemacs-layers

My Spacemacs configuration. This includes dotfile (`init.el`) plus layers.

Layers prefixed with `bb` are opinionated personal configuration layers. All
others are *intended* to be generally useful for others (although in reality
they may of course not be).

## Installation
To install these layers, clone this repository into your `.emacs.d/private`
directory. Then add the layers you want to enable to
`dotspacemacs-configuration-layers` in your dotfile.

For this to work, you need at least Spacemacs version 0.103.

The fact that my `init.el` is present should not cause a problem.

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

### no-dots

By default it's impossible to ignore the dotted directories `.` and `..` in
`helm-find-files`, even if you use `helm-boring-file-regexp-list`. This layer
hacks it in, anyway.

Note that this works regardless of the value of `helm-ff-skip-boring-files` and
`helm-boring-file-regexp-list`. That functionality will continue to work as
before.

### operators

Adds some new evil operators. For now, just one: a narrowing operator on
`SPC n n`.
