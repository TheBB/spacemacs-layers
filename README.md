# spacemacs-layers

Miscellaneous layers for Spacemacs

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

### modify-theme

Usually, to modify a theme, you have to call `custom-set-face`. Unfortunately,
modifications introduced in this manner apply to *all* themes, which is bad if
you like to change themes and modify just the faces you are unhappy with in each
of them.

To use this layer, write your configurations in the `modify-theme-modifications`
variable. This should be set before layer load time, so do it in
`dotspacemacs/init`, or via a `:variables` entry in `dotspacemacs/layers`.

This is an alist mapping themes to a list of modifications, which is in turn an
alist mapping faces to face specs (see the documentation of `defface` for
details on the latter). Thus, to make strings and comments italic in monokai,
you would do this:

```lisp
(setq-default modify-theme-modifications
              '((monokai . ((font-lock-comment-face ((t (:slant italic))))
                            (font-lock-string-face ((t (:slant italic))))))))
```

You can use the symbol `t` to apply modifications to *all* themes.

This works by advising `load-theme`, and applying modifications to the `user`
theme after a theme is loaded using `custom-set-faces`. This means that any
other usage of `custom-set-faces` will conflict with this layer.

Three variables are provided for common customizations. You can set these in the
`:variables` block in `dotspacemacs/layers`. Each can be set to a list of themes
where the given modification will apply, or the symbol `all`, in which case that
modification will apply to all themes.

- `modify-theme-headings-inherit-from-default`: Set the `inherit` attribute to
  `default` on all header faces. This will typically make them all monospaced.
- `modify-theme-headings-same-size`: Set the `height` attribute to 1 on all
  header faces. This should make them all the same size.
- `modify-theme-headings-bold`: Set the `weight` attribute to `bold` on all
  header faces. This is useful if you used `modify-theme-headings-same-size` and
  are now having trouble distinguishing headers because they used to be large
  but not bold.

These automatic modifications can be overridden by others in
`modify-theme-modifications`.

**Note:** The list of header faces might be incomplete. See
`modify-theme--header-faces`. If you notice anything missing, please make a PR.

### no-dots

By default it's impossible to ignore the dotted directories `.` and `..` in
`helm-find-files`, even if you use `helm-boring-file-regexp-list`. This layer
hacks it in, anyway.

Note that this works regardless of the value of `helm-ff-skip-boring-files` and
`helm-boring-file-regexp-list`. That functionality will continue to work as
before.
