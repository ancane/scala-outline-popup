# Scala outline popup [![MELPA Stable](http://stable.melpa.org/packages/scala-outline-popup-badge.svg)](http://stable.melpa.org/#/scala-outline-popup) [![MELPA](http://melpa.org/packages/scala-outline-popup-badge.svg)](http://melpa.org/#/scala-outline-popup)

Opens a popup window containing classes, objects, types, defs and implicit vals from active scala file.
Keeps indentation of all the items. List is filterable. `Enter` on an item jumps to it's position in file.

![outline screenshot](https://github.com/ancane/scala-outline-popup/raw/master/outline-popup.png)

## Installation
From melpa.

## `M-x scala-outline-popup`

## Keymap

If you assign `scala-outline-popup` command a hotkey (ex., `C-e`), you may wan't same key to close the popup.
This can be done by adding the key to `popup-isearch-keymap`:

```
(require 'popup)

(define-key popup-isearch-keymap (kbd "C-e") 'popup-isearch-cancel)
```

## `scala-outline-popup-position`
Variable controls popup horizontal positioning.
Possible values are:

*  'center - opens popup at window center
*  'fill-column - center relative to fill-column (default setting)
*  'point - open popup at point

```
(setq scala-outline-popup-position 'point)
```

## Initial item selection

It's possible to have closest, previous or next definition selected, when you open the popup.

```
(setq scala-outline-popup-select 'closest) // 'next or 'prev
```

## Fuzzy matching with flx
Fuzzy matching is enabled by default.
Set `(setq scala-outline-popup-use-flx nil)` to disable it.

## Dependencies

* [dash](https://github.com/magnars/dash.el)
* [popup](https://github.com/auto-complete/popup-el)
* [scala-mode](https://github.com/hvesalai/scala-mode2)
* [flx-ido](https://github.com/lewang/flx)
