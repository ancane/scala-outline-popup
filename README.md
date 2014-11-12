Scala outline popup [![MELPA Stable](http://stable.melpa.org/packages/scala-outline-popup-badge.svg)](http://stable.melpa.org/#/scala-outline-popup) [![MELPA](http://melpa.org/packages/scala-outline-popup-badge.svg)](http://melpa.org/#/scala-outline-popup)
===================

## Description
Opens a popup window containing classes, objects, types, defs and implicit vals from active scala file.
Keeps indentation of all the items. List is filterable. `Enter` on an item jumps to it's position in file.

![outline screenshot](https://github.com/ancane/scala-outline-popup/raw/master/outline-popup.png)

## Dependencies

* [popup](https://github.com/auto-complete/popup-el)
* [scala-mode](https://github.com/hvesalai/scala-mode2)

## Installation

From melpa.

## Usage

Run from scala-mode: `M-x scala-outline-popup`

## Keymap

If you assign `scala-outline-popup` command a hotkey (ex., `C-e`), you may wan't same key to close the popup.
This can be done by adding the key to `popup-isearch-keymap`:

```
(require 'popup)

(define-key popup-isearch-keymap (kbd "C-e") 'popup-isearch-cancel)
```

## Initial item selection

It's possible to have closest, previous or next definition selected, when you open the popup.

```
(require 'scala-outline-popup)

(setq scala-outline-popup-select 'closest) // 'next or 'prev
```
