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

```
git clone git@github.com:ancane/scala-outline-popup.git
```

Put the following into your Emacs init file.

```
(add-to-list 'load-path "/path/to/scala-outline-popup/")
(require 'scala-outline-popup)
```

## Usage

Run from scala-mode: `M-x scala-outline-popup`
