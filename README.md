fancy-battery
=============

[![License GPL 3][badge-license]][copying]

Display battery status in Emacs mode line ([Solarized Light][]):

![Battery status in mode line][screenshot]

Provides `fancy-battery-mode`, which is like the built-in
`display-battery-mode`, but more fancy: It shows just the remaining time, and
uses colours to indicate the status of the battery.  It is also more
customizable.

[badge-license]: 	https://img.shields.io/github/license/lunaryorn/fancy-battery.el.svg
[COPYING]: https://github.com/lunaryorn/fancy-battery.el/blob/master/COPYING
[Solarized Light]: https://github.com/bbatsov/solarized-emacs
[screenshot]: https://raw.githubusercontent.com/lunaryorn/fancy-battery.el/master/screenshot.png

Installation
------------

As usual, from [MELPA][] or [MELPA Stable][], with `M-x package-install RET
fancy-battery`.

In your [`Cask`][cask] file:

```cl
(source melpa)

(depends-on "fancy-battery")
```

In your `init.el`, `.emacs`, or other config file:

```cl
;; Start emacs with fancy-battery-mode enabled
(add-hook 'after-init-hook #'fancy-battery-mode)
```

[Cask]: https://github.com/cask/cask
[MELPA]: http://melpa.milkbox.net
[MELPA Stable]: http://melpa-stable.milkbox.net

Appearance
----------

The original fancy-battery-mode provided customizable faces
* `fancy-battery-critical` (inherited from your theme's `error` face)
* `fancy-battery-charging` (inherited from `success`)
* `fancy-battery-discharging` (inherited from `warning`)

These are still available in the stable build (master branch)

Customization
-------------

Customize `fancy-battery-critical`, `fancy-battery-charging`, and
`fancy-battery-discharging` to change the status colours used by the default
mode line format.

Customize `battery-update-interval` to change the interval at which battery
information is updated, and `battery-status-function` to add your own battery
status backend.

Coming soon: will provide a 'mode-line-format'-esque interface for customizing features such as
* power source
* format for battery time left to charge/discharge (e.g. minutes, hours, hours:minutes)
* battery capacity (mAh or mWh)
* rate of discharge
* battery temperature

License
-------

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see http://www.gnu.org/licenses/.

See [`COPYING`][copying] for details.
