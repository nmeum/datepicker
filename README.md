## README

An [fzf]-like tool to interactively select a date in a provided format.

![Example usage with journalctl(1)](https://files.8pit.net/img/datepicker-demo.gif)

### Motivation

A variety of command-line utilities ([journalctl], [mpick], [khal], …) allow passing dates in different formats via command-line options.
Entering these dates in the required formats manually can be cumbersome and annoying.
Inspired by [fzf], this utility allows visually selecting a date interactively through a TUI, thereby easing specification of dates for other command-line utilities.

### Installation

To install this utility clone the repository and run:

	$ cabal install

### Usage Example

Presently, `datepicker` only supports a single command-line argument: The date format.
This format must be specified using the format strings supported by the [formatTime] function from Haskell's `time` library.
For example, in order to use this utility with [journalctl] you may invoke it as follows:

	$ journalctl --since="$(datepicker '%Y-%m-%d %H:%M:%S')"

The command-line interface is subject to change, more options will be added shortly.

### License

This program is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
Public License for more details.

You should have received a copy of the GNU General Public License along
with this program. If not, see <https://www.gnu.org/licenses/>.

[fzf]: https://github.com/junegunn/fzf
[journalctl]: https://manpages.debian.org/unstable/systemd/journalctl.1.en.html#FILTERING_OPTIONS
[mpick]: https://manpages.debian.org/unstable/mblaze/mpick.1.en.html#EXAMPLES
[khal]: https://manpages.debian.org/unstable/khal/khal.1.en.html
[formatTime]: https://hackage.haskell.org/package/time/docs/Data-Time-Format.html#v:formatTime
