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

By default, `datepicker` requires selection of both a date and a time and prints the selected date in the [RFC 1123] date format.
The behavior can be customized using several command-line flags, e.g. the `-f` option allows specification of a different date format.
This format must be specified using the format strings supported by the [formatTime] function from Haskell's `time` library.
For example, in order to use this utility with [journalctl] you may invoke it as follows:

	$ journalctl --since="$(datepicker -f '%Y-%m-%d %H:%M:%S')"

Refer to the `--help` output for an overview of all supported command-line options.
The command-line interface is still subject to change, more options will likely be added in future versions.

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
[RFC 1123]: https://datatracker.ietf.org/doc/html/rfc1123
