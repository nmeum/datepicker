## README

An [fzf]-like tool to interactively select a date in a provided format.

![Example usage with journalctl(1)](https://files.8pit.net/img/datepicker-demo.gif)

### Motivation

A variety of command-line utilities ([at], [journalctl], [mpick], [khal], …) allow passing dates in different formats via command-line options.
Entering these dates in the required formats manually can be cumbersome and annoying.
Inspired by [fzf], this utility allows visually selecting a date interactively through a TUI, thereby easing specification of dates for other command-line utilities.

### Status

Basic functionally is operational and works.
However, I am not entirely happy with the keyboard navigation in the date selection view yet (see below).
Further, the command-line interface needs some refinement, specifically additional display options from [cal] (e.g. `-m`) would be nice to have.

### Installation

To install this utility clone the repository and run:

	$ cabal install

### Usage Example

By default, `datepicker` requires selection of both a date and a time and prints the selected date in the [RFC 1123] date format.
The behavior can be customized using several command-line flags, e.g. the `-f` option allows specification of a different date format.
This format must be specified using the format strings supported by the [formatTime] function from Haskell's `time` library.
A few example usages are provided below.

**at** — Execute a job and a specified time:

	$ echo "ls ~" | at -m -t "$(datepicker -f %0Y%m%d%H%M)"

**journalctl** — Select log entries newer than a given date:

	$ journalctl --since="$(datepicker -f '%Y-%m-%d %H:%M:%S')"

**mpick** — Select emails newer than a given date:

	$ mlist ~/mail/INBOX | mpick -t "date >= \"$(datepicker -d -f %Y-%m-%d)\""

Refer to the `--help` output for an overview of all supported command-line options.
The command-line interface is still subject to change, more options will likely be added in future versions.

### Key Bindings

Two input views are provided: (1) A date selection view and (2) a time selection view.

**Date Selection:**

* `Esc` / `q`: Abort selection, exit with non-zero exit status
* `Enter`: Confirm selection of focused date
* `Up` and `Down`: Change focus to date in previous/next week
* `Left` and `Right`: Change focus to previous/next date
	* Note: For the first/last day of the week the focus is changed to the previous/next month
	* This might change in a future version

**Time Selection:**

* `Esc` / `q`: Abort selection, exit with non-zero exit status
* `Enter`: Confirm selection of specified time
	* Note: If the time is invalid, confirming the selection won't be possible
	* In a future version, this may cause an error to be emitted
* `[0-9]`: Input a new digit at the highlighted location
* `Backspace`: Move cursor to previous time digit
* `Left` and `Right`: Move cursor to previous/next digit

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
[cal]: https://manpages.debian.org/unstable/ncal/cal.1.en.html
[at]: https://manpages.debian.org/unstable/at/at.1.en.html
[journalctl]: https://manpages.debian.org/unstable/systemd/journalctl.1.en.html#FILTERING_OPTIONS
[mpick]: https://manpages.debian.org/unstable/mblaze/mpick.1.en.html#EXAMPLES
[khal]: https://manpages.debian.org/unstable/khal/khal.1.en.html
[formatTime]: https://hackage.haskell.org/package/time/docs/Data-Time-Format.html#v:formatTime
[RFC 1123]: https://datatracker.ietf.org/doc/html/rfc1123
