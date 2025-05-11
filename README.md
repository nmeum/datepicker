## README

An [fzf]-like tool to interactively select a date in a provided format.

![Example usage with at(1) job scheduling](https://files.8pit.net/img/datepicker-demo-20250101.gif)

### Motivation

A variety of command-line utilities ([at], [journalctl], [mpick], [khal], …) allow passing dates in different formats via command-line options.
Entering these dates in the required formats manually can be cumbersome and annoying.
Inspired by [fzf], this utility allows visually selecting a date interactively through a TUI, thereby easing specification of dates for other command-line utilities.

### Status

The currently implemented feature set works well, more advanced features (like [fzf-like previews][fzf preview]) may be added in a future version.

### Installation

If you have a Haskell development environment setup and a compatible GHC version installed, you can install this utility by cloning the repository and running:

	$ cabal install

Alternatively, if you don't have a Haskell development environment, you can also install this software using [Guix].
Guix will automatically install a supported GHC version for you and add the binary to your `$PATH`.
In order to install `datepicker` this way run the following command

	$ guix time-machine -C channels.scm -- package -f package.scm

### Tests

A test suite is available, it performs several checks on the TUI using [tmux] and requires the `datepicker` binary in your `$PATH`.
Once these requirements are satisfied, it can be invoked using:

	$ cabal test

### Usage Examples

By default, `datepicker` requires selection of both a date and a time and prints the selected date in the [RFC 1123] date format.
The behavior can be customized using several command-line flags, e.g. the `-f` option allows specification of a different date format.
This format must be specified using the format strings supported by the [formatTime] function from Haskell's `time` library.
A few example usages are provided below.

**at** — Execute a job at a specified time which is selected based on the current year (`-y`):

	$ echo "ls ~" | at -m -t "$(datepicker -y -f %0Y%m%d%H%M)"

**journalctl** — Select log entries newer than a given date from a span of three months (`-3`):

	$ journalctl --since="$(datepicker -3 -f '%Y-%m-%d %H:%M:%S')"

**mpick** — Select emails newer than a given date in the current month, skipping time selection (`-d`):

	$ mlist ~/mail/INBOX | mpick -t "date >= \"$(datepicker -d -f %Y-%m-%d)\""

Refer to the `--help` output for an overview of all supported command-line options.

### Key Bindings

Two input views are provided: (1) A date selection view and (2) a time selection view.

**Date Selection:**

* `Esc` / `q`: Abort selection, exit with non-zero exit status
* `Enter`: Confirm selection of focused date
* `Up` and `Down`: Change focus to date in previous/next week
* `Left` and `Right`: Change focus to previous/next date

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
[Guix]: https://guix.gnu.org
[formatTime]: https://hackage.haskell.org/package/time/docs/Data-Time-Format.html#v:formatTime
[RFC 1123]: https://datatracker.ietf.org/doc/html/rfc1123
[tmux]: https://tmux.github.io
[fzf preview]: https://manpages.debian.org/unstable/fzf/fzf.1.en.html#PREVIEW_WINDOW
