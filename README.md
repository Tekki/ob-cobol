# ob-cobol

![](img/ob-cobol.gif)

## Overview

`ob-cobol` enables
[Org-Babel](http://orgmode.org/worg/org-contrib/babel/intro.html)
support for evaluating COBOL code.

```cobol
#+begin_src cobol
IDENTIFICATION DIVISION.
    PROGRAM-ID. ob-cobol.

PROCEDURE DIVISION.
    DISPLAY "Hello COBOL".
#+end_src
```
```
#+RESULTS:
: Hello COBOL
```

Open the compiled source file with `M-x ob-cobol-last-src-file`.

This package was created with
[ob-rust](https://github.com/micanzhang/ob-rust) as model.

## Rules

* Default source format is `free`.
* Code with identification and procedure division will be compiled
  unchanged.
* A missing identification division will be added automatically.
* If there are no divisions, all the code will be moved to the
  procedure division.
* If a block with no divisions contains a line consisting of a
  horizontal ellipsis (`…`, Unicode U+2026, `C-k ,.` in evil-mode),
  the part before the ellipsis will be added to the working-storage
  section, the rest to the procedure division.

## Examples

The following two blocks will produce the same result as the code from
the overview. The first without identification division:

```cobol
#+begin_src cobol
PROCEDURE DIVISION.
    DISPLAY "Hello COBOL".
#+end_src
```

And the second without any divisions:

```cobol
#+begin_src cobol
    DISPLAY "Hello COBOL".
#+end_src
```

Code with variables can be abbreviated as

```cobol
#+begin_src cobol
    01 username PIC X(30) VALUE "Mainframe".
    …
    DISPLAY "Your name is " FUNCTION trim(username).
#+end_src
```

and will be expanded to

```cobol
IDENTIFICATION DIVISION.
    PROGRAM-ID. sample01.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 username PIC X(30) VALUE "Mainframe".

PROCEDURE DIVISION.
    DISPLAY "Your name is " FUNCTION trim(username).
```

The default source format is changed for a single code block using the
`source-format` header

```cobol
#+begin_src cobol :source-format fixed
000100 01 username PIC X(30) VALUE "Mainframe B".
   …
000200 DISPLAY "Your name is " FUNCTION trim(username).
#+end_src
```

or globally in the init file

```elisp
(setq ob-cobol-source-format "fixed")
```

In the same way you can change the dialect of the compiler with the
`dialect` header

```cobol
#+begin_src cobol :dialect ibm
*> some code in IBM COBOL
#+end_src
```

or in the init file

```elisp
(setq ob-cobol-dialect "ibm")
```

## Installation

### ob-cobol

Clone this repo to a folder on your desktop.

```bash
cd ~/git
git clone https://github.com/Tekki/ob-cobol
```

This will download the code to `~/git/ob-cobol`. Then add it to your
init file.

```elisp
(use-package ob-cobol
  :load-path "~/git/ob-cobol/")
```

If you want to work in IBM dialect with fixed instead of free format,
change the default.

```elisp
(use-package ob-cobol
  :load-path "~/git/ob-cobol/"
  :config
  (setq ob-cobol-dialect "ibm"
        ob-cobol-source-format "fixed"))
```

### GnuCOBOL

`ob-cobol` expects a [GnuCOBOL](https://gnucobol.sourceforge.io/)
compiler accessible under `cobc`. If it is not located in the path,
you can change variable `ob-cobol-compiler`. Another compiler will
only work if it uses the same command line arguments.

### cobol-mode

[cobol-mode](https://elpa.gnu.org/packages/cobol-mode.html) is not
required, but useful for syntax highlighting. Remember to set the
correct source format.

```elisp
(use-package cobol-mode
  :ensure t
  :config
  (setq cobol-source-format 'free))
```

## License

`ob-cobol` is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

This program is distributed in the hope that it will be useful but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the [GNU
General Public License](https://www.gnu.org/licenses/gpl-3.0.txt) for
more details.
