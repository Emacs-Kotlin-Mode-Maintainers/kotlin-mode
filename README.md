# Emacs Kotlin Mode

[![MELPA](https://melpa.org/packages/kotlin-mode-badge.svg)](https://melpa.org/#/kotlin-mode)

In a sense the title says it all, this is an [Emacs](https://www.gnu.org/software/emacs/) major mode for
editing [Kotlin](http://kotlinlang.org/) source code – and in the future [Gradle](http://gradle.org/) build
files.

If you are using Emacs 24 or later, use the package management system to install this mode from
[MELPA](http://melpa.org/) (*).

If you are using an earlier version of Emacs, you are probably best advised to upgrade Emacs to a version
with package management so you can use package management to install the mode from MELPA.



(*) As at 2016-06-20 this mode is only in MELPA and not in MELPA stable, a release will be made when a
version is ready to be declared.

## Kotlin REPL key binding

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Key</th>
<th scope="col" class="org-left">Function</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">C-c C-z</td>
<td class="org-left">Start REPL</td>
</tr>


<tr>
<td class="org-left">C-c C-n</td>
<td class="org-left">Send current line to REPL</td>
</tr>


<tr>
<td class="org-left">C-c C-r</td>
<td class="org-left">Send selected region to REPL</td>
</tr>


<tr>
<td class="org-left">C-c C-c</td>
<td class="org-left">Send current code block to REPL</td>
</tr>


<tr>
<td class="org-left">C-c C-b</td>
<td class="org-left">Send whole buffer to REPL</td>
</tr>
</tbody>
</table>
