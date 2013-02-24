## Specifications

### Pages syntax

*Stone*'s pages are written in a easy-to-read and easy-to-write
formatting syntax : Markdown. In fact, Markdown-like : the parser I
use doesn't implements standard Markdown on some points (you can send
patches to [Cow](https://github.com/mirage/ocaml-cow), i would be
happy).

You can have a look at the Markdown specification
[here](http://daringfireball.net/projects/markdown/syntax) or at a
recap
[here](http://support.mashery.com/docs/customizing_your_portal/Markdown_Cheat_Sheet)

The differences between the imprementation used by *Stone* and the
standard are :

* Headings underlined by '`-`'s and '`=`'s are not supported (use '`#`'s)
* Trailing spaces to put a line break are not supported
* Code blocks must start with '`{{`' and end with '`}}`'

### Template

Templates for *Stone* are just plain html pages, with some magic
variables that will be replaced during the pages generation.

The variables you can use are the following :

* `PAGE_TITLE` : will be replaced by the title's page, if specified in
the `Pages` variables in the `config.stone` file. Otherwise, is
replaced by the page's filename prefix (filename without extension).
* `SITE_TITLE` : replaced by the website's title, specified in the
  `Title` variable in `config.stone`
* `CSS` : inserts a html snippet which includes the css stylesheet
  present in the `data/` directory (looks like `<link
  href="static//style.css" type="text/css" rel="stylesheet"/>`)
* `BAR` : replaced by the list of the pages listed in `Pages` in the
  `config.stone`. Each item is a link to the page, named by it's
  title.
* `CONTENT` : the content of the page itself, generated from the
  markdown of the page (in the `pages/` directory).

When inserting a variable in your template, you must surrond it by
'`$`'s : if the var is `VAR`, the string `$VAR$` must be inserted in the
template.

### Generated pages properties

* When you create a heading (with some '`#`'s), an anchor is created. It
  is named with the following pattern : if this is a `h3` title, with text `"My
  Title"`, the anchor's name will be `"h3-MyTitle"`.
  It also has class `"anchor-toc"`.
* The `li` item in the bar list which points out to the current page
  has the css class `current`. So in the bar of the specs page (for
  example), there is `<li class="current"><a href="specs.html">
  ... </li>`.
