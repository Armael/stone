## Obtaining *Stone*

### Using Opam (recommended)

*Stone* is written in OCaml. To install all the required dependancies
 and *Stone* itself, we can use the awesome OCamlPro `opam` package
 manager. If `opam` isn't packaged in your Linux distro/OS, see the
 [installation instructions](http://opam.ocamlpro.com/doc/Advanced_Install.html).

Once `opam` is initialized, just type:
{{
   opam install stone
}}

Now, the `stone` command is available.

### Manual installation (if you want the very last updates)

*Stone* has two dependancies :
 [Cow](https://github.com/mirage/ocaml-cow) and
 [Config_file](http://config-file.forge.ocamlcore.org/).
 
You have to install them first (you can do that using `opam`).
 
Then, get *Stone* sources:

{{
    git clone git://git.isomorphis.me/stone.git
}}

And then, build them:
{{
    cd stone/
    ./configure
    make
    cd ..
}}

Now you have a binary named `stone` in the `stone` folder.

