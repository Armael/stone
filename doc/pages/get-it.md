## Obtaining *Stone*

Obtaining *Stone* is achieved by building it from source. It just
takes a few commands to do that, but you will have first to install
the required libraries.

### Build instructions

*Stone* is written in OCaml. To install all the required dependancies,
 we will use the awesome OCamlPro `opam` package manager. If `opam`
 isn't packaged in your Linux distro/OS, see the
 [installation instructions](http://opam.ocamlpro.com/doc/Advanced_Install.html).

Once OPAM is initialized, install the two required libraries
 ([Cow](https://github.com/mirage/ocaml-cow) and
 [Config_file](http://config-file.forge.ocamlcore.org/)):
 
 {{
     opam install cow config-file
 }}

Now that we have setup the compilation environnment, we can get
*Stone* sources':

{{
    git clone git://git.isomorphis.me/stone.git
}}

And then, build them:
{{
    cd stone/
    make
    cd ..
}}

That's it! Now you have a binary named `stone` in the `stone` folder
(how surprising!).
