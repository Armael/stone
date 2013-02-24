CC := ocamlfind ocamlc
PACKAGES := cow,cow.syntax,config-file,unix
PP := camlp4o
CFLAGS := -linkpkg -I src/ -I src/packed/

SRC := util.ml params.ml template.ml conf.ml gen.ml stone.ml
PACKER_SRC := utils/pack_text.ml

SRC := $(patsubst %,src/%,$(SRC))
PACKER_SRC := $(patsubst %,src/%,$(PACKER_SRC))

PACKER := $(patsubst %.ml,%,$(PACKER_SRC))

PACKED_NAMES := config.stone example_index.md template.html style.css
PACKED := $(addsuffix _pak.ml, $(basename $(PACKED_NAMES)))
PACKED := $(patsubst %,src/packed/%,$(PACKED))

all: stone

stone: $(PACKED)
	$(CC) -package $(PACKAGES) -syntax $(PP) $(CFLAGS) -o $@ $(PACKED) $(SRC) 

src/packed/config_pak.ml: packer
	$(PACKER) data/config.stone src/packed/config_pak.ml

src/packed/example_index_pak.ml: packer
	$(PACKER) data/example_index.md src/packed/example_index_pak.ml

src/packed/template_pak.ml: packer
	$(PACKER) data/template.html src/packed/template_pak.ml

src/packed/style_pak.ml: packer
	$(PACKER) data/style.css src/packed/style_pak.ml

packer:
	$(CC) $(CFLAGS) -o $(PACKER) $(PACKER_SRC)

$(PACKED_SRC):

clean:
	rm -rf src/*.cmo
	rm -rf src/*.cmi
	rm -rf src/packed/*.cmo
	rm -rf src/packed/*.cmi
	rm -rf src/utils/*.cmo
	rm -rf src/utils/*.cmi

mproper: clean
	rm -rf stone
	rm -rf $(PACKER)
	rm -rf $(PACKED)
