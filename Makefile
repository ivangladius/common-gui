
all: build

build:
	git submodule init
	git submodule update
	make -C raylib/src PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=SHARED -j$(nproc)
