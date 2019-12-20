#.PHONY: pythonbridge

UNAME := $(shell uname)

ifeq ($(UNAME), Linux)
	FLAGS := -Fl"./linux"
endif

ifeq ($(UNAME), Darwin)
	FLAGS := -WM10.10 -XR"/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk" -Fl"./darwin"
endif

all:
	mkdir -p ./static.debug.ppcx64
	fpc "./sources/Main.pas" -vbr -godwarfcpp -gw -FU"./static.debug.ppcx64" -o"./PythonBridge" -Fu"./sources" $(FLAGS)

clean:
	rm -f -r ./static.debug.ppcx64
