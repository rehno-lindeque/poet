# Copyright (c) 2009-2010, Rehno Lindeque.

TARGET_DIR = ../../bin
TARGET = ../../bin/poet
BUILD_BASE_DIR = ../../build
BUILD_DIR = ../../build/posix-ghc
OSI_DIR = ../../../../libraries/osi/api
SRC_DIR = ../../src/haskell
SRC = ../../src/haskell/*.hs $(OSI_DIR)/osix/semanticdb/*.hs
LIBS = ../../../../libraries/osi/implementations/qsemanticdb/lib/libqsemanticdbcd.a
#LIBS = ../../../../libraries/osi/implementations/qsemanticdb/lib/libqsemanticdbc.a
GHC_FLAGS = -lstdc++ -XRankNTypes

all: $(TARGET)

$(TARGET): $(TARGET_DIR) $(BUILD_DIR) $(SRC) $(LIBS)
	ghc $(GHC_FLAGS) --make $(SRC) -o $(TARGET) $(LIBS)

$(TARGET_DIR): 
	mkdir $(TARGET_DIR)

$(BUILD_DIR): $(BUILD_BASE_DIR)
	mkdir $(BUILD_DIR)

$(BUILD_BASE_DIR):  
	mkdir $(BUILD_BASE_DIR)

clean:
	rm -r $(TARGET_DIR)
	rm -r $(BUILD_BASE_DIR)
