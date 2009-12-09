# Copyright (c) 2009, Rehno Lindeque.

TARGET_DIR = ../../bin
TARGET = ../../bin/poet
BUILD_BASE_DIR = ../../build
BUILD_DIR = ../../build/posix-ghc
SRC_DIR = ../../src/haskell
SRC = ../../src/haskell/*.hs

all: $(TARGET)

$(TARGET): $(TARGET_DIR) $(BUILD_DIR) $(SRC)
	ghc --make $(SRC) -o $(TARGET)

$(TARGET_DIR): 
	mkdir $(TARGET_DIR)

$(BUILD_DIR): $(BUILD_BASE_DIR)
	mkdir $(BUILD_DIR)

$(BUILD_BASE_DIR):  
	mkdir $(BUILD_BASE_DIR)

clean:
	rm -r $(TARGET_DIR)
	rm -r $(BUILD_BASE_DIR)
