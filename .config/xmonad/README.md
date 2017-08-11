# Compile

Xmonad itself expects all libs to be located in the lib folder. GHC and Stack build expect the paths to be relative without lib folder. For a working setup (stack compile and xmonad --recompile) add a symlink from lib into the corresponding relative folder containing the included .hs files.
