# Compile

XMonad itself expects all libs to be located in the lib folder. GHC and Stack build expect the paths to be relative wihouth lib folder. For a working setup (Stack compile and Xmonad xmonad --recompile) perform a symlink from lib into the corresponding relative folder containing
the included .hs files
