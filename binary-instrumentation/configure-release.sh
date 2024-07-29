set -eux

cmake \
-G Ninja \
-D CMAKE_OSX_SYSROOT=/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk \
-D CMAKE_BUILD_TYPE=Release \
-D CMAKE_C_COMPILER_LAUNCHER=ccache \
-D CMAKE_CXX_COMPILER_LAUNCHER=ccache \
..
