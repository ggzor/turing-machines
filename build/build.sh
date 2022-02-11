#/usr/bin/sh

set -e

echo "Updating cabal..."
cabal update

echo "Installing dependencies..."
cabal install --only-dependencies

echo "Building with cabal..."
cabal build --enable-executable-static

cp "`find . -type f -name turing-machine`" turing-machine

echo "Stripping symbols..."
strip turing-machine

apk --no-cache add upx

echo "Compressing with upx..."
upx turing-machine

