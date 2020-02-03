#!/bin/sh

if [ "$TRAVIS_OS_NAME" = "osx" ]
then
  cabal install
else
  cabal install -f 'static'
fi

tar -C dist/build/dotenv -cvf dotenv-$TRAVIS_OS_NAME-x86_64-static.tar.gz dotenv
mv dotenv-$TRAVIS_OS_NAME-x86_64-static.tar.gz ~/$TRAVIS_BUILD_NUMBER/
