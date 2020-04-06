#!/bin/sh

if [ "$TRAVIS_OS_NAME" = "osx" ]
then
  cabal install
  tar -C dist/build/dotenv -cvf dotenv-$TRAVIS_OS_NAME-x86_64.tar.gz dotenv
  mv dotenv-$TRAVIS_OS_NAME-x86_64.tar.gz ~/$TRAVIS_BUILD_NUMBER/
else
  cabal install -f 'static'
  tar -C dist/build/dotenv -cvf dotenv-$TRAVIS_OS_NAME-x86_64-static.tar.gz dotenv
  mv dotenv-$TRAVIS_OS_NAME-x86_64-static.tar.gz ~/$TRAVIS_BUILD_NUMBER/
fi
