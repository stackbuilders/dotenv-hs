echo 'cabal update'
cabal update
echo 'build dependencies'
cabal build --only-dependencies --enable-static
echo 'build static executable'
cabal build --enable-executable-static
echo 'copy bin'
cp $(cabal list-bin dotenv) dotenv
echo 'finished'
