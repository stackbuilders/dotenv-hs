echo 'STARTING'
cabal update
cabal build --only-dependencies --enable-static
cabal build --enable-executable-static
cp $(cabal list-bin dotenv) dotenv
echo 'DONE!'
