{-# LANGUAGE
    MultiWayIf
  , UnicodeSyntax
  #-}

import Hake

main ∷ IO ()
main = hake $ do
  "clean | clean the project" ∫
    cabal ["clean"] ?> removeDirIfExists buildPath
                    >> cleanCabalLocal

  "build deps | install all the dependencies" ∫
    cabal ["install", "--only-dependencies", "--overwrite-policy=always"]

  rekishiExecutable ♯
   let processBuild =
           cabalConfigure
        >> cabalBuild
        >> getCabalBuildPath appName >>=
            \p -> copyFile p rekishiExecutable
    in processBuild ?> cleanCabalLocal

  "install | install to system" ◉ [rekishiExecutable] ∰
    cabal ["install", "--overwrite-policy=always"]

 where
  appName ∷ String
  appName = "rekishi"

  buildPath ∷ String
  buildPath = "dist-newstyle"

  rekishiExecutable ∷ String
  rekishiExecutable =
    {- HLINT ignore "Redundant multi-way if" -}
    if | os ∈ ["win32", "mingw32", "cygwin32"] -> buildPath </> appName ++ "exe"
       | otherwise                             -> buildPath </> appName
