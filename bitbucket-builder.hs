{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}


import Shelly
import Data.Text as T
import System.IO
import Control.Monad
import Text.InterpolatedString.Perl6 (q, qc)
import System.Process as P
import Data.String
import Data.Maybe

import           Network.Bitbucket (push_to_downloads, git_clone
                                    , install_stack )
import           Network.Bitbucket.Types


{-# ANN module "HLint: ignore Use camelCase" #-}

default (T.Text)
pattern DEFAULT_STACK_URL = "https://github.com/commercialhaskell/stack/releases/download/v2.1.3/stack-2.1.3-linux-x86_64.tar.gz"

idt :: Text -> Text
idt = id

get_env' :: Text -> Sh Text
get_env' var =
    handle =<< get_env var
  where
    handle = maybe (terror $ "no such var " <> var) return

which' ::  Shelly.FilePath -> Sh Shelly.FilePath
which' prog =
  handle =<< which prog
  where
    msg = "couldn't find on PATH: " <> toTextIgnore prog
    handle = maybe (terror msg) return

main :: IO ()
main =  do
  hSetBuffering stdout LineBuffering
  shelly $ verbosely $ do
    bb_password     <- get_env' "BB_APP_PASSWORD"
    bb_userid       <- get_env' "BB_USERNAME"
    bb_repo_path    <- get_env' "BB_REPO_PATH"
    extra_git_args  <- T.words <$> get_env_text "EXTRA_GIT_ARGS"
    stack_url       <- get_env  "STACK_URL" >>=
                          \case Nothing -> return DEFAULT_STACK_URL
                                Just j  -> return j
    install_path_   <- get_env  "STACK_INSTALL_DIR" >>= \path ->
                          let path' = fromMaybe "~/.local/bin" path
                          -- perform any shell expansions:
                          in T.strip <$> bash "echo" [path']
    let
      install_path :: IsString a => a
      install_path  =  fromString $ T.unpack install_path_

    let bb_auth = Auth bb_userid bb_password
      -- i.e. the path bit of the URL
      -- e.g. github.com/username/repository.git

    echo "[+] checking some prereqs"

    forM_ ["tar", "curl", "git"] $ \tool -> do
      echo $ "   [-] " <> tool
      void $ which' $ fromText tool

    echo "[+] installing stack"
    void $ cmd "mkdir" "-p" install_path
    prependToPath install_path
    install_stack stack_url install_path

    let build_dir = "build"

    echo $ "[+] cloning" <> bb_repo_path
    mkdir_p "build"
    git_clone bb_auth extra_git_args bb_repo_path "build"

    chdir_p build_dir $ do

      echo "[+] setup stack"
      void $ cmd "stack" "setup"


      echo "[+] build"
      void $ cmd "stack" "build"

      echo "[+] tar for uploading"

      -- get package name and version
      [pkg, ver] <- T.words <$> cmd "stack" "ls" "dependencies" "--depth" "0"

      let pkg_ver :: Text
          pkg_ver = [qc|{idt pkg}-{idt ver}|]


          tgz     = fromText pkg_ver <.> "tgz"

      echo $ "  [-] make temp dir " <> pkg_ver
      mkdir_p $ fromText pkg_ver

      echo $ "  [-] copy executables into " <> pkg_ver
      void $ cmd "stack" "--local-bin-path" (fromText pkg_ver) "build" "--copy-bins"

      echo $ "  [-] tar into " <> T.pack (show tgz)
      void $ cmd "tar" "cvf" tgz pkg_ver

      --push_to_downloads bb_auth bb_userid "some-build" tgz

    echo "DONE"

