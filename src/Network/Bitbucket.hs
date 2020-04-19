{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
-- {-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Network.Bitbucket
  where

import Shelly
import Data.Text as T
import System.IO
import Control.Monad
import Text.InterpolatedString.Perl6 (q, qc)
import System.Process as P
import Data.String
import Data.Maybe
import           Network.Bitbucket.Types

{-# ANN module "HLint: ignore Use camelCase" #-}

-- | wrapper around shell call of "git clone".
--
-- @
-- git_clone auth extra_git_args repo_path target_dir
-- @
--
-- Clone from @repo_path@ to @target_dir@, using the
-- specified uid/password authentication @auth@.
git_clone :: Auth -> [Text] -> Text -> Text -> Sh ()
git_clone (Auth uid password) extra_git_args repo_path target_dir  = do
  let
    final_url =
      [qc|https://{uid}:{password}@{repo_path}|]
  void $ cmd "git" "clone" extra_git_args final_url target_dir

-- | wrapper around call to "curl" to install stack.
--
-- @
-- install_stack stack_url stack_install_dir
-- @
--
-- Download .tgz from @stack_url@, extract the stack
-- binary within using @tar@, and install to @stack_install_dir@.
install_stack :: Text -> Text -> Sh ()
install_stack stack_url stack_install_dir = do
  mkdir_p $ fromText stack_install_dir

  let
    curl_cmd :: Text
    curl_cmd =
      [qc|
        curl -L {stack_url} | \
          tar xvz --wildcards --strip-components=1 \
            -C {stack_install_dir} '*/stack'
      |]

  -- prevent Shelly from collecting output etc.,
  -- just let it go straight to stdout/stderr/terminal.
  liftIO $ P.callProcess "bash" ["-c", T.unpack $ "set -x; " <> curl_cmd]

-- | wrapper around call to "curl" to upload a file to the
-- Bitbucket "Downloads" files of a repo.
--
-- @
-- push_to_downloads auth user_slug repo_slug file
-- @
--
-- Upload the file @file@ to the repository specified by
-- @user_slug/repo_slug@, using uid/password authentication @auth@.
push_to_downloads :: Auth -> Text -> Text -> Shelly.FilePath -> Sh ()
push_to_downloads (Auth uid password) user_slug repo_slug file =
  void $ cmd "curl" "-u" [qc|{uid}:{password|] "-X" "POST"
    [qc|https://api.bitbucket.org/2.0/repositories/{user_slug}/{repo_slug}/downloads|] "-F" "files=@{toTextIgnore file}"


