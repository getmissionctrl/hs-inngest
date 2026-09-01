-- | Test suite entry point. Enumerated manually (rather than via
-- @hspec-discover@) so @cabal build@/@cabal test@ need no extra build tool —
-- important for a fresh Nix shell with no Hackage index.
module Main (main) where

import Test.Hspec

import qualified Inngest.TypesSpec
import qualified Inngest.ConfigSpec
import qualified Inngest.SigningSpec
import qualified Inngest.ClientSpec
import qualified Inngest.StepSpec
import qualified Inngest.FunctionSpec
import qualified Inngest.ExecutionSpec
import qualified Inngest.SyncSpec
import qualified Inngest.ServeSpec
import qualified Inngest.IntegrationSpec
import qualified Inngest.LoggingSpec

main :: IO ()
main = hspec $ do
  describe "Inngest.Types"       Inngest.TypesSpec.spec
  describe "Inngest.Config"      Inngest.ConfigSpec.spec
  describe "Inngest.Signing"     Inngest.SigningSpec.spec
  describe "Inngest.Client"      Inngest.ClientSpec.spec
  describe "Inngest.Step"        Inngest.StepSpec.spec
  describe "Inngest.Function"    Inngest.FunctionSpec.spec
  describe "Inngest.Execution"   Inngest.ExecutionSpec.spec
  describe "Inngest.Sync"        Inngest.SyncSpec.spec
  describe "Inngest.Serve"       Inngest.ServeSpec.spec
  describe "Inngest.Integration" Inngest.IntegrationSpec.spec
  describe "Inngest.Logging"     Inngest.LoggingSpec.spec
