{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

--import Control.Lens
import GHC.Wasm.Prim
import Control.Monad.IO.Class
import Control.Exception.Safe
import Control.Monad.Except
--import Control.Monad (when)
import System.IO
--import System.FilePath
--import System.Directory (createDirectoryIfMissing)
import Data.IORef
import Data.List (sort)
import qualified Data.Text as T

import qualified Pact.Core.Version as PI
import qualified Data.Version as V

import Pact.Core.Builtin
import Pact.Core.Environment
import Pact.Core.Persistence.MockPersistence
import Pact.Core.Pretty
import Pact.Core.Repl.Compile
import Pact.Core.Repl.Utils
import Pact.Core.Serialise
import Pact.Core.Info
import System.IO.Unsafe (unsafePerformIO)

main :: IO ()
main = error "not used, reactor module"


evalState :: IORef (ReplState ReplCoreBuiltin)
evalState = unsafePerformIO (newEvalState >>= newIORef)
{-# NOINLINE evalState #-}

evalResult :: IORef String
evalResult = unsafePerformIO (newIORef "")
{-# NOINLINE evalResult #-}

setResult :: String -> IO ()
setResult = writeIORef evalResult

resetState:: IO ()
resetState = newEvalState >>= writeIORef evalState

newEvalState:: IO (ReplState ReplCoreBuiltin)
newEvalState = do
  pdb <- mockPactDb serialisePact_repl_fileLocSpanInfo
  ee <- defaultEvalEnv pdb replBuiltinMap
  return (mkReplState' ee handleOutput)

  where
    handleOutput :: (Pretty a, MonadIO m) => FileLocSpanInfo -> a -> m ()
    handleOutput _ = liftIO . setResult . show . pretty


foreign export javascript "pactReset" pactReset :: IO ()
pactReset :: IO ()
pactReset = resetState

foreign export javascript "pactVersion" pactVersion :: IO JSString
pactVersion :: IO JSString
pactVersion = (pure . toJSString . V.showVersion) PI.version

foreign export javascript "pactBuiltins" pactBuiltins :: IO JSString
pactBuiltins :: IO JSString
pactBuiltins = (pure . toJSString . unlines) $ T.unpack <$> sort replCoreBuiltinNames

--foreign export javascript "pactBuiltinDoc" pactBuiltinDoc :: IO JSString
--pactBuiltinDoc :: IO JSString
--pactBuiltinDoc = (pure . toJSString . unlines) $ T.unpack <$> replCoreBuiltinNames

foreign export javascript "pactEval" pactEval :: JSString -> IO JSString
pactEval :: JSString -> IO JSString
pactEval input = do

  result <- evalReplM evalState $ do
    eout <- tryError $ interpretReplProgramDirect $ SourceCode "(interactive)" $ T.pack $ fromJSString $ input
    case eout of
      Right _ -> pure ()
      Left err -> do
          rstate <- getReplState
          --replCurrSource .== defaultSrc
          liftIO $ setResult $ T.unpack $ renderLocatedPactErrorFromState rstate err

  case result of
    Left err -> setResult $ T.unpack $ replError (SourceCode "(interactive)" "") err
    _ -> return ()

  toJSString <$> readIORef evalResult
