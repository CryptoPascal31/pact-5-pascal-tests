{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import GHC.Wasm.Prim
import Control.Monad.IO.Class
import Control.Monad.Except

import Data.IORef
import Data.Coerce
import Data.Maybe
import Data.List (sort)
import qualified Data.Text as T

import qualified Pact.Core.Version as PI
import qualified Data.Version as V

import Pact.Core.Builtin
import Pact.Core.Environment
import Pact.Core.Persistence.MockPersistence
import Pact.Core.Pretty
import Pact.Core.Repl.BuiltinDocs
import Pact.Core.Repl.BuiltinDocs.Internal
import qualified Pact.Core.Syntax.Lexer as LEX
import qualified Pact.Core.Syntax.Parser as PARS
import qualified Data.Map.Strict as M
import Pact.Core.Errors
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

setTextResult :: T.Text -> IO ()
setTextResult = setResult . T.unpack

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


jsToText :: JSString -> T.Text
jsToText = T.pack . fromJSString

textToJs :: T.Text -> JSString
textToJs = toJSString . T.unpack

jsToSource :: JSString -> SourceCode
jsToSource =  (SourceCode "(interactive)") . jsToText

foreign export javascript "pactReset" pactReset :: IO ()
pactReset :: IO ()
pactReset = resetState

foreign export javascript "pactVersion sync" pactVersion :: IO JSString
pactVersion :: IO JSString
pactVersion = (pure . toJSString . V.showVersion) PI.version

foreign export javascript "pactBuiltins sync" pactBuiltins :: IO JSString
pactBuiltins :: IO JSString
pactBuiltins = (pure . toJSString . unlines) $ T.unpack <$> sort replCoreBuiltinNames

foreign export javascript "pactBuiltinDoc sync" pactBuiltinDoc :: JSString -> IO JSString
pactBuiltinDoc :: JSString -> IO JSString
pactBuiltinDoc = pure . textToJs . handleMissing . getDoc . jsToText
  where
    getDoc k = coerce <$> M.lookup k builtinDocs
    handleMissing = fromMaybe "# Missing doc"


foreign export javascript "pactEval" pactEval :: JSString -> IO JSString
pactEval :: JSString -> IO JSString
pactEval input = do

  result <- evalReplM evalState $ do
    eout <- (tryError . interpretReplProgramDirect . jsToSource)  input
    case eout of
      Right _ -> pure ()
      Left err -> do
          rstate <- getReplState
          --replCurrSource .== defaultSrc
          liftIO $ setTextResult $ renderLocatedPactErrorFromState rstate err

  case result of
    Left err -> setTextResult $ replError (SourceCode "(interactive)" "") err
    _ -> return ()

  toJSString <$> readIORef evalResult

foreign export javascript "pactCheckCompleteExpression sync" pactCheckCompleteExpression :: JSString -> IO Bool
pactCheckCompleteExpression :: JSString -> IO Bool
pactCheckCompleteExpression input =
  pure $ case doParse input of
            Left (PEParseError (ParsingError e) _) | isMissingBracketerror e -> False
            _ -> True
  where
    doParse = \x -> LEX.lexer (jsToText x) >>= PARS.parseReplProgram
    isMissingBracketerror e = e `elem` ["[')']", "['(']", "[']']", "['}']"]