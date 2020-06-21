module DiPolysemy
    ( Di(..)
    , runDiToIO
    , runDiToStderrIO
    , runDiToIOFinal
    , runDiToStderrIOFinal
    , log
    , flush
    , local
    , reset
    , push
    , attr_
    , attr
    , debug
    , info
    , notice
    , warning
    , error
    , alert
    , critical
    , emergency
    , debug_
    , info_
    , notice_
    , warning_
    , error_
    , alert_
    , critical_
    , emergency_ ) where

import           Data.Functor

import qualified Df1                        as D

import qualified Di.Core                    as DC
import qualified Di.Df1                     as Df1
import qualified Di.Handle                  as DH

import           Polysemy
import qualified Polysemy.Reader as P

import           Prelude                    hiding ( error, log )
import Control.Concurrent (takeMVar, MVar, putMVar, newEmptyMVar)
import Polysemy.Async (asyncToIOFinal, async)
import Polysemy.Resource (resourceToIOFinal, bracket)

data Di level path msg m a where
  Log    :: level -> msg -> Di level path msg m ()
  Flush  :: Di level path msg m ()
  Local  :: (DC.Di level path msg -> DC.Di level path msg) -> m a -> Di level path msg m a
  Reset  :: m a -> Di level path msg m a

makeSem ''Di

data DiIOInner m a where
  RunDiIOInner :: (DC.Log level Df1.Path msg -> IO ()) -> (DC.Di level Df1.Path msg -> m a) -> DiIOInner m a

makeSem ''DiIOInner

diToIO :: forall r a. Member (Embed IO) r => Sem (DiIOInner ': r) a -> Sem r a
diToIO = interpretH
  (\case RunDiIOInner commit a -> do
           istate <- getInitialStateT
           ma <- bindT a

           withLowerToIO $ \lower finish -> do
             let done :: Sem (DiIOInner ': r) x -> IO x
                 done = lower . raise . diToIO

             DC.new commit (\di -> do
                               res <- done (ma $ istate $> di)
                               finish
                               pure res))

type DiR level msg = (DC.Di level Df1.Path msg, DC.Di level Df1.Path msg)

interpretDi :: forall r a level msg. Members '[Embed IO, P.Reader (DiR level msg)] r
      => Sem (Di level Df1.Path msg ': r) a
      -> Sem r a
interpretDi = interpretH $ \case
      Log level msg -> do
        (_, di) <- P.ask @(DiR level msg)
        (embed @IO $ DC.log di level msg) >>= pureT
      Flush         -> do
        (_, di) <- P.ask @(DiR level msg)
        (embed @IO $ DC.flush di) >>= pureT
      Local f m     -> do
        m' <- interpretDi <$> runT m
        raise $ P.local @(DiR level msg) (fmap f) m'
      Reset m       -> do
        m' <- interpretDi <$> runT m
        raise $ P.local @(DiR level msg) (\(odi, _) -> (odi, odi)) m'

dup :: a -> (a, a)
dup a = (a, a)

runDiToIOFinal
  :: forall r level msg a.
  Members '[Final IO, Embed IO] r
  => (DC.Log level Df1.Path msg -> IO ())
  -> Sem (Di level Df1.Path msg ': r) a
  -> Sem r a
runDiToIOFinal commit m = do
  diIn <- embedFinal newEmptyMVar
  diOut <- embedFinal newEmptyMVar
  void . asyncToIOFinal . async . embedFinal $ DC.new commit $ outer diIn diOut
  inner diIn diOut
  where
    outer :: MVar (DC.Di level Df1.Path msg) -> MVar (DC.Di level Df1.Path msg) -> (DC.Di level Df1.Path msg) -> IO ()
    outer aIn aOut a = do
      putMVar aIn a
      void $ takeMVar aOut

    inner :: MVar (DC.Di level Df1.Path msg) -> MVar (DC.Di level Df1.Path msg) -> Sem r a
    inner diIn diOut = resourceToIOFinal $ bracket
                       (embedFinal $ takeMVar diIn)
                       (embedFinal . putMVar diOut)
                       (\di -> raise . P.runReader (di, di) $ interpretDi (raiseUnder $ m))

runDiToStderrIOFinal :: Members '[Final IO, Embed IO] r => Sem (Di Df1.Level Df1.Path Df1.Message ': r) a -> Sem r a
runDiToStderrIOFinal m = do
  commit <- embedFinal @IO $ DH.stderr Df1.df1
  runDiToIOFinal commit m

runDiToIO
  :: forall r level msg a.
  Member (Embed IO) r
  => (DC.Log level Df1.Path msg -> IO ())
  -> Sem (Di level Df1.Path msg ': r) a
  -> Sem r a
runDiToIO commit m = diToIO inner
  where inner :: Sem (DiIOInner ': r) a
        inner = runDiIOInner commit (raise . flip (P.runReader . dup) (interpretDi $ raiseUnder m))


runDiToStderrIO :: Member (Embed IO) r => Sem (Di Df1.Level Df1.Path Df1.Message ': r) a -> Sem r a
runDiToStderrIO m = do
  commit <- embed @IO $ DH.stderr Df1.df1
  runDiToIO commit m

push :: forall level msg r a. Member (Di level Df1.Path msg) r => Df1.Segment -> Sem r a -> Sem r a
push s = local @level @Df1.Path @msg (Df1.push s)

attr_ :: forall level msg r a. Member (Di level Df1.Path msg) r => Df1.Key -> Df1.Value -> Sem r a -> Sem r a
attr_ k v = local @level @Df1.Path @msg (Df1.attr_ k v)

attr :: forall value level msg r a. (Df1.ToValue value, Member (Di level Df1.Path msg) r) => Df1.Key -> value -> Sem r a -> Sem r a
attr k v = attr_ @level @msg k (Df1.value v)

debug :: forall msg path r. (Df1.ToMessage msg, Member (Di Df1.Level path Df1.Message) r) => msg -> Sem r ()
debug = log @Df1.Level @path D.Debug . Df1.message

info :: forall msg path r. (Df1.ToMessage msg, Member (Di Df1.Level path Df1.Message) r) => msg -> Sem r ()
info = log @Df1.Level @path D.Info . Df1.message

notice :: forall msg path r. (Df1.ToMessage msg, Member (Di Df1.Level path Df1.Message) r) => msg -> Sem r ()
notice = log @Df1.Level @path D.Notice . Df1.message

warning :: forall msg path r. (Df1.ToMessage msg, Member (Di Df1.Level path Df1.Message) r) => msg -> Sem r ()
warning = log @Df1.Level @path D.Warning . Df1.message

error :: forall msg path r. (Df1.ToMessage msg, Member (Di Df1.Level path Df1.Message) r) => msg -> Sem r ()
error = log @Df1.Level @path D.Error . Df1.message

alert :: forall msg path r. (Df1.ToMessage msg, Member (Di Df1.Level path Df1.Message) r) => msg -> Sem r ()
alert = log @Df1.Level @path D.Alert . Df1.message

critical :: forall msg path r. (Df1.ToMessage msg, Member (Di Df1.Level path Df1.Message) r) => msg -> Sem r ()
critical = log @Df1.Level @path D.Critical . Df1.message

emergency :: forall msg path r. (Df1.ToMessage msg, Member (Di Df1.Level path Df1.Message) r) => msg -> Sem r ()
emergency = log @Df1.Level @path D.Emergency . Df1.message

debug_ :: forall path r. Member (Di Df1.Level path Df1.Message) r => Df1.Message -> Sem r ()
debug_ = log @Df1.Level @path D.Debug

info_ :: forall path r. Member (Di Df1.Level path Df1.Message) r => Df1.Message -> Sem r ()
info_ = log @Df1.Level @path D.Info

notice_ :: forall path r. Member (Di Df1.Level path Df1.Message) r => Df1.Message -> Sem r ()
notice_ = log @Df1.Level @path D.Notice

warning_ :: forall path r. Member (Di Df1.Level path Df1.Message) r => Df1.Message -> Sem r ()
warning_ = log @Df1.Level @path D.Warning

error_ :: forall path r. Member (Di Df1.Level path Df1.Message) r => Df1.Message -> Sem r ()
error_ = log @Df1.Level @path D.Error

alert_ :: forall path r. Member (Di Df1.Level path Df1.Message) r => Df1.Message -> Sem r ()
alert_ = log @Df1.Level @path D.Alert

critical_ :: forall path r. Member (Di Df1.Level path Df1.Message) r => Df1.Message -> Sem r ()
critical_ = log @Df1.Level @path D.Critical

emergency_ :: forall path r. Member (Di Df1.Level path Df1.Message) r => Df1.Message -> Sem r ()
emergency_ = log @Df1.Level @path D.Emergency
