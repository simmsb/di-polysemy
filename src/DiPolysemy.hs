module DiPolysemy
    ( Di(..)
    , runDiToIO
    , runDiToStderrIO
    , log
    , flush
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

import           Prelude                    hiding ( error, log )

data Di level path msg m a where
  Log    :: level -> msg -> Di level path msg m ()
  Flush  :: Di level path msg m ()
  Push   :: D.Segment -> m a -> Di level D.Path msg m a
  Attr_  :: D.Key -> D.Value -> m a -> Di level D.Path msg m a

makeSem ''Di

data DiIOInner m a where
  RunDiIOInner :: (DC.Log level D.Path msg -> IO ()) -> (DC.Di level D.Path msg -> m a) -> DiIOInner m a

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

runDiToIO
  :: forall r level msg a.
  Member (Embed IO) r
  => (DC.Log level D.Path msg -> IO ())
  -> Sem (Di level D.Path msg ': r) a
  -> Sem r a
runDiToIO commit m = diToIO $ runDiIOInner commit (`go` raiseUnder m)
  where
    go :: Member (Embed IO) r0 => DC.Di level D.Path msg -> Sem (Di level D.Path msg ': r0) a0 -> Sem r0 a0
    go di m = (`interpretH` m) $ \case
      Log level msg -> do
        t <- embed $ DC.log di level msg
        pureT t
      Flush         -> do
        t <- embed $ DC.flush di
        pureT t
      Push s m'     -> do
        mm <- runT m'
        raise $ go (Df1.push s di) mm
      Attr_ k v m'  -> do
        mm <- runT m'
        raise $ go (Df1.attr_ k v di) mm

runDiToStderrIO :: Member (Embed IO) r => Sem (Di D.Level D.Path D.Message ': r) a -> Sem r a
runDiToStderrIO m = do
  commit <- embed $ DH.stderr Df1.df1
  runDiToIO commit m

attr :: (D.ToValue value, Member (Di level D.Path msg) r) => D.Key -> value -> Sem r a -> Sem r a
attr k v = attr_ k (D.value v)

debug :: (D.ToMessage msg, Member (Di D.Level path D.Message) r) => msg -> Sem r ()
debug = log D.Debug . D.message

info :: (D.ToMessage msg, Member (Di D.Level path D.Message) r) => msg -> Sem r ()
info = log D.Info . D.message

notice :: (D.ToMessage msg, Member (Di D.Level path D.Message) r) => msg -> Sem r ()
notice = log D.Notice . D.message

warning :: (D.ToMessage msg, Member (Di D.Level path D.Message) r) => msg -> Sem r ()
warning = log D.Warning . D.message

error :: (D.ToMessage msg, Member (Di D.Level path D.Message) r) => msg -> Sem r ()
error = log D.Error . D.message

alert :: (D.ToMessage msg, Member (Di D.Level path D.Message) r) => msg -> Sem r ()
alert = log D.Alert . D.message

critical :: (D.ToMessage msg, Member (Di D.Level path D.Message) r) => msg -> Sem r ()
critical = log D.Critical . D.message

emergency :: (D.ToMessage msg, Member (Di D.Level path D.Message) r) => msg -> Sem r ()
emergency = log D.Emergency . D.message

debug_ :: Member (Di D.Level path D.Message) r => D.Message -> Sem r ()
debug_ = log D.Debug

info_ :: Member (Di D.Level path D.Message) r => D.Message -> Sem r ()
info_ = log D.Info

notice_ :: Member (Di D.Level path D.Message) r => D.Message -> Sem r ()
notice_ = log D.Notice

warning_ :: Member (Di D.Level path D.Message) r => D.Message -> Sem r ()
warning_ = log D.Warning

error_ :: Member (Di D.Level path D.Message) r => D.Message -> Sem r ()
error_ = log D.Error

alert_ :: Member (Di D.Level path D.Message) r => D.Message -> Sem r ()
alert_ = log D.Alert

critical_ :: Member (Di D.Level path D.Message) r => D.Message -> Sem r ()
critical_ = log D.Critical

emergency_ :: Member (Di D.Level path D.Message) r => D.Message -> Sem r ()
emergency_ = log D.Emergency
