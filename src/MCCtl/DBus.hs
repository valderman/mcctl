-- | DBus helpers for MCCtl.
module MCCtl.DBus (dbusCall, dbusCallWith, Client, connectSystem, disconnect) where
import DBus
import DBus.Client
import MCCtl.Config

-- | Create a method call for MCCtl.
dbusMeth :: MemberName -> [Variant] -> MethodCall
dbusMeth member args = (methodCall dbusObj dbusIface member) {
    methodCallDestination = Just dbusBus,
    methodCallBody = args
  }

-- | Connect to the system bus, make a DBus call to the MCCtl server, then
--   disconnect from the bus.
--
--   Use only for oneshot invocations; for anything else we should reuse the
--   connection.
dbusCall :: MemberName -> [Variant] -> IO MethodReturn
dbusCall memb args = do
  client <- connectSystem
  res <- call_ client $ dbusMeth memb args
  disconnect client
  return res

-- | Connect to the system bus, make a DBus call to the MCCtl server, then
--   disconnect from the bus.
--
--   Use only for oneshot invocations; for anything else we should reuse the
--   connection.
dbusCallWith :: Client -> MemberName -> [Variant] -> IO MethodReturn
dbusCallWith client memb args = do
  res <- call_ client $ dbusMeth memb args
  return res
