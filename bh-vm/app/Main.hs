module Main where

import Lib
import Turtle.Prelude

-- Read system values and cmd.
--
--  cmd     = name + ip + tariff + template + arch (?)
--  tariff  = memory + size
--  system  = pool + bridge
--
--  Overwrite: cmd > tariff > system
--
--  map template --> cd iso path
--
-- virsh vol-create besthost133 gen-vol.xml
-- virsh vol-path vm-abc --pool besthost133  ==> volpath
-- virsh define gen-dom.xml
-- attach install cd.
--
-- Checks:
--
--  - volpath not used by any other vm. Use readlink. Path exists - ask.
--  - ip not used by any other vm. Warn about vms without explicit ip spec.
--  Fix plain text db: virsh --> db.
--
-- Assume: default net (instead of specifying bridge directly) and default
-- pool (name it default). Then i may omit system config altogether.

main :: IO ()
main = someFunc
