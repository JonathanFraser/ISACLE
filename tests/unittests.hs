import Prelude

import Test.Tasty

import qualified Tests.Isacle.Harvard.ISA
import qualified Tests.Isacle.Harvard.Pipeline
import qualified Tests.Isacle.GPIO
import qualified Tests.Isacle.Periph.Timer
import qualified Tests.Isacle.Periph.UART
import qualified Tests.Isacle.Periph.DMA
import qualified Tests.Isacle.System.Bus

main :: IO ()
main = defaultMain $ testGroup "."
  [ Tests.Isacle.Harvard.ISA.isaTests
  , Tests.Isacle.Harvard.Pipeline.pipelineTests
  , Tests.Isacle.GPIO.gpioTests
  , Tests.Isacle.Periph.Timer.timerTests
  , Tests.Isacle.Periph.UART.uartTests
  , Tests.Isacle.Periph.DMA.dmaTests
  , Tests.Isacle.System.Bus.busTests
  ]
