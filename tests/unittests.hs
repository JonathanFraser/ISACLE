import Prelude

import Test.Tasty

import qualified Tests.Core.Harvard.ISA
import qualified Tests.Core.Harvard.Pipeline
import qualified Tests.Core.GPIO
import qualified Tests.Core.Periph.Timer
import qualified Tests.Core.Periph.UART
import qualified Tests.Core.Periph.DMA

main :: IO ()
main = defaultMain $ testGroup "."
  [ Tests.Core.Harvard.ISA.isaTests
  , Tests.Core.Harvard.Pipeline.pipelineTests
  , Tests.Core.GPIO.gpioTests
  , Tests.Core.Periph.Timer.timerTests
  , Tests.Core.Periph.UART.uartTests
  , Tests.Core.Periph.DMA.dmaTests
  ]
