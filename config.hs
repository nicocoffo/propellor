import Propellor
import qualified Propellor.Property.File as File
import qualified Propellor.Property.MacOS.Brew as Brew

main :: IO ()
main = defaultMain [macbook]

macbook :: Host
macbook = host "localhost" $ props
	& Brew.cmdProperty "info" ["wget"]
	& File.dirExists "/Users/ncoughlin/testa"
