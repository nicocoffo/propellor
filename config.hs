import Propellor
import qualified Propellor.Property.File as File
import qualified Propellor.Property.MacOS.Brew as Brew

main :: IO ()
main = defaultMain [macbook]

macbook :: Host
macbook = host "localhost" $ props
	& Brew.installed (Brew.Formula "doesntexist")
	& File.dirExists "/Users/ncoughlin/testa"
