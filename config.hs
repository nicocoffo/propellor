import Propellor
import qualified Propellor.Property.File as File

main :: IO ()
main = defaultMain [macbook]

macbook :: Host
macbook = host "localhost" $ props
	& File.dirExists "/Users/ncoughlin/testa"
