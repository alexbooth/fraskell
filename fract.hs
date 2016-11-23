import Codec.Picture
import Codec.Picture.Types
import System.Environment

main :: IO ()
main = do
    putStrLn "Beginning rendering of image."
    savePngImage "output.png" generateFractal
    putStrLn "Completed"

width :: Int
width = 1920

height :: Int
height = 1080

generateFractal :: DynamicImage
generateFractal = ImageRGB8 $ generateImage mandelbrot width height

iters :: Int
iters = 25

palette :: [PixelRGB8]
palette = foldr (\a -> \b -> (PixelRGB8 (fromIntegral a*10) (fromIntegral a*10) 0):b) [] [0..iters]

mandelbrot :: Int -> Int -> PixelRGB8
mandelbrot x0 y0 = getColor 0 0 0
    where getColor :: Float -> Float -> Int -> PixelRGB8
          getColor x y i = if x*x + y*y < 2*2 && i < iters 
                           then getColor (x*x - y*y + (scaleX x0)) (2*x*y + (scaleY y0)) (i+1)
                           else palette!!i

scaleX :: Int -> Float
scaleX x = (3.5 * (fromIntegral x)) / (fromIntegral width) - 2.5

scaleY :: Int -> Float
scaleY y = (2.0 * (fromIntegral y)) / (fromIntegral height) - 1.0