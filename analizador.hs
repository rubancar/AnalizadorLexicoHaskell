import System.IO
main = do
	handle <- openFile "prueba.c" ReadMode
	contents <- hGetContents handle
	--putStr contents
	putStrLn $ show contents
--putStrLn $ head (splitOn contents '\n')
	hClose handle

--imprimirLineas lista = do
