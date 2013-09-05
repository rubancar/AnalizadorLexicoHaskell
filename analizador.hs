import System.IO

{-
Definicion de un nuevo tipo de dato token, usando los tipos estandares
de datos en Haskell, esto se realiza para darle una mejor presentacion
al codigo
-}
data Token = EOF | 
             Identificador String | 
	     EspecificadorDeTipo String |
	     Instruccion String |
             ValorNumerico String | 
	     Cadena String |
             Operador Char | 
             Separador Char | 
	     Modificador String | 
             Error String 
             deriving (Show, Eq)

lexer :: String->[Token]

	
	
lexer xs =  let s =  eliminarComentarios xs
            in lexer' s


lexer' [] = [EOF]
-- for a non-empty string we read the first token
-- then we let lexer tokenize the rest of string
lexer' xs = let (t, rest)= nextToken xs
            in (t:lexer rest)

-- next token reads the input string and returns the next token and
-- rest of string
nextToken:: String->(Token, String)

nextToken [] = (EOF, [])
nextToken l@(x:xs) | esLetra x =  let (first, rest) = leerIdentificador l
                                      in (verificarIdentificador first, rest)
                  | esDigito x = let (first, rest) = finDeNumero l
                                       in (ValorNumerico first, rest)
		  | x == '"' = let (first, rest) = finDeCadenaTexto xs
					in (Cadena first, rest)
                  | esOperador x = (Operador x, xs)
                  | esSeparador x = (Separador x, xs)
                  | otherwise = (Error [x], xs)



leerIdentificador [] = ([], [])
leerIdentificador l@(x:xs) = let (first, rest) = leerLetrasODigitos xs
                          	 (first', rest') = guionesBajos rest
                          	 identifier = (x:first) ++ first'
                          	 in (identifier, rest')



leerLetrasODigitos :: String->(String, String)
leerLetrasODigitos xs = span esLetraODigito xs



guionBajo :: String->(String, String)
guionBajo [] = ([], [])
guionBajo [x] = ([], [x])
guionBajo l@(x:y:xs) = if esGuioBajo x && esLetraODigito y
                               -- lets read the rest of letters/digits of this
                               -- tail
                               then let (first,rest) = leerLetrasODigitos xs
                                    in (x:y:first, rest)
                               -- there is no guionBajo here
                               else ("", l)

guionesBajos :: String->(String, String)
-- reads 0 or more occurances of guionBajo
guionesBajos xs = let -- lets try to read the first guionBajo
                         (first, rest) = guionBajo xs in
                         if null first
                            -- if first tail is absent, then we just return
                            then (first, rest)
                            -- otherwise we read rest of the tail recursively
                            else let (first', rest') = guionesBajos rest
                                 -- we join the first tail and rest of the tail
                                 -- we return this along with unread string
                                 in (first ++ first' , rest')


verificarIdentificador :: String->Token
verificarIdentificador "void" = EspecificadorDeTipo "void"
verificarIdentificador "int" = EspecificadorDeTipo "int"
verificarIdentificador "char" = EspecificadorDeTipo "char"
verificarIdentificador "double" = EspecificadorDeTipo "double"
verificarIdentificador "enum" = EspecificadorDeTipo "enum"
verificarIdentificador "float" = EspecificadorDeTipo "float"
verificarIdentificador "long" = EspecificadorDeTipo "long"
verificarIdentificador "short" = EspecificadorDeTipo "short"
verificarIdentificador "signed" = EspecificadorDeTipo "signed"
verificarIdentificador "struct" = EspecificadorDeTipo "struct"
verificarIdentificador "extern" = EspecificadorDeTipo "extern"
verificarIdentificador "register" = EspecificadorDeTipo "register"
verificarIdentificador "volatile" = EspecificadorDeTipo "volatile"
verificarIdentificador "union" = EspecificadorDeTipo "union"
verificarIdentificador "break" = Instruccion "break"
verificarIdentificador "case" = Instruccion "case"
verificarIdentificador "continue" = Instruccion "continue"
verificarIdentificador "default" = Instruccion "default"
verificarIdentificador "do" = Instruccion "do"
verificarIdentificador "else" = Instruccion "else"
verificarIdentificador "for" = Instruccion "for"
verificarIdentificador "goto" = Instruccion "goto"
verificarIdentificador "if" = Instruccion "if"
verificarIdentificador "return" = Instruccion "return"
verificarIdentificador "switch" = Instruccion "switch"
verificarIdentificador "while" = Instruccion "while"
verificarIdentificador "sizeof" = Instruccion "sizeof"
verificarIdentificador "typedef" = Instruccion "typedef"
verificarIdentificador "auto" = Modificador "auto"
verificarIdentificador "static" = Modificador "static"
verificarIdentificador "unsigned" = Modificador "unsigned"
verificarIdentificador "const" = Modificador "const"
verificarIdentificador z = Identificador z


--verifica el fin de una cadena de texto, obviando los posibles  errores
--que probocan las sentencias de escape
finDeCadenaTexto [] = ([],[])
finDeCadenaTexto l@(x:y:xs) | y == '"' && x /= '\\' =  ([],xs)
finDeCadenaTexto l@(x:xs) = let (first, rest) = finDeCadenaTexto xs in (x:first, rest)

--lee un numero sea entero o flotante, lo devuelve junto con el resto del
--String para seguir analizando
finDeNumero [] = ([],[])
finDeNumero l@(x:y:xs) | esDigito x && not(esDigito y) && y /= '.' = (x:[],xs)
finDeNumero l@(x:y:xs) | esDigito x && y == '.' || x == '.' && esDigito y = let (first, rest) = finDeNumero (y:xs) in (x:first, rest)
finDeNumero l@(x:xs) = let (first, rest) = finDeNumero xs in (x:first, rest)

{-
Elimina los comentarios de una sola linea que en C empienza con //
y terminan con el caracter de salto de linea \n
-}
eliminarComentario :: String->String
eliminarComentario [] = []
eliminarComentario l 
		| length(l) < 3 = l
eliminarComentario l@(x:y:xs) =
			case (x == '/' && y == '/') of 
				True -> verificarFinDeLinea xs
				False -> l


verificarFinDeLinea [] = []
verificarFinDeLinea l@(x:xs) = case esFinDeLinea x of
                                 	True-> xs
                                 	False-> verificarFinDeLinea xs 
				
{-
Elimina los comentarios multilinea que en C empienza con el conjunto de 
caracteres /** y terminan con */, elimina las posibles interpretaciones
erroneas que puedan surgir en los comentarios cuando en ellos tengamos
palabras reservadas del lenguaje C
-}
eliminarComentarioMultilinea [] = []
eliminarComentarioMultilinea l
			| length(l) < 4 = l
eliminarComentarioMultilinea l@(x:y:z:xs) = case (x == '/' && y == '*' && z == '*') of
							True -> verificarFinMultilinea xs
							False -> l

verificarFinMultilinea [] = []
verificarFinMultilinea l 
		| length(l) < 2 = l
verificarFinMultilinea l@(x:y:xs) = case (x == '*' && y == '/') of
						True -> xs
						False -> verificarFinMultilinea (y:xs)

--descarta los saltos de linea, para evitar que sea interpretados 
--como un identificador
eliminarSaltosDeLinea [] = []
eliminarSaltosDeLinea l@(x:xs) = case x == '\n' of
				True -> eliminarSaltosDeLinea xs
				False -> l



-- salta lo que no significa nada para el analisis lexico
eliminarComentarios =  eliminarComentario.eliminarComentarioMultilinea.saltarEspacios.eliminarSaltosDeLinea

-- salta los espacios entre las palabras
saltarEspacios [] = []
saltarEspacios l@(x:xs) = case esEspacio x of
                                True -> saltarEspacios xs
                                False -> l


esEspacio ch = ch <= ' '

esMayuscula ch = 'A' <= ch && ch <= 'Z'

esMinuscula ch = 'a' <= ch && ch <= 'z'

esLetra ch = esMinuscula ch || esMayuscula ch

esDigito ch = '0' <= ch && ch <= '9'

esLetraODigito ch = esLetra ch || esDigito ch

esGuioBajo ch = ch == '_'

esOperador ch = ch `elem` "+-*/=><"

esSeparador ch = ch `elem` ";,(){}[]"

esFinDeLinea ch = ch == '\n'

	
{-
{-
Funciones que permiten mostrar por pantalla el contenido del archivo
analizado, en su formato real, incluidos sus saltos de linea
-}
splitOn :: [Char] -> Char -> [[Char]]
splitOn [] c = []
splitOn x c = let (a,b) = splitUntil x c in [a] ++ splitOn b c

splitUntil :: [Char] -> Char -> ([Char],[Char])
splitUntil [] c = ([],[])
splitUntil (x:xs) c
	| x==c = ([],xs)
	| otherwise = let (a,b) = splitUntil xs c in (x:a,b)
-}


{-
Da un formato a la salida por pantalla, ya que Token
no es un dato conocido para las funciones comunes de impresion
en Haskell
-}
printAnalisis :: [Token] -> IO()
printAnalisis [] = return ()
printAnalisis (x:xs) = do putStrLn $ show x
                          printAnalisis xs

main :: IO ()
main = do
    	putStr "Ingrese nombre de archivo a analizar: "
    	hFlush stdout
    	nombreArchivo <- getLine
	handle <- openFile nombreArchivo ReadMode
	contents <- hGetContents handle
	let tokens = lexer contents
	printAnalisis  tokens
	hClose handle
	
