# menu.haskell
main = do
    opciones True

opciones n = do

    if n
        then do
       
        putStrLn("---                  Menú               --- ")
       
        putStrLn "1.- Fibonacci"
        putStrLn "2.- Imprimir Números 1-10"
        putStrLn "3.- Factorial"
        putStrLn "4.- Desaparecer números"
        putStrLn "5.- Palíndromo"
        putStrLn "6.- Calculadora "
        putStrLn "7.- Salir"
        putStrLn "Dame una opción: "
        opc <- getLine
        putStrLn ("__________________________________________")
        case opc of
            "1" -> fibonacci
            "2" -> imprimirNumeros
            "3" -> factorial
            "4" -> desapareceNumeros
            "5" -> palindromos 
            "6" -> calculadora
            "7" -> opciones False
            _ -> opciones True
    else
        putStrLn "Fin del Programa"


fibonacci = do
        putStrLn("Introduce una posición para fibonacci: ")
        n <- getLine
        print("El numero es: "++show(fib(read n)))
        opciones True

fib n =
    if n == 0
        then
            0
    else if n == 1
        then
            1
    else
        fib(n-1) + fib(n-2)

imprimirNumeros = do
        imprimir 1
        opciones True

imprimir n = 
    if n <= 10
        then do
            print n
            imprimir(n+1)
        else do
            print ("Fin")

factorial = do
        putStrLn("Introduce el número a obtener factorial: ")
        f <- getLine
        print("El numero es: "++show(fact(read f)))
        opciones True

fact f =
    if f == 0
        then
            1
    else
       f * fact(f-1) 

desapareceNumeros = do

        lista [0,1,2,3,4,5,6,7,8,9,10]
        opciones True

lista x = 

    if null x
        then
        print ("Fin") 
    else do
        print(x)
        lista(init x)
        

palindromos  = do
        putStrLn("Ingresa la palabra a evaluar")
        p <- getLine
        comprobar(p)
comprobar p = do
        if p ==reverse p
            then do
                putStrLn("Es un palindromo") 
            else do 
                putStrLn("no es un palindromo")  
        opciones True                 
       

calculadora = do
        operacion True
        opciones True
        
operacion n = do  
    
    if n
        then do
        
        putStrLn "1.- Suma"
        putStrLn "2.- Resta"
        putStrLn "3.- Multiplicación"
        putStrLn "4.- División"
        putStrLn "5.- Salir"
        opc <- getLine
        putStrLn ("___________________________________")

        case opc of
            "1" -> suma
            "2" -> resta
            "3" -> multi
            "4" -> divi
            "5" -> operacion False
            _ -> operacion True
    else
        putStrLn "Fin"    

suma = do
       -- print
        
        putStrLn "Ingresa el número 1: "
        a <- readLn
        putStrLn "Ingresa el número 2: "
        b <- readLn
        putStrLn ("El resultado es: " ++ show(a + b))
        operacion True
        -- let uno = read a::Int
        -- let dos = read b::Int
resta = do
        putStrLn "Ingresa el número 1: "
        a <- readLn
        putStrLn "Ingresa el número 2: "
        b <- readLn
        putStrLn ("El resultado es: " ++ show(a - b))
        operacion True
        -- let uno = read a::Int
        -- let dos = read  b::Int

multi = do
        putStrLn "Ingresa el número 1: "
        a <- readLn
        putStrLn "Ingresa el número 2: "
        b <- readLn
        putStrLn ("El resultado es: " ++ show(a * b))
        operacion True

divi = do
        putStrLn "Ingresa el número 1: "
        a <- getLine
        putStrLn "Ingresa el número 2: "
        b <- getLine

        let aInt = read a::Int
        let bInt = read b::Int
        let r = div aInt bInt
        putStrLn ("El resultado es: " ++ show(r))
        operacion True
