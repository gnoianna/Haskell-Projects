module Student where
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO.Unsafe

data Student = Student {firstName::String, lastName::String, age::Int} 
   deriving (Show, Read, Eq)

--LISTY WYKORZYSTYWANE W PROGRAMIE 
listToProcess = [Student "Alicja" "Akla" 21, Student "Batrek" "Bodo" 20, Student "Celina" "Czyzyk" 21, Student "Damian" "Dab"  22, Student "Eustachy" "Elo" 20]
modifiedList = [Student "AlicjaX" "Akla" 22, Student "BatrekX" "Bodo" 20, Student "Celina" "CzyzykX" 21, Student "DamianX" "Dab"  22, Student "Eustachy" "Elo" 20]

--ZADANIE 1 -- Utworzyć listę zawierającą pełne imiona i nazwiska studentów w postaci łańcuchów znaków.
getListToStringChain :: [Student] -> [String]
getListToStringChain list = map getInfoAboutStudent list

getInfoAboutStudent :: Student -> String
getInfoAboutStudent (Student a b c) = a++" "++b

--ZADANIE 2 -- Utworzyć listę zawierającą pary w postaci krotek: numer porządkowy, student.
getIndexedListWithTuples :: [Student] -> [(Int, Student)]
getIndexedListWithTuples list = zip [1..] list

--ZADANIE 3 -- Przetworzyć listę z powyższego punktu na raport tekstowy w formacie.
printStudents :: [Student] -> IO()
printStudents list = printElements (getFullString (getIndexedListWithTuples list))

printElements :: [String] -> IO()
printElements [] = return ()
printElements (x:xs) = do putStrLn x
                          printElements xs

getFullString :: [(Int, Student)] -> [String]
getFullString list = map getFullInfoAboutStudent list

getFullInfoAboutStudent :: (Int, Student) -> String
getFullInfoAboutStudent (index, Student a b c) = (show index)++". student: "++b++" "++(take 1 a)++". wiek: "++(show c)

--ZADANIE 4 -- Wygenerować tabelkę HTML.
getHTMLTable :: [Student] -> IO()
getHTMLTable list = do putStrLn "<table>"
                       putStrLn "<tr>"
                       putStrLn "<th> L.p. </th>"
                       putStrLn "<th> Imie </th>"
                       putStrLn "<th> Nazwisko </th>"
                       putStrLn "<th> Wiek </th>"
                       putStrLn "</tr>"
                       listToHTML list
                       putStrLn "</table>"

listToHTML :: [Student] -> IO()
listToHTML list = getHTMLFromTuples (getIndexedListWithTuples list)

getHTMLFromTuples :: [(Int, Student)] -> IO()
getHTMLFromTuples [] = return ()
getHTMLFromTuples (x:xs) = do getHTMLRow x
                              getHTMLFromTuples xs

getHTMLRow :: (Int, Student) -> IO()
getHTMLRow (index, Student a b c) = do putStrLn "<tr>"
                                       putStrLn ("<td>"++(show index)++"</td>")
                                       putStrLn ("<td>"++a++"</td>")
                                       putStrLn ("<td>"++b++"</td>")
                                       putStrLn ("<td>"++(show c)++"</td>")
                                       putStrLn "</tr>"

--ZADANIE 5 -- Wygenerować listę zmian w postaci typu wydarzenia, StudentsFirstNameChangeEvent oldName newName,
--przez utworzenie zmodyfikowanej listy studentów, a następnie porównanie (pod tymi samymi pozycjami powinni się znajdować ci sami studenci)

--StudentsFirstNameChangeEvent new: ______, old: ______"
--StudentsListNameChangeEvent new: ______, old: ______"
--StudentsAgeChangeEvent new: ______, old: ______"

createEventList :: [Student] -> [Student] -> [String]
createEventList [] [] = []
createEventList (x:xs) (y:ys) = (compareStudent x y) ++ (createEventList xs ys)

compareStudent :: Student -> Student -> [String]
compareStudent (Student a b c) (Student a2 b2 c2) = (compareFirstName a a2) ++ (compareLastName b b2) ++ (compareAge c c2)

compareFirstName :: String -> String -> [String]
compareFirstName name name2 = if name == name2
                                then []
                                else ["StudentsFirstNameChangeEvent new: "++name++" , old: "++name2]

compareLastName :: String -> String -> [String]
compareLastName lastname lastname2 = if lastname == lastname2
                                then []
                                else ["StudentsLastNameChangeEvent new: "++lastname++" , old: "++lastname2]

compareAge :: Int -> Int -> [String]
compareAge age age2 = if age == age2
                                then []
                                else ["StudentsAgeChangeEvent new: "++(show age)++" , old: "++(show age2)]

--ZADANIE 6 -- Dodać możliwość odczytu danych z pliku (punkt 1) i zapisu do pliku (punkty 3, 4).
-- punkt 1 -- 
readFromFileAndPrint:: String -> IO()
readFromFileAndPrint filepath = do
    fileContent <- readFile filepath
    let linesOfFile = lines fileContent
    let listOfStudents = handleString linesOfFile
    printStudents listOfStudents

readFromFileAndReturnIO:: String -> IO [Student]
readFromFileAndReturnIO filepath = do
    fileContent <- readFile filepath
    let linesOfFile = lines fileContent
    let listOfStudents = handleString linesOfFile
    return( listOfStudents)

readFromFileAndUnsafeConvert:: String -> [Student]
readFromFileAndUnsafeConvert filepath = (unsafePerformIO (readFromFileAndReturnIO filepath))

handleString:: [String]->[Student]
handleString [] = []
handleString (x:xs) = (createStudent (firstLast ((convertStringToWords x)!!1)) (firstLast ((convertStringToWords x)!!2)) (read ((convertStringToWords x)!!3))) ++(handleString xs)

convertStringToWords:: String -> [String]
convertStringToWords x = words x

firstLast:: String -> String
firstLast (x:xs) = init xs

createStudent :: String -> String -> Int -> [Student]
createStudent name lname age = [(Student name lname age)]

-- punkt 3 --
printStudentsToFile :: [Student] -> IO()
printStudentsToFile list = do   writeFile "Students.txt" (getStringWithAllStudents(getFullString (getIndexedListWithTuples list )))

getStringWithAllStudents :: [String] -> String
getStringWithAllStudents [] = []
getStringWithAllStudents (x:xs) =  (x ++ "\n") ++ (getStringWithAllStudents xs)

-- zapis punkt 4
getHTMLTableToFile :: [Student] -> IO()
getHTMLTableToFile list = (writeFile "tableOfStudents.html" (stringHTML list))

stringHTML :: [Student] -> String
stringHTML list = ("<html>\n"++
                        "<table>\n"++
                        "<tr>\n"++
                        "<th> Nr </th>\n"++
                        "<th> Imie </th>\n"++
                        "<th> Nazwisko </th>\n"++
                        "<th> Wiek </th>\n"++
                        "</tr>\n"++
                        (listToHTMLString list)++
                        "</table>\n"++
                        "</html>\n")


listToHTMLString :: [Student] -> String
listToHTMLString list = listOfTuplesToHTMLString (getIndexedListWithTuples list)

listOfTuplesToHTMLString:: [(Int, Student)] -> String
listOfTuplesToHTMLString [] = []
listOfTuplesToHTMLString (x:xs) = ((getHTMLRowString x) ++ (listOfTuplesToHTMLString xs))

getHTMLRowString :: (Int, Student) -> String
getHTMLRowString (index, Student a b c) = ("<tr>\n" ++
                                        ("<td> "++(show index)++". </td>\n")++
                                        ("<td> "++a++" </td>\n")++
                                        ("<td> "++b++" </td>\n")++
                                        ("<td> "++(show c)++" </td>\n")++
                                        "</tr>\n")
