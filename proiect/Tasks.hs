{-
	PP Project 2021

	This is where you will write the implementation for the given tasks.
	You can add other modules aswell.
-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Tasks where

import Dataset
import Text.Printf
import Data.List
import Data.Array

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]

data Query =
    FromCSV CSV
    | ToCSV Query
    | AsList String Query
    | Sort String Query
    | ValueMap (Value -> Value) Query
    | RowMap (Row -> Row) [String] Query
    | VUnion Query Query
    | HUnion Query Query
    | TableJoin String Query Query
    | Cartesian (Row -> Row -> Row) [String] Query Query
    | Projection [String] Query
    | forall a. FEval a => Filter (FilterCondition a) Query
    | Graph EdgeOp Query
 
-- where EdgeOp is defined:
type EdgeOp = Row -> Row -> Maybe Value

data QResult = CSV CSV | Table Table | List [String]

instance Show QResult where
	show (CSV csv) = show csv
	show (Table table) = write_csv table
	show (List row) = show row

class Eval a where
	eval :: a -> QResult

data FilterCondition a =
    Eq String a |
    Lt String a |
    Gt String a |
    In String [a] |
    FNot (FilterCondition a) |
    FieldEq String String

type FilterOp = Row -> Bool

class FEval a where
	feval :: [String] -> (FilterCondition a) -> FilterOp

{-
	TASK SET 1
-}

-- Task 1
-- Transforma din string in float
strToFloat :: String -> Float
strToFloat "-1.00" = 0
strToFloat [] = 0
strToFloat str = read str::Float

--aduna notele unui elev si calculeaza nota finala
compute_grade :: Float -> Row -> Float
compute_grade final_grade (name:exam:[]) = (final_grade/4) + strToFloat exam
compute_grade final_grade (name:grade:row) = compute_grade (final_grade + strToFloat grade) (name:row)

compute_exam_grades :: Table -> Table
compute_exam_grades (columns:rows) = ["Nume", "Punctaj Exam"] : (map op rows) where
	op (name:grades) = name : [printf "%.2f" (compute_grade 0 (name:grades))]

-- Task 2
-- Number of students who have passed the exam:
-- calculeaza media fiecarui student si ii numara pe cei care au promovat
get_passed_students_num :: Table -> Int
get_passed_students_num (columns:rows) = foldr op 0 rows where
	op student passed
		| (compute_grade 0 student >= 2.5) = passed + 1
		| otherwise = passed

-- Percentage of students who have passed the exam:
intToFloat :: Int -> Float
intToFloat n = fromIntegral n::Float

get_passed_students_percentage :: Table -> Float
get_passed_students_percentage grades = (intToFloat (get_passed_students_num grades)) / (intToFloat (length grades) - 1)

-- Average exam grade
-- aduna mediile fiecarui student, iar suma este impartina la nr de studenti
get_exam_avg :: Table -> Float
get_exam_avg (columns:rows) = (foldr (\student sum -> sum + compute_grade 0 student) 0 rows) / (intToFloat (length rows))

-- Number of students who gained at least 1.5p from homework:
-- calculeaza punctajul pe teme al fiecarui student, iar daca aceste este > 1.5, total elevilor creste
get_passed_hw_num :: Table -> Int
get_passed_hw_num (columns:rows) = foldr op 0 rows where
	op (name:lab:t1:t2:t3:_) total
		| (strToFloat t1 + strToFloat t2 + strToFloat t3 >= 1.5) = total + 1
		| otherwise = total

-- Task 3
-- parcurge fiecare nota a fiecarui student si o aduna in coloana corespunzatoare ei
get_avg_responses_per_qs :: Table -> Table
get_avg_responses_per_qs (columns:rows) = ["Q1", "Q2", "Q3", "Q4", "Q5", "Q6"] :
										  [get_average (foldr op [0,0,0,0,0,0] rows) (intToFloat (length rows))] where
	op (name:exam:[]) qs = qs
	op (name:grade:grades) (q:qs) = (q + strToFloat grade) : op (name:grades) (qs)
	get_average [] _ = []
	get_average (q:qs) total = (printf "%.2f" (q / total)) : get_average qs total

-- Task 4
-- foldr primeste 6 liste si parcurge notele fiecarui student si adauga in lista corespunzatoare, nota studentului
-- functia transform duce listele de integer in liste de string si calculeaza numarul intrebarii la care este
get_exam_summary :: Table -> Table
get_exam_summary (columns:rows) = ["Q", "0", "1", "2"] : 
								  (transform 1 (foldr op [[0,0,0], [0,0,0], [0,0,0], [0,0,0], [0,0,0], [0,0,0]] rows)) where
	op (name:exam:[]) _ = []
	op (name:grade:grades) ((grade0:grade1:grade2:[]):qs) 
		| (grade == "0" || grade == "") = [grade0 + 1, grade1, grade2] : op (name:grades) qs
		| (grade == "1") = [grade0, grade1 + 1, grade2] : op (name:grades) qs
		| (grade == "2") = [grade0, grade1, grade2 + 1] : op (name:grades) qs
	transform _ [] = []
	transform q_nr (q:qs) = (("Q" ++ show q_nr) : map show q) : (transform (q_nr+1) qs)

-- Task 5
-- primeste 2 studenti si ii compara in functie de nota respectiv nume
compare_students :: Row ->  Row -> Ordering
compare_students (nameA:gradeA:[]) (nameB:gradeB:[])
	| (strToFloat gradeA) > (strToFloat gradeB) = GT
	| (gradeA == gradeB) = compare nameA nameB
	| otherwise = LT

get_ranking :: Table -> Table
get_ranking (columns:rows) = ["Nume", "Punctaj Exam"] : sortBy compare_students (map op rows) where
	op (name:grades) = name : [printf "%.2f" (compute_grade 0 (name:grades))]

-- Task 6
-- primeste doi studenti si ii compara in functie de nota de la proba orala, respectiv nume
compare_dif :: Row -> Row -> Ordering
compare_dif (nameA:interviewA:finalA:difA:[]) (nameB:interviewB:finalB:difB:[]) 
	| difB == difA = compare nameA nameB
	| otherwise = compare difA difB


get_exam_diff_table :: Table -> Table
get_exam_diff_table (columns:rows) = ["Nume", "Punctaj interviu", "Punctaj scris", "Diferenta"] :
									sortBy compare_dif (map (op 0) rows) where
	op interview_grade (name:final:[]) = name : [printf "%.2f" (interview_grade/4), printf "%.2f" (strToFloat final), 
												printf "%.2f" (abs (interview_grade/4 - strToFloat final))]
	op interview_grade (name:gr:row) = op (interview_grade + strToFloat gr) (name:row)


-- Partea 2
-- Separa un String dupa un anumit delimitator(functie din laboratorul 4)
splitBy :: Char -> String -> [String]
splitBy delim phrase = foldr op [] phrase where
	op c []
		| c == delim = [[], []]
		| otherwise = [[c]]
	op c (y:ys)
		| c == delim = [] : (y:ys)
		| otherwise = (c : y) : ys

-- Citeste un fisier csv
read_csv :: CSV -> Table
read_csv file = map (splitBy ',') (lines file)

-- Transforma un tabel intr-un String de tipul csv
write_csv :: Table -> CSV
write_csv table = foldr op [] table where
	op (col:[]) [] = col
	op (col:[]) acc = col ++ "\n" ++ acc
	op (col:row) acc = col ++ "," ++ op row acc

--Task 2.1
-- obtine indexul coloanei cu numele primit
get_column_index :: String -> Row -> Int
get_column_index name [] = 0
get_column_index name (column : columns)
	| name == column = 0
	| otherwise = 1 + get_column_index name columns

as_list :: String -> Table -> [String]
as_list [] table = []
as_list column table = foldr (op (get_column_index column (head table))) [] (tail table) where
	op index row acc 
		| index == 0 = (head row) : acc
		| otherwise = op (index-1) (tail row) acc


--Task 2.2
-- compara doua randuri in functie de parametrul dintr-o anumita coloana, respectiv dupa prima coloana
compare_column :: Int -> Row -> Row -> Ordering
compare_column index (first1:el1:col1) (first2:el2:col2)
	| index == 1 && el1 == el2 = compare first1 first2
	| index == 1 = compare el1 el2
	| index == 0 = compare first1 first2
	| otherwise = compare_column (index-1) (first1:col1) (first2:col2)


tsort :: String -> Table -> Table
tsort column table = (head table) : sortBy (compare_column (get_column_index column (head table))) (tail table)

--Task 2.3
vmap :: (Value -> Value) -> Table -> Table
vmap f table = (head table) : map (map f) (tail table)

--Task 2.4
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f columns table = columns : map f (tail table)

get_hw_grade_total :: Row -> Row
get_hw_grade_total (name:lab:grades) = name : sum 0 grades where
	sum total [] = [printf "%.2f" total]
	sum total (grade:rest) = sum (total + strToFloat grade) rest

--Task 2.5
vunion :: Table -> Table -> Table
vunion (columns1:table1) (columns2:table2)
	| columns1 == columns2 = columns1 : (table1 ++ table2)
	| otherwise = columns1 : table1

--Task 2.6
-- intoarce o lista de String-uri goale, de dimensiunea coloanei primite ca parametru
get_extra_columns :: Row -> Row
get_extra_columns [] = []
get_extra_columns (col:row) = "" : get_extra_columns row

-- cat timp nu s-au terminat coloanele in niciun tabel, acestea sunt concatenate
-- dupa ce s-a terminat un tabel, se calculeaza cate coloane are si se concateneaza acel numar de coloane goale
hunion :: Table -> Table -> Table
hunion (last1:[]) (last2:[]) = (last1 ++ last2) : []
hunion (row1:table1) (last2:[]) = (row1 ++ last2) : hunion table1 [get_extra_columns last2]
hunion (last1:[]) (row2:table2) = (last1 ++ row2) : hunion [get_extra_columns last1] table2
hunion (row1:table1) (row2:table2) = (row1 ++ row2) : hunion table1 table2

--Task 2.7
{- intoarce o lista cu perechi de numere intregi care reprezinta indexul 
coloanelor care au acelasi nume in doua tabele. -}
get_common_columns :: String -> Int -> Row -> Row -> [[Int]]
get_common_columns key index1 [] _ = []
get_common_columns key index1 (val1:row1) row2
	| key == val1 = get_common_columns key (index1 + 1) row1 row2
	| length row2 == get_column_index val1 row2 = get_common_columns key (index1 + 1) row1 row2
	| otherwise = (index1 : [get_column_index val1 row2]) : get_common_columns key (0) row1 row2

-- verifica daca doua randuri au aceeasi valoare pe o anumita coloana
cmp_key_values :: Int -> Int -> Row -> Row -> Bool
cmp_key_values key1 key2 row1 row2
	| key1 == 0 && key2 == 0 = (head row1) == (head row2)
	| key1 == 0 = cmp_key_values key1 (key2-1) row1 (tail row2)
	| key2 == 0 = cmp_key_values (key1-1) key2 (tail row1) row2
	| otherwise = cmp_key_values (key1-1) (key2-1) (tail row1) (tail row2)

{- cauta intr-un tabel, randul care are acceasi valoare pe o anumita coloana
cu valoarea unui rand de pe o coloana, indexul fiind primit ca parametru -}
find_same_key :: Int -> Int -> Row -> Table -> Row
find_same_key index1 index2 row1 (row2:table2)
	| cmp_key_values index1 index2 row1 row2 == True = row2
	| table2 == [] = get_extra_columns row2
	| otherwise = find_same_key index1 index2 row1 table2 

-- sterge elementul de pe pozitia index dintr-o lista
delete_key :: Int -> Row -> Row
delete_key index row
	| index == 0 = tail row
	| otherwise = (head row) : delete_key (index-1) (tail row)

{- sterge dintr-un rand elementele de pe coloanele care au acelasi nume 
cu coloanele din primul tabel-}

delete_common_col :: Int -> [[Int]] -> Row -> Row
delete_common_col index _ [] = []
delete_common_col index common_col (val:row) 
	| foldr (\col acc -> ((last col) == index) || acc) False common_col == True = delete_common_col (index+1) common_col row
	| otherwise = val : delete_common_col (index+1) common_col row

-- Uneste cele doua randuri, conform functiei tjoin
get_new_row :: Int -> [[Int]] -> Row -> Row -> Row -> Row
get_new_row key1 [] row1 row2 final_row2 = row1 ++ final_row2
get_new_row key1 (col:common_col) (val1:row1) row2 final_row2
	| row1 == [] = final_row2
	| key1 == 0 = val1 : get_new_row (-1) (((head col -1):(tail col)):common_col) row1 row2 final_row2
	| (head col) == 0 && ((row2 !! (last col)) /= "") = row2 !! (last col) : get_new_row (key1-1) common_col row1 row2 final_row2
	| (head col) == 0 = val1 : get_new_row (key1-1) common_col row1 row2 final_row2
	| otherwise = get_new_row (key1-1) (((head col - 1):(tail col)):common_col) row1 row2 final_row2 

-- uneste headerele tabelelor, astfel incat o coloana sa fie unica
get_new_col :: String -> Row -> Row -> Row
get_new_col key col1 col2 = col1 ++ delete_common_col 0 ((1:[get_column_index key col2]):get_common_columns key 0 col1 col2) col2

tjoin :: String -> Table -> Table -> Table
tjoin key t1 t2 = get_new_col key (head t1) (head t2) : 
	map (\row -> get_new_row (get_column_index key (head t1)) 
		(get_common_columns key 0 (head t1) (head t2)) row 
		(find_same_key (get_column_index key (head t1)) (get_column_index key (head t2)) 
			row (tail t2)) (delete_common_col 0 (get_common_columns key 0 (head t1) 
				(head t2)) (delete_key (get_column_index key (head t2)) 
				(find_same_key (get_column_index key (head t1)) (get_column_index key 
					(head t2)) row (tail t2))))) (tail t1)

--Task 2.8
cartesian :: (Row -> Row -> Row) -> [String] -> Table -> Table -> Table
cartesian f columns t1 t2 = columns : foldr (op t2) [] (tail t1) where 
	op t2 row1 acc = foldr (\row2 acc -> f row1 row2 : acc) acc (tail t2)


--Task 2.9
-- intoarce indexul coloanelor care trebuie sa fie extrase
get_columns_indexes :: [String] -> Row -> [Int]
get_columns_indexes [] _ = []
get_columns_indexes (x:new_columns) all_columns = (get_column_index x all_columns) : get_columns_indexes new_columns all_columns

projection :: [String] -> Table -> Table
projection columns table = columns : map (op (get_columns_indexes columns (head table))) (tail table) where
	op [] _ = []
	op (index:indexes) (col:row)
		| index == 0 = col : op (map (abs) (map (1-) indexes)) row
		| otherwise = op (map (abs) (map(1-) (index:indexes))) row




--Task set 3
justToString :: (Maybe Value) -> Value
justToString (Just val) = val

evalGraph :: EdgeOp -> Table -> Table
evalGraph f [] = []
evalGraph f t = op f (head t) (tail t) ++ evalGraph f (tail t) where
	op f row t
		| t == [] = []
		| f row (head t) /= Nothing && (head (head t) > head row) = [head row, head (head t), justToString (f row (head t))] : op f row (tail t)
		| f row (head t) /= Nothing = [head (head t), head row, justToString (f row (head t))] : op f row (tail t)
		| otherwise = op f row (tail t)

-- Task 3.1
get_table :: QResult -> Table
get_table (Table table) = table

get_row :: QResult -> [String]
get_row (List row) = row

instance Eval Query where
	eval (FromCSV str) = Table (read_csv str)
	eval (ToCSV query) = CSV (write_csv (get_table (eval query)))
	eval (AsList colname query) = List (as_list colname (get_table (eval query)))
	eval (Sort colname query) = Table (tsort colname (get_table (eval query)))
	eval (ValueMap op query) = Table (vmap op (get_table (eval query)))
	eval (RowMap op colnames query) = Table (rmap op colnames (get_table (eval query)))
	eval (VUnion query1 query2) = Table (vunion (get_table (eval query1)) (get_table (eval query2)))
	eval (HUnion query1 query2) = Table (hunion (get_table (eval query1)) (get_table (eval query2)))
	eval (TableJoin colname query1 query2) = Table (tjoin colname (get_table (eval query1)) (get_table (eval query2)))
	eval (Cartesian op colnames query1 query2) = Table (cartesian op colnames (get_table (eval query1)) (get_table (eval query2)))
	eval (Projection colnames query) = Table (projection colnames (get_table (eval query)))
	eval (Filter expr query) = Table (head (get_table (eval query)) : (filter (feval (head (get_table (eval query))) expr) (tail (get_table (eval query)))))
	eval (Graph f query) = Table (["From", "To", "Value"] : evalGraph f (tail (get_table (eval query))))

--Task 3.2

instance FEval Float where
	feval columns (Eq colname val) = \row -> val == (strToFloat (row !! (get_column_index colname columns)))
	feval columns (Lt colname val) = \row -> val > (strToFloat (row !! (get_column_index colname columns)))
	feval columns (Gt colname val) = \row -> val < (strToFloat (row !! (get_column_index colname columns)))
	feval cols (In coln vals) = \row -> foldr (\val acc -> (val == strToFloat (row !! (get_column_index coln cols))) || acc) False vals
	feval columns (FNot expr) = \row -> (not ((feval columns expr) row))
	feval cols (FieldEq coln1 coln2) = \row -> strToFloat (row !! (get_column_index coln1 cols)) == strToFloat (row !! (get_column_index coln2 cols))

instance FEval String where
	feval columns (Eq colname val) = \row -> val == (row !! (get_column_index colname columns))
	feval columns (Lt colname val) = \row -> val > (row !! (get_column_index colname columns))
	feval columns (Gt colname val) = \row -> val < (row !! (get_column_index colname columns))
	feval cols (In coln vals) = \row -> foldr (\val acc -> (val == (row !! (get_column_index coln cols))) || acc) False vals
	feval columns (FNot expr) = \row -> (not ((feval columns expr) row))
	feval cols (FieldEq coln1 coln2) = \row -> (row !! (get_column_index coln1 cols)) == (row !! (get_column_index coln2 cols))

-- Task 3.6
{-	listele cu notele celor 2 elevi se transforma intr-o singura lista ce contine
intrebarile la care au raspuns la fel. Se face suma si se pastreaza rezultatul daca
este mai mare de 4.-}
get_similarities :: Row -> Row -> Maybe Value
get_similarities r1 r2 
	| (head r1) == "" || (head r2) == "" = Nothing
	| sum (zipWith (\v1 v2 -> if (v1 == v2) then 1 else 0) (tail r1) (tail r2)) > 4 = 
					Just (show (sum (zipWith (\v1 v2 -> if (v1 == v2) then 1 else 0) (tail r1) (tail r2))))
	| otherwise = Nothing

-- compara doi studenti, si intoarce Ordering in functie de cel cu mai multe similaritati
cmp_similarities :: Row -> Row -> Ordering
cmp_similarities r1 r2
	| last r1 == "Value" = LT
	| last r2 == "Value" = LT
	| compare (strToFloat (last r1)) (strToFloat (last r2)) == EQ = compare (head r1) (head r2)
	| otherwise = compare (strToFloat (last r1)) (strToFloat (last r2))

-- Sorteaza elevii dupa similaritati
sort_similarities_query :: Query -> Query
sort_similarities_query graph = FromCSV (write_csv (sortBy cmp_similarities (get_table (eval graph))))

similarities_query :: Query
similarities_query = sort_similarities_query (Graph get_similarities (FromCSV lecture_grades_csv))



-- TASK 4.1
-- calculeaza distanta dintre 2 stringuri folosind lazy evaluation
lev :: String -> String -> Int
lev a b = dist (length a) (length b) where
        dist i 0 = i
        dist 0 j = j
        dist i j
          | a !! (i - 1) ==  b !! (j - 1) = dp ! (i - 1, j - 1)
          | otherwise = minimum [ dp ! (i - 1, j) + 1, dp ! (i, j - 1) + 1, dp ! (i - 1, j - 1) + 1]

        dp = listArray bounds [dist i j | (i, j) <- range bounds]
        bounds = ((0, 0), ((length a), (length b)))

{- primeste doua coloane si intoarce o coloana care contine valorile care se
afla doar in prima coloana -}
select_typos :: [String] -> [String] -> [String]
select_typos c1 c2 = foldr (op c2) [] c1 where
	op c2 val1 acc
		| c2 == [] = val1 : acc
		| (head c2) == val1 = acc
		| otherwise = op (tail c2) val1 acc

{- primeste o coloana ce contine valori corecte si una cu valori gresite si
intoarce doua coloane care contin valoarea gresita, respectiv cea corecta -}

find_match :: [String] -> [String] -> Table
find_match c1 c2 = map (op c2 [] 10) c1 where
	op c2 match min val1
		| c2 == [] = val1 : [match]
		| min == 1 = val1 : [match]
		| lev (head c2) val1 < min = op (tail c2) (head c2) (lev (head c2) val1) val1
		| otherwise = op (tail c2) match min val1

-- inlocuieste intr-un rand, numele gresit cu numele curect
replace_entry :: Int -> Row -> Value -> Row
replace_entry pos old val
	| old == [] = []
	| pos == 0 = val : replace_entry (pos-1) (tail old) val
	| otherwise = (head old) : replace_entry (pos-1) (tail old) val

-- inlocuieste intr-un tabel numele gresite cu numele corespunzatoare
replace :: Int -> Table -> Table -> CSV
replace col t new_val = write_csv (map (op col new_val) t) where
	op index new_val old_val
		| new_val == [] = old_val
		| (old_val !! index) == (head (head new_val)) = replace_entry index old_val (last (head new_val))
		| otherwise = op index (tail new_val) old_val

-- apeleaza functiile pentru a obtine rezultatul
aux :: String -> Table -> Table -> CSV
aux col t1 t2 = replace (get_column_index col (head t1)) t1 (find_match 
						(select_typos (as_list col t1) (as_list col t2)) 
						(select_typos (as_list col t2) (as_list col t1)))

correct_table :: String -> CSV -> CSV -> CSV
correct_table col t1 t2 = aux col (read_csv t1) (read_csv t2)

-- 4.2
-- cauta un student in tabel dupa nume si intoarce nota sa de pe parcurs
-- intoarce -1 daca studentul nu este in tabel
compute_hw_grade :: String -> Table -> Float
compute_hw_grade name [] = -1
compute_hw_grade name hw
	| name == head (head hw) = sum (head hw)
	| otherwise = compute_hw_grade name (tail hw)where
		sum (name:[]) = 0
		sum (name:grade:rest) = strToFloat grade + sum (name:rest)

-- cauta un student in tabel dupa email si intoarce nota sa de la curs
-- intoarce -1 daca studentul nu este in tabel
compute_lecture_grade :: String -> Table -> Float
compute_lecture_grade email [] = -1
compute_lecture_grade email email_table
	| email == head (head email_table) = sum 0.0 0.0 (head email_table)
	| otherwise = compute_lecture_grade email (tail email_table) where
		sum len total (email:[]) = 2 * total / len
		sum len total (email:grade:rest) = sum (len+1) (total + strToFloat grade) (email:rest)

-- cauta un student in tabel dupa nume si intoarce nota sa de la examen
-- intoarce -1 daca studentul nu este in tabel
compute_exam_grade :: String -> Table -> Float
compute_exam_grade name [] = -1
compute_exam_grade name exam_table
	| name == head (head exam_table) = compute_grade 0 (head exam_table)
	| otherwise = compute_exam_grade name (tail exam_table)

-- calculeaza nota finala a unui elev
{- P.S.: strToFloat intoarce 0 pentru "-1.00", adica pentru studentii care nu 
au fost gasiti in anumite tabele, pentru a putea fi calculata usor nota finala-}
compute_total :: Row -> Float
compute_total (name:hw:lecture:exam:[])
	| strToFloat hw + strToFloat lecture < 2.5 || strToFloat exam < 2.5 = 4.00
	| otherwise = (min (strToFloat hw + strToFloat lecture) 5) + strToFloat exam

grades_union :: Table -> Table -> Table -> Table -> Table
grades_union email hw exam lecture = ["Nume", "Punctaj Teme", "Punctaj Curs", "Punctaj Exam", "Punctaj Total"] :
			map (\x -> (op x) ++ [printf "%.2f" (compute_total (op x))]) (tail email) where
				op (name:email:[]) = [name, printf "%.2f" (compute_hw_grade name hw), printf "%.2f" (compute_lecture_grade email lecture), printf "%.2f" (compute_exam_grade name exam)]

{- am pus -1 la studentii care nu au fost gasiti in tabele, pentru a-i putea identifica,
iar acum le modific nota la "" pentru ca afisarea sa fie corecta-}
grades_transform :: Table -> Table
grades_transform final_grades = (head final_grades) : map op (tail final_grades) where
	op (x:grades)
		| grades == [] = [x]
		| x == "-1.00" = "" : op grades
		| otherwise = x : op grades

grades :: CSV -> CSV -> CSV -> CSV -> CSV
grades email hw exam lecture = write_csv (grades_transform (tsort "Nume" 
										(grades_union (read_csv (correct_table "Nume" email hw)) 
										(read_csv hw) (read_csv exam) (read_csv lecture))))