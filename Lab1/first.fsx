printfn "| Вариант 7 - Епифанов Евгений - М8О-212Б-22 |"

let n_s = 10.  // кол-во итераций
let a = 0.0   // начало отрезка
let b = 0.5   // конец отрезка 
let e = 0.000000001
let step = (b - a) / n_s // шаг
let fnc x = (x * (3.0 - x)) / ((1.0 - x) * (1.0 - x) * (1.0 - x))  // Получаем значение для функции от x

printfn "============================================================================================================"
printfn "||    x\t      Значение Функции   Ряд Тэйлора(Туп)   Число итераций    Ряд Тэйлора(Умн)    Число итераций  ||"
printfn "||--------------------------------------------------------------------------------------------------------||"

// Наивный Тейлор
let rec iter f i n b x =
  if n<=b && (n * (n + 2.) * (x ** n))>e then
    f (iter f i (n + 1.) b x) (n * (n + 2.) * (x ** n))
  else 
    i
    
let rec ogr_iter f i n b x =
  if n<=b && (n * (n + 2.) * (x ** n))>e then
    f (ogr_iter f i (n + 1.) b x) 1.
  else 
    i

// Умный Тейлор
let rec iter2 f i n b x part =
  if n<=b && (part*x)>e then
    f (part*x) (iter2 f i (n + 1.) b x ((part * x * (n+1.) * (n+3.)) / (n+2.) / n) )
  else 
    i
    
let rec ogr_iter2 f i n b x part =
  if n<=b && (part*x)>e then
    f 1. (ogr_iter2 f i (n + 1.) b x ((part * x * (n+1.) * (n+3.)) / (n+2.) / n) )
  else 
    i

// Подсчет и вывод значений
let rec myfor3 a b f =
  if a<b then
    f a
    myfor3 (a + step) b f

myfor3 a b (fun i-> printfn "|| %f  %.15f  %.15f      %.1f           %.15f       %.1f        ||" i (fnc i) (iter (+) 0. 1. 100. i) (ogr_iter (+) 0. 1. 100. i) (iter2 (+) 0. 1. 100. i 3.) (ogr_iter2 (+) 0. 1. 100. i 3.))

