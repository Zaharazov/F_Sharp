open System

let fnc1 x = (3. * log(x) * log(x)) + 6. * log(x) - 5.
let przv1 x = ((6. * log(x)) / x) + (6. / x)
let a1 = 1.
let b1 = 3.
let fnc2 x = 0.6 * (3. ** x) - (2.3 * x) - 3.
let przv2 x = (log(3.) * 0.6 * (3. ** x)) - 2.3
let a2 = 2.
let b2 = 3.
let fnc3 x = (x * x) - log(1. + x) - 3.
let przv3 x = (2.*x) - (1./(x + 1.))
let a3 = 2.
let b3 = 3.
let e = 0.000001

// Ньютон 
let nut x f1 f2 = f1(x) / f2(x)

let rec iter_while f x a b =
  if (abs(x - (x - (f x a b))) < e)
    then x
  else 
    iter_while f (x - (f x a b)) a b

let x01 = b1
let o1 = iter_while nut x01 fnc1 przv1
let x02 = b2
let o2 = iter_while nut x02 fnc2 przv2
let x03 = b3
let o3 = iter_while nut x03 fnc3 przv3

// Дихотомия
let rec iter_while2 a b f =
  if (abs(a - b) < e)
    then (a + b) / 2.
  else 
    let c = (a + b) / 2.
    if (f(a) * f(c) < 0.) then
      iter_while2 a c f
    else
      iter_while2 c b f

let ot1 = iter_while2 a1 b1 fnc1
let ot2 = iter_while2 a2 b2 fnc2
let ot3 = iter_while2 a3 b3 fnc3

// Итерации
let rec iter_while3 b f =
  if (abs(f(b) - b) < e)
    then f(b)
  else 
    iter_while3 (f(b)) f

let dr_fnc1 x = 2.71828 ** ((5. - 3. * log(x)**2.) / 6.)
let dr_fnc2 x = Math.Log((23. * x + 30.) / 6., 3.)
let dr_fnc3 x = sqrt(log(1. + x) + 3.)

let otv1 = iter_while3 b1 dr_fnc1
let otv2 = iter_while3 b2 dr_fnc2
let otv3 = iter_while3 b3 dr_fnc3

// Таблица
printfn "| Вариант 7  -  Епифанов Евгений  -  М8О-212Б-22   |"
printfn "|///////////| equation 1 | equation 2 | equation 3 |"
printfn "|  Ньютон   |   %.4f   |   %.4f   |   %.4f   |" o1 o2 o3
printfn "| Дихотомия |   %.4f   |   %.4f   |   %.4f   |" ot1 ot2 ot3 
printfn "| Итерации  |   %.4f   |   %.4f   |   %.4f   |" otv1 otv2 otv3
