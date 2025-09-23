# Лабораторная работа №1

---

- Студент: `Тимошкин Роман Вячеславович`
- Группа: `P3331`

---

## Задание 1: Project Euler 14 - Последовательность Коллатца

### Описание проблемы

Задача заключается в нахождении числа меньше 1,000,000, для которого длина последовательности Коллатца максимальна. Последовательность Коллатца строится следующим образом:

- Если число четное, делим его на 2
- Если число нечетное, умножаем на 3 и прибавляем 1
- Последовательность продолжается до достижения числа 1

Пример: для числа 13 последовательность будет: 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1 (длина 10)

### Описание решения

Решение реализовано на OCaml с использованием шести различных подходов:

1. **Монолитная хвостовая рекурсия** - поиск максимума с помощью хвостовой рекурсии
2. **Монолитная обычная рекурсия** - поиск с обычной рекурсией (может переполнить стек)
3. **Модульный конвейер** - функция-генератор + фильтрация + свертка
4. **С использованием отображения** - преобразование списка чисел в их длины последовательностей
5. **С циклами** - императивный подход с мутабельными переменными
6. **С последовательностями (Seq)** - ленивые вычисления с бесконечной последовательностью

Все решения вычисляют длину последовательности Коллатца с помощью хвостовой рекурсии для эффективности.

### Код решения

```ocaml
let collatz_next n = if n land 1 = 0 then n / 2 else (3 * n) + 1

let collatz_length_tail n =
  let rec aux acc current =
    if current = 1 then acc + 1 else aux (acc + 1) (collatz_next current)
  in
  aux 0 n

let rec collatz_length_recursive n =
  if n = 1 then 1 else 1 + collatz_length_recursive (collatz_next n)

let solve_monolithic_tail limit =
  let rec search n best best_len =
    if n = limit then best
    else
      let len = collatz_length_tail n in
      if len > best_len then search (n + 1) n len
      else search (n + 1) best best_len
  in
  search 1 1 1

let solve_monolithic_recursive limit =
  let rec search n best best_len =
    if n = limit then best
    else
      let len = collatz_length_recursive n in
      if len > best_len then search (n + 1) n len
      else search (n + 1) best best_len
  in
  search 1 1 1

module CollatzGenerator = struct
  let generate bound = List.init (bound + 1) Fun.id
end

module CollatzFilter = struct
  let filter_numbers bound = List.filter (fun n -> n > 0 && n < bound)
end

module CollatzReducer = struct
  let reduce_numbers =
    List.fold_left (fun (best, best_len) n ->
        let len = collatz_length_tail n in
        if len > best_len then (n, len) else (best, best_len))
end

let solve_modular limit =
  CollatzGenerator.generate limit
  |> CollatzFilter.filter_numbers limit
  |> CollatzReducer.reduce_numbers (1, 1)
  |> fst

let solve_with_map limit =
  List.init (limit - 1) (fun i -> i + 1)
  |> List.map (fun n -> (n, collatz_length_tail n))
  |> List.fold_left
       (fun (best, best_len) (n, len) ->
         if len > best_len then (n, len) else (best, best_len))
       (1, 1)
  |> fst

let solve_with_loops limit =
  let best = ref 1 in
  let best_len = ref 1 in
  for n = 1 to limit - 1 do
    let len = collatz_length_tail n in
    if len > !best_len then (
      best := n;
      best_len := len)
  done;
  !best

let solve_with_seq limit =
  let open Seq in
  let naturals = unfold (fun state -> Some (state, state + 1)) 1 in
  let step (best, best_len) n =
    let len = collatz_length_tail n in
    if len > best_len then (n, len) else (best, best_len)
  in
  naturals |> take_while (fun n -> n < limit) |> fold_left step (1, 1) |> fst
```

```go
package lib

// Вычисление длины цепочки Коллатца
func chainLen(n uint64) uint {
	i := uint(1)
	for ; n > 1; i++ {
		if n%2 == 0 {
			n = n / 2
		} else {
			n = 3*n + 1
		}
	}
	return i
}

// Поиск числа с самой длинной цепочкой
func LongestChain(limit uint64) (longestChain uint, longestNum uint64) {
	for i := uint64(2); i < limit; i++ {
		length := chainLen(i)
		if length > longestChain {
			longestChain = length
			longestNum = i
		}
	}
	return
}
```

## Задание 2: Project Euler 16 - Сумма цифр степени

### Описание проблемы

Необходимо вычислить сумму всех цифр числа $2^{1000}$. Число $2^{1000}$ очень велико (около 300 цифр), поэтому требуется работа с большими числами. Пример: для $2^{15} = 32768$ сумма цифр равна $3 + 2 + 7 + 6 + 8 = 26$.

### Описание решения

Решение также реализовано шестью различными способами на OCaml:

1. **Монолитная хвостовая рекурсия** - хвостовая рекурсия для суммирования цифр
2. **Монолитная обычная рекурсия** - простая рекурсия по списку цифр
3. **Модульный конвейер** - генерация строки → фильтрация нулей → суммирование
4. **С использованием отображения** - преобразование символов в цифры + сумма
5. **С циклами** - императивный цикл по строке
6. **С последовательностями** - ленивые вычисления с Seq

Для работы с большими числами используется библиотека Zarith.

### Код решения

```ocaml
let explode_string s = List.init (String.length s) (String.get s)
let int_from_char c = int_of_char c - int_of_char '0'

let solve_monolithic_tail base power =
  let rec aux acc = function
    | [] -> acc
    | d :: rest -> aux (acc + int_from_char d) rest
  in
  Z.pow (Z.of_int base) power |> Z.to_string |> explode_string |> aux 0

let solve_monolithic_recursive base power =
  let rec aux = function [] -> 0 | d :: rest -> int_from_char d + aux rest in
  Z.pow (Z.of_int base) power |> Z.to_string |> explode_string |> aux

module PowerGenerator = struct
  let generate base power =
    Z.pow (Z.of_int base) power |> Z.to_string |> explode_string
end

module PowerFilter = struct
  let filter = List.filter (( <> ) '0')
end

module PowerReducer = struct
  let reduce = List.fold_left (fun acc c -> acc + int_from_char c) 0
end

let solve_modular base power =
  PowerGenerator.generate base power
  |> PowerFilter.filter |> PowerReducer.reduce

let solve_with_map base power =
  Z.pow (Z.of_int base) power
  |> Z.to_string |> explode_string |> List.map int_from_char
  |> List.fold_left ( + ) 0

let solve_with_loops base power =
  let acc = ref 0 in
  let numbers = Z.pow (Z.of_int base) power |> Z.to_string in
  for n = 0 to String.length numbers - 1 do
    let number = int_from_char numbers.[n] in
    acc := !acc + number
  done;
  !acc

let solve_with_seq base power =
  Z.pow (Z.of_int base) power
  |> Z.to_string |> String.to_seq |> Seq.map int_from_char
  |> Seq.fold_left ( + ) 0
```

```go
package lib

import (
	"math/big"
	"strconv"
)

// Сумма цифр строки
func sumDigits(s string) uint64 {
	sum := uint64(0)
	for _, c := range s {
		n, _ := strconv.Atoi(string(c))
		sum += uint64(n)
	}
	return sum
}

// Сумма цифр 2^1000
func PowerDigitSum() uint64 {
	n := new(big.Int)
	n.Exp(big.NewInt(2), big.NewInt(1000), nil)
	return sumDigits(n.String())
}
```

## Общие выводы

Работа демонстрирует богатство выразительных средств OCaml и важность выбора правильного подхода в зависимости от свойств задачи. Функциональный подход показывает свои преимущества в обработке рекурсивных задач и композиции функций: хвостовая рекурсия обеспечивает эффективность без переполнения стека, неизменяемость данных упрощает отладку, а модульные функции легко комбинируются в читаемые пайплайны. Ленивые вычисления с Seq эффективно работают с большими данными, экономя память, в то время как императивные циклы предлагают прямолинейное и быстрое решение для простых операций.

| Стиль               | Преимущества              | Недостатки                           | Подходит для                |
| ------------------- | ------------------------- | ------------------------------------ | --------------------------- |
| Монолитная рекурсия | Простота                  | Риск переполнения стека              | Маленькие данные            |
| Хвостовая рекурсия  | Эффективность             | Чуть сложнее читается                | Большие рекурсивные задачи  |
| Модульный           | Композируемость           | Много мелких функций                 | Сложная обработка данных    |
| Map/Fold            | Функциональная чистота    | Память для промежуточных результатов | Простая трансформация       |
| Циклы               | Понятность, эффективность | Мутации, побочные эффекты            | Простые императивные задачи |
| Seq                 | Ленивость, память         | Сложность отладки                    | Большие/бесконечные данные  |
