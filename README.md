<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент(-ка)</b>: Приймак Юліанна КВ-23</p>
<p align="right"><b>2025</b>: рік</p>

## Загальне завдання

Завдання складається з двох частин:
1. Переписати функціональну реалізацію алгоритму сортування з лабораторної
роботи 3 з такими змінами:
використати функції вищого порядку для роботи з послідовностями (де/якщо
це доречно, в разі, якщо функції вищого порядку не були використані при
реалізації л.р. №3);
key та 
додати до інтерфейсу функції (та використання в реалізації) два ключових
параметра: 
test , що працюють аналогічно до того, як працюють
параметри з такими назвами в функціях, що працюють з послідовностями (р.
12). При цьому 
key має виконатись мінімальну кількість разів.
2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за
варіантом (див. п 4.1.2). Використання псевдофункцій не забороняється, але, за
можливості, має бути зменшене до необхідного мінімуму.

## Варіант першої частини 21 (1)
Алгоритм сортування вибором за незменшенням.

## Лістинг реалізації першої частини завдання
```lisp
(defun find-min-with-key (lst key test)
    (let* ((first-elem (first lst))
        (first-key (funcall key first-elem)))
    (if (null (rest lst))
        (values first-elem first-key)
        (multiple-value-bind (rest-min rest-key) 
            (find-min-with-key (rest lst) key test)
            (if (funcall test first-key rest-key)
                (values first-elem first-key)
                (values rest-min rest-key))))))

(defun remove-first-by-key (elem lst key)
    (let ((target-key (funcall key elem)))
    (cond
        ((null lst) nil)
        ((equal (funcall key (first lst)) target-key) (rest lst))
        (t (cons (first lst) (remove-first-by-key elem (rest lst) key))))))

(defun selection-sort-functional (lst &key (key #'identity) (test #'<))
    (if (null lst)
        nil
        (multiple-value-bind (min-elem min-key) 
            (find-min-with-key lst key test)
            (cons min-elem 
                (selection-sort-functional 
                (remove-first-by-key min-elem lst key)
                :key key 
                :test test)))))
```

### Тестові набори та утиліти першої частини
```lisp
(defun check-first-function (name input expected &rest sort-args)
    (format t "~a ~:[FAILED~;passed~]~%"
        name
        (equal (apply #'selection-sort-functional input sort-args) expected)))

(defun test-first-function ()
    (check-first-function "[Test 1]" '(8 3 7 1 9) '(1 3 7 8 9))  
    (check-first-function "[Test 2]" '(15 10 5) '(5 10 15)) 
    (check-first-function "[Test 3]" '(4 4 4) '(4 4 4))
    (check-first-function "[Test 4]" '(6 2 6 2 1) '(1 2 2 6 6))
    (check-first-function "[Test 5]" '(100) '(100))
    (check-first-function "[Test 6]" '(-3 7 -1 4) '(-1 -3 4 7) :key #'abs)
    (check-first-function "[Test 7]" '(2 7 4 9 3) '(9 7 4 3 2) :test #'>))
```

### Тестування першої частини
```lisp
[Test 1] passed
[Test 2] passed
[Test 3] passed
[Test 4] passed
[Test 5] passed
[Test 6] passed
[Test 7] passed
```

## Варіант другої частини 21 (9)
