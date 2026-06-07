import unittest
from unittest.mock import patch
import coverage
import io
import sys

_cov = coverage.Coverage(source=['lab2'])
_cov.start()

# Импортируем все необходимые классы и функции из вашего файла lab2.py
from lab2 import (
    BooleanParser, 
    term_to_str, 
    quine_mccluskey, 
    print_coverage_table, 
    print_karnaugh_map, 
    main
)

class TestBooleanParser(unittest.TestCase):
    def test_basic_operations(self):
        """Проверка базовых логических операций (И, ИЛИ, НЕ)"""
        parser = BooleanParser("a & b")
        self.assertTrue(parser.parse({'a': True, 'b': True}))
        self.assertFalse(parser.parse({'a': True, 'b': False}))

        parser = BooleanParser("a | b")
        self.assertTrue(parser.parse({'a': False, 'b': True}))
        self.assertFalse(parser.parse({'a': False, 'b': False}))

        parser = BooleanParser("!a")
        self.assertTrue(parser.parse({'a': False}))
        self.assertFalse(parser.parse({'a': True}))

    def test_complex_operations(self):
        """Проверка импликации и эквивалентности"""
        parser = BooleanParser("a -> b")
        self.assertFalse(parser.parse({'a': True, 'b': False}))
        self.assertTrue(parser.parse({'a': False, 'b': False}))

        parser = BooleanParser("a ~ b")
        self.assertTrue(parser.parse({'a': True, 'b': True}))
        self.assertFalse(parser.parse({'a': True, 'b': False}))

    def test_parser_exceptions(self):
        """Проверка обработки синтаксических ошибок (скобки, левые символы)"""
        with self.assertRaises(ValueError):
            parser = BooleanParser("(a & b")
            parser.parse({'a': True, 'b': True})
            
        with self.assertRaises(ValueError):
            parser = BooleanParser("a -> ") 
            parser.parse({'a': True, 'b': True})

    def test_empty_expression(self):
        """Проверка парсинга пустой строки"""
        parser = BooleanParser("")
        self.assertFalse(parser.parse({}))


class TestMinimizationHelpers(unittest.TestCase):
    def test_term_to_str(self):
        """Проверка конвертации маски в строку ДНФ и КНФ"""
        vars_list = ['a', 'b', 'c']
        # ДНФ
        self.assertEqual(term_to_str('10-', vars_list, is_cnf=False), "(a & !b)")
        self.assertEqual(term_to_str('---', vars_list, is_cnf=False), "1")
        # КНФ
        self.assertEqual(term_to_str('01-', vars_list, is_cnf=True), "(a | !b)")
        self.assertEqual(term_to_str('---', vars_list, is_cnf=True), "0")

    @patch('sys.stdout', new_callable=io.StringIO)
    def test_quine_mccluskey(self, mock_stdout):
        """Проверка алгоритма склеивания К-М"""
        vars_list = ['a', 'b', 'c']
        # Функция, равная 1 везде, кроме 000
        target_indices = [1, 2, 3, 4, 5, 6, 7] 
        pi_keys, best_cover, all_pi = quine_mccluskey(3, target_indices, vars_list, is_cnf=False)
        
        # Проверяем, что алгоритм нашел минимальное покрытие
        self.assertTrue(len(best_cover) > 0)
        
        # Проверка на пустые индексы
        pi, cover, dict_pi = quine_mccluskey(3, [], vars_list)
        self.assertEqual(pi, [])
        self.assertEqual(cover, [])


class TestPrintFunctions(unittest.TestCase):
    @patch('sys.stdout', new_callable=io.StringIO)
    def test_print_coverage_table(self, mock_stdout):
        """Проверка функции вывода таблицы покрытия (не должна падать)"""
        vars_list = ['a', 'b']
        print_coverage_table(['1-'], [2, 3], {'1-': {2, 3}}, vars_list, is_cnf=False)
        self.assertIn('X', mock_stdout.getvalue())
        
        # Вызов с пустыми индексами (ничего не должна печатать)
        mock_stdout.truncate(0)
        print_coverage_table([], [], {}, vars_list, False)
        self.assertEqual(mock_stdout.getvalue(), "")

    @patch('sys.stdout', new_callable=io.StringIO)
    def test_print_karnaugh_map(self, mock_stdout):
        """Проверка вывода карты Карно для разных количеств переменных"""
        # Меньше 2 переменных
        print_karnaugh_map(['a'], [0, 1])
        self.assertIn('тривиальна', mock_stdout.getvalue())

        # Нормальная карта (3 переменные)
        mock_stdout.truncate(0)
        print_karnaugh_map(['a', 'b', 'c'], [0, 1, 1, 1, 1, 1, 1, 1])
        self.assertIn('01', mock_stdout.getvalue())


class TestMainExecution(unittest.TestCase):
    @patch('sys.stdout', new_callable=io.StringIO)
    @patch('builtins.input', return_value="((!a->c) | b)")
    def test_main_normal_flow(self, mock_input, mock_stdout):
        """Прогон всей программы от начала до конца (интеграционный тест)"""
        main()
        output = mock_stdout.getvalue()
        # Проверяем наличие ключевых строк в выводе
        self.assertIn("Таблица истинности функции:", output)
        self.assertIn("СДНФ: (!a & !b & c)", output)
        self.assertIn("Индексная форма функции: 127", output)
        self.assertIn("Класс T0", output)
        self.assertIn("Класс L", output)
        self.assertIn("Минимизация табличным методом (Карта Карно)", output)

    @patch('sys.stdout', new_callable=io.StringIO)
    @patch('builtins.input', return_value="")
    def test_main_empty_input(self, mock_input, mock_stdout):
        """Проверка работы с функцией по умолчанию (если нажат Enter)"""
        main()
        self.assertIn("Используется тестовая функция", mock_stdout.getvalue())

    @patch('sys.stdout', new_callable=io.StringIO)
    @patch('builtins.input', return_value="1 & 0")
    def test_main_no_variables(self, mock_input, mock_stdout):
        """Проверка ошибки отсутствия переменных"""
        main()
        self.assertIn("не найдено переменных", mock_stdout.getvalue())

    @patch('sys.stdout', new_callable=io.StringIO)
    @patch('builtins.input', return_value="a & (b")
    def test_main_calculation_error(self, mock_input, mock_stdout):
        """Проверка перехвата исключения парсера внутри main"""
        main()
        self.assertIn("Ошибка при вычислении выражения", mock_stdout.getvalue())

if __name__ == '__main__':
    try:
        unittest.main(exit=False)
    finally:
        _cov.stop()
        _cov.save()
        print("\n" + "="*60 + "\nИТОГОВОЕ ПОКРЫТИЕ\n" + "="*60)
        _cov.report(show_missing=True)