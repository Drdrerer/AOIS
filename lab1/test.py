import unittest
from unittest.mock import patch
import coverage
from io import StringIO

_cov = coverage.Coverage(source=['BitUtils', 'IntRepresenter', 'IntegerALU', 'FloatIEEE754', 'BCD2421', 'AppInterface'])
_cov.start()

from BitUtils import BitUtils
from IntRepresenter import IntRepresenter
from IntegerALU import IntegerALU
from FloatIEEE754 import FloatIEEE754
from BCD2421 import BCD2421
from AppInterface import AppInterface

class TestBitUtils(unittest.TestCase):
    def test_add_raw(self):
        self.assertEqual(BitUtils.add_raw([0, 1], [0, 1]), [1, 0])
        self.assertEqual(BitUtils.add_raw([1, 1], [0, 1]), [0, 0])

    def test_invert(self):
        self.assertEqual(BitUtils.invert([1, 0, 1, 1]), [1, 1, 0, 0])
        self.assertEqual(BitUtils.invert([0, 1, 1, 0]), [0, 0, 0, 1])


class TestIntRepresenter(unittest.TestCase):
    def setUp(self):
        self.repr = IntRepresenter(32)

    def test_to_direct(self):
        self.assertEqual(self.repr.to_direct(5)[-3:], [1, 0, 1])
        self.assertEqual(self.repr.to_direct(-5)[0], 1)

    def test_to_complement(self):
        self.assertEqual(self.repr.to_complement(5)[-3:], [1, 0, 1])
        bits = self.repr.to_complement(-10)
        self.assertEqual(self.repr.bits_to_int(bits), -10)
        self.assertEqual(self.repr.bits_to_int(self.repr.to_complement(10)), 10)

    def test_bits_to_int_direct(self):
        # Покрытие ветки, где is_complement=False
        bits_pos = self.repr.to_direct(15)
        self.assertEqual(self.repr.bits_to_int(bits_pos, is_complement=False), 15)
        bits_neg = self.repr.to_direct(-15)
        self.assertEqual(self.repr.bits_to_int(bits_neg, is_complement=False), -15)


class TestIntegerALU(unittest.TestCase):
    def setUp(self):
        self.alu = IntegerALU(32)

    def test_arithmetic_int(self):
        self.assertEqual(self.alu.repr.bits_to_int(self.alu.add(10, -5)), 5)
        self.assertEqual(self.alu.repr.bits_to_int(self.alu.sub(-10, 5)), -15)
        self.assertEqual(self.alu.repr.bits_to_int(self.alu.multiply(-3, -4), False), 12)
        self.assertEqual(self.alu.repr.bits_to_int(self.alu.multiply(-3, 4), False), -12)
        
        _, _, val = self.alu.divide(-7, 2)
        self.assertEqual(val, -3.5)
        with self.assertRaises(ValueError):
            self.alu.divide(10, 0)

    def test_divide_sign_branches(self):
        # Покрытие всех комбинаций знаков при делении
        _, _, val = self.alu.divide(7, -2)
        self.assertEqual(val, -3.5)
        _, _, val = self.alu.divide(-7, -2)
        self.assertEqual(val, 3.5)
        _, _, val = self.alu.divide(7, 2)
        self.assertEqual(val, 3.5)


class TestFloatIEEE754(unittest.TestCase):
    def test_conversions(self):
        bits = FloatIEEE754.to_ieee_bits(15.625)
        self.assertEqual(FloatIEEE754.to_float(bits), 15.625)
        self.assertEqual(FloatIEEE754.to_float([0]*32), 0.0)
        self.assertEqual(FloatIEEE754.to_float(FloatIEEE754.to_ieee_bits(-1.5)), -1.5)

    def test_arithmetic_float(self):
        a_b = FloatIEEE754.to_ieee_bits(10.5)
        b_b = FloatIEEE754.to_ieee_bits(2.25)
        
        self.assertAlmostEqual(FloatIEEE754.to_float(FloatIEEE754.add_sub(a_b, b_b)), 12.75)
        self.assertAlmostEqual(FloatIEEE754.to_float(FloatIEEE754.add_sub(a_b, b_b, True)), 8.25)
        self.assertAlmostEqual(FloatIEEE754.to_float(FloatIEEE754.multiply(a_b, b_b)), 23.625)
        self.assertAlmostEqual(FloatIEEE754.to_float(FloatIEEE754.divide(a_b, b_b)), 4.6666666, places=6)

    def test_float_branches(self):
        a = FloatIEEE754.to_ieee_bits(1.0)
        b = FloatIEEE754.to_ieee_bits(100.0)
        self.assertAlmostEqual(FloatIEEE754.to_float(FloatIEEE754.add_sub(a, b)), 101.0)
        
        v = FloatIEEE754.to_ieee_bits(5.0)
        self.assertEqual(FloatIEEE754.to_float(FloatIEEE754.add_sub(v, v, True)), 0.0)
        
        zero = [0]*32
        self.assertEqual(FloatIEEE754.to_float(FloatIEEE754.multiply(v, zero)), 0.0)

    def test_float_edge_cases(self):
        # Деление на ноль
        zero = FloatIEEE754.to_ieee_bits(0.0)
        a = FloatIEEE754.to_ieee_bits(5.0)
        with self.assertRaises(ValueError):
            FloatIEEE754.divide(a, zero)

        # Вычитание, где M1 < M2 (смена знака)
        c1 = FloatIEEE754.to_ieee_bits(1.0)
        c2 = FloatIEEE754.to_ieee_bits(3.0)
        self.assertEqual(FloatIEEE754.to_float(FloatIEEE754.add_sub(c1, c2, is_sub=True)), -2.0)

        # Вычитание, требующее сдвига (нормализации) мантиссы
        c3 = FloatIEEE754.to_ieee_bits(2.0)
        c4 = FloatIEEE754.to_ieee_bits(1.875)
        self.assertEqual(FloatIEEE754.to_float(FloatIEEE754.add_sub(c3, c4, is_sub=True)), 0.125)

        # Деление, требующее сдвига q < (1 << 23)
        res_div = FloatIEEE754.divide(FloatIEEE754.to_ieee_bits(3.0), FloatIEEE754.to_ieee_bits(4.0))
        self.assertEqual(FloatIEEE754.to_float(res_div), 0.75)

        # Ситуация exp <= 0 в pack_ (слишком маленькое число)
        tiny = FloatIEEE754.to_ieee_bits(1e-30)
        huge = FloatIEEE754.to_ieee_bits(1e30)
        res_tiny = FloatIEEE754.divide(tiny, huge)
        self.assertEqual(FloatIEEE754.to_float(res_tiny), 0.0)

        # Умножение с переполнением мантиссы
        m1 = FloatIEEE754.to_ieee_bits(1.9999)
        res_mul = FloatIEEE754.multiply(m1, m1)
        self.assertAlmostEqual(FloatIEEE754.to_float(res_mul), 1.9999 * 1.9999, places=3)


class TestBCD2421(unittest.TestCase):
    def test_bcd_logic(self):
        enc = BCD2421.encode(123)
        self.assertEqual(BCD2421.decode(enc), 123)
        self.assertEqual(BCD2421.decode(BCD2421.add(5, 6)), 11)
        self.assertEqual(BCD2421.decode(BCD2421.add(99, 1)), 100)

    def test_bcd_negative(self):
        # Проверка логики отрицательных чисел и дополнения до 10^8
        enc_neg = BCD2421.encode(-18)
        self.assertEqual(BCD2421.decode(enc_neg), -18)
        
        # Сложение положительного и отрицательного числа
        res = BCD2421.add(4, -18)
        self.assertEqual(BCD2421.decode(res), -14)


class TestInterface(unittest.TestCase):
    @patch('builtins.input', side_effect=[
        '1', '10',              
        '1', '-10',             
        '2', '10', '5',         
        '3', '3', '4',          
        '4', '7', '2',          
        '5', '1.0', '2.0', '+', 
        '5', '1.0', '2.0', '-', 
        '5', '2.0', '1.0', '-', 
        '5', '1.0', '2.0', '*', 
        '5', '1.0', '2.0', '/',
        '5', '1.0', '2.0', '!',
        '6', '5', '6',          
        '0'                    
    ])
    def test_interface_and_errors(self, mock_input):
        app = AppInterface()
        with patch('sys.stdout', new=StringIO()):
            app.run()
            
    @patch('builtins.input', side_effect=['4', '10', '0', '0']) 
    def test_interface_div_zero(self, mock_input):
        app = AppInterface()
        with patch('sys.stdout', new=StringIO()):
            app.run()

    @patch('builtins.input', side_effect=['99', '0']) 
    def test_interface_invalid_choice(self, mock_input):
        # Проверка ввода несуществующего пункта меню
        app = AppInterface()
        with patch('sys.stdout', new=StringIO()):
            app.run()

    @patch('builtins.input', side_effect=['1', 'not_a_number', '0']) 
    def test_interface_exception_handling(self, mock_input):
        # Проверка блока except Exception: (ввод строки вместо числа)
        app = AppInterface()
        with patch('sys.stdout', new=StringIO()):
            app.run()

if __name__ == '__main__':
    try:
        unittest.main(exit=False)
    finally:
        _cov.stop()
        _cov.save()
        print("\n" + "="*60 + "\nИТОГОВОЕ ПОКРЫТИЕ\n" + "="*60)
        _cov.report(show_missing=True)
