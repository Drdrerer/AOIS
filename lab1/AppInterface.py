from BitUtils import BitUtils
from IntRepresenter import IntRepresenter
from IntegerALU import IntegerALU
from FloatIEEE754 import FloatIEEE754
from BCD2421 import BCD2421

class AppInterface:
    def __init__(self):
        self.alu = IntegerALU()
        self.repr = IntRepresenter()

    def format_bits(self, bits: list) -> str:
        return "".join(map(str, bits))

    def run(self):
        while True:
            print("\n" + "="*40)
            print("ЛАБОРАТОРНАЯ РАБОТА 1. Представление чисел")
            print("1. Прямой, обратный, доп. коды (10 -> 2)")
            print("2. Сложение и вычитание (Доп. код)")
            print("3. Умножение (Прямой код)")
            print("4. Деление (Прямой код, точность 5)")
            print("5. IEEE-754 (Представление и сложение)")
            print("6. BCD 2421 (Сложение)")
            print("7. Перевод обратно в десятичные (2 -> 10)")
            print("0. Выход")
            print("="*40)
            
            choice = input("Выберите пункт меню: ")
            
            try:
                if choice == "1":
                    n = int(input("Введите целое число: "))
                    d = self.repr.to_direct(n)
                    print(f"Прямой:   {self.format_bits(d)}")
                    print(f"Обратный: {self.format_bits(BitUtils.invert(d) if n < 0 else d)}")
                    print(f"Дополн:   {self.format_bits(self.repr.to_complement(n))}")
                    
                elif choice == "2":
                    a, b = int(input("Введите A: ")), int(input("Введите B: "))
                    res_add = self.alu.add(a, b)
                    res_sub = self.alu.sub(a, b)
                    print(f"\nСЛОЖЕНИЕ A+B:\n2-ый: {self.format_bits(res_add)}\n10-ый: {self.repr.bits_to_int(res_add)}")
                    print(f"\nВЫЧИТАНИЕ A-B:\n2-ый: {self.format_bits(res_sub)}\n10-ый: {self.repr.bits_to_int(res_sub)}")

                elif choice == "3":
                    a, b = int(input("Введите A: ")), int(input("Введите B: "))
                    res_mul = self.alu.multiply(a, b)
                    print(f"\nУМНОЖЕНИЕ A*B:\n2-ый: {self.format_bits(res_mul)}\n10-ый: {self.repr.bits_to_int(res_mul, False)}")

                elif choice == "4":
                    a, b = int(input("Введите делимое A: ")), int(input("Введите делитель B: "))
                    int_bits, frac_bits, val_10 = self.alu.divide(a, b)
                    print(f"\nДЕЛЕНИЕ A/B:\n2-ый: {self.format_bits(int_bits)} . {self.format_bits(frac_bits)}")
                    print(f"10-ый: {val_10:.5f}")

                elif choice == "5":
                    a = float(input("Введите число A: "))
                    b = float(input("Введите число B: "))
                    
                    b_a = FloatIEEE754.to_ieee_bits(a)
                    b_b = FloatIEEE754.to_ieee_bits(b)
                    
                    print(f"\n[IEEE-754 БИТЫ В ПАМЯТИ]")
                    print(f"A: {b_a[0]} | {''.join(map(str, b_a[1:9]))} | {''.join(map(str, b_a[9:]))}")
                    print(f"B: {b_b[0]} | {''.join(map(str, b_b[1:9]))} | {''.join(map(str, b_b[9:]))}")

                    op = input("\nВыберите операцию (+, -, *, /): ")
                    
                    if op == '+': res_bits = FloatIEEE754.add_sub(b_a, b_b, is_sub=False)
                    elif op == '-': res_bits = FloatIEEE754.add_sub(b_a, b_b, is_sub=True)
                    elif op == '*': res_bits = FloatIEEE754.multiply(b_a, b_b)
                    elif op == '/': res_bits = FloatIEEE754.divide(b_a, b_b)
                    else:
                        print("Неизвестная операция.")
                        continue
                        
                    print(f"\n[РЕЗУЛЬТАТ]")
                    print(f"Биты: {res_bits[0]} | {''.join(map(str, res_bits[1:9]))} | {''.join(map(str, res_bits[9:]))}")
                    print(f"10-й вид: {FloatIEEE754.to_float(res_bits)}")
                    
                    expected = eval(f"{a} {op} {b}")
                    print(f"Ожидалось: {expected}")

                elif choice == "6":
                    a, b = int(input("Введите A: ")), int(input("Введите B: "))
                    res_bcd = BCD2421.add(a, b)
                    print(f"\nСЛОЖЕНИЕ BCD 2421:\n2-ый: {self.format_bits(res_bcd)}\n10-ый: {BCD2421.decode(res_bcd)}")

                elif choice == '7': 
                    bin_str = input("Введите двоичное число (символы 0 и 1): ").strip()
                    bits = [int(b) for b in bin_str if b in ('0', '1')]
                    
                    size = self.alu.repr.size
                    if len(bits) < size:
                        bits = [0] * (size - len(bits)) + bits
                    elif len(bits) > size:
                        bits = bits[-size:]
                        
                    is_comp_input = input("Это дополнительный код? (y/n): ").strip().lower()
                    is_complement = True if is_comp_input == 'y' else False
                    
                    res_int = self.alu.repr.bits_to_int(bits, is_complement=is_complement)
                    print(f"Результат в 10-ичной системе: {res_int}")

                elif choice == "0":
                    break
                else:
                    print("Неверный ввод!")
            except Exception as e:
                print(f"Ошибка выполнения: {e}")