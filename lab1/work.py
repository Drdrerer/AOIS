import sys


class BitUtils:
    @staticmethod
    def add_raw(a: list, b: list) -> list:
        size = len(a)
        res = [0] * size
        carry = 0
        for i in range(size - 1, -1, -1):
            s = a[i] + b[i] + carry
            res[i] = s % 2
            carry = s // 2
        return res

    @staticmethod
    def invert(bits: list) -> list:
        return [bits[0]] + [1 - b for b in bits[1:]]


class IntRepresenter:
    def __init__(self, size=32):
        self.size = size

    def to_direct(self, n: int) -> list:
        arr = [0] * self.size
        num = abs(int(n))
        for i in range(self.size - 1, 0, -1):
            arr[i] = num % 2
            num //= 2
        if n < 0: arr[0] = 1
        return arr

    def to_complement(self, n: int) -> list:
        direct = self.to_direct(n)
        if n >= 0: return direct
        inv = BitUtils.invert(direct)
        one = [0] * (self.size - 1) + [1]
        return BitUtils.add_raw(inv, one)

    def bits_to_int(self, bits: list, is_complement=True) -> int:
        if is_complement and bits[0] == 1:
            one_comp = BitUtils.add_raw(bits, [1]*32)
            direct = BitUtils.invert(one_comp)
        else:
            direct = bits
            
        val = 0
        for i in range(1, self.size):
            val += direct[i] * (2 ** (self.size - 1 - i))
        return -val if direct[0] == 1 else val


class IntegerALU:
    def __init__(self, size=32):
        self.repr = IntRepresenter(size)
        self.size = size

    def add(self, a: int, b: int) -> list:
        return BitUtils.add_raw(self.repr.to_complement(a), self.repr.to_complement(b))

    def sub(self, a: int, b: int) -> list:
        return self.add(a, -b)

    def multiply(self, a: int, b: int) -> list:
        d1, d2 = self.repr.to_direct(a), self.repr.to_direct(b)
        res_sign = d1[0] ^ d2[0]
        acc = [0] * (self.size - 1)
        m_cand, m_plier = d1[1:], d2[1:]
        
        for i in range(len(m_plier)-1, -1, -1):
            if m_plier[i] == 1:
                shifted = [0] * (self.size - 1)
                shift = (len(m_plier)-1) - i
                for j in range((len(m_plier)-1) - shift, -1, -1):
                    shifted[j] = m_cand[j + shift]
                acc = BitUtils.add_raw(acc, shifted)
        return [res_sign] + acc

    def divide(self, a: int, b: int, precision=5):
        if b == 0: raise ValueError("Деление на ноль!")
        sign = 0 if (a >= 0) == (b >= 0) else 1
        dvd, dvs = abs(a), abs(b)
        q_int = dvd // dvs
        rem = dvd % dvs
        
        frac_bits = []
        for _ in range(precision):
            rem *= 2
            frac_bits.append(1 if rem >= dvs else 0)
            if rem >= dvs: rem -= dvs
            
        int_bits = self.repr.to_direct(q_int if sign == 0 else -q_int)
        val_10 = q_int + sum(bit * (2**-(i+1)) for i, bit in enumerate(frac_bits))
        if sign == 1: val_10 = -val_10
        
        return int_bits, frac_bits, val_10


class FloatIEEE754:
    @staticmethod
    def to_ieee_bits(f: float) -> list:
        if f == 0.0: return [0] * 32
        sign = 0 if f > 0 else 1
        f = abs(f)
        
        exp = 127
        if f >= 1.0:
            while f >= 2.0: f /= 2.0; exp += 1
        else:
            while f < 1.0: f *= 2.0; exp -= 1
            
        f -= 1.0
        mantissa = []
        for _ in range(23):
            f *= 2.0
            bit = int(f)
            mantissa.append(bit)
            f -= bit
            
        exp_bits = [0]*8
        temp_e = exp
        for i in range(7, -1, -1):
            exp_bits[i] = temp_e % 2
            temp_e //= 2
            
        return [sign] + exp_bits + mantissa

    @staticmethod
    def to_float(bits: list) -> float:
        if all(b == 0 for b in bits[1:]): return 0.0
        sign = -1 if bits[0] == 1 else 1
        exp = sum(bits[i+1] * (2**(7-i)) for i in range(8)) - 127
        mantissa = 1.0 + sum(bits[i+9] * (2**-(i+1)) for i in range(23))
        return sign * mantissa * (2 ** exp)

    @classmethod
    def _extract(cls, bits: list):
        sign = bits[0]
        exp = sum(bits[i+1] * (2**(7-i)) for i in range(8))
        mant = (1 * (2**23)) + sum(bits[i+9] * (2**(22-i)) for i in range(23))
        if exp == 0: mant -= (1 * (2**23)) 
        return sign, exp, mant

    @classmethod
    def _pack(cls, sign: int, exp: int, mant: int) -> list:
        if exp <= 0: return [0] * 32
        
        res = [0] * 32
        res[0] = sign
        
        for i in range(8, 0, -1):
            res[i] = exp % 2
            exp //= 2
        
        mant = mant % (2**23)
        for i in range(31, 8, -1):
            res[i] = mant % 2
            mant //= 2
            
        return res

    @classmethod
    def add_sub(cls, bits1: list, bits2: list, is_sub=False) -> list:
        s1, e1, m1 = cls._extract(bits1)
        s2, e2, m2 = cls._extract(bits2)
        if is_sub: s2 ^= 1
        
        if e1 == 0 and m1 == 0: return bits2 if not is_sub else [s2]+bits2[1:]
        if e2 == 0 and m2 == 0: return bits1
        
        if e1 > e2:
            m2 //= (2 ** (e1 - e2))
            res_e = e1
        else:
            m1 //= (2 ** (e2 - e1))
            res_e = e2
        
        if s1 == s2:
            res_m = m1 + m2
            res_s = s1
        else:
            if m1 >= m2:
                res_m = m1 - m2
                res_s = s1
            else:
                res_m = m2 - m1
                res_s = s2
                
        if res_m == 0: return [0] * 32
        
        if res_m >= (2**24):
            res_m //= 2
            res_e += 1
       
        else:
            while res_m < (2**23) and res_e > 0:
                res_m *= 2
                res_e -= 1
                
        return cls._pack(res_s, res_e, res_m)

    @classmethod
    def multiply(cls, bits1: list, bits2: list) -> list:
        s1, e1, m1 = cls._extract(bits1)
        s2, e2, m2 = cls._extract(bits2)
        
        if (e1 == 0 and m1 == 0) or (e2 == 0 and m2 == 0): return [0]*32
        
        res_s = s1 ^ s2
        res_e = e1 + e2 - 127
        
        res_m = m1 * m2
        
        if res_m >= (2**47):
            res_m //= (2**24)
            res_e += 1
        else:
            res_m //= (2**23) 
            
        return cls._pack(res_s, res_e, res_m)

    @classmethod
    def divide(cls, bits1: list, bits2: list) -> list:
        s1, e1, m1 = cls._extract(bits1)
        s2, e2, m2 = cls._extract(bits2)
        
        if e2 == 0 and m2 == 0: raise ValueError("Деление на ноль!")
        if e1 == 0 and m1 == 0: return [0]*32
        
        res_s = s1 ^ s2
        res_e = e1 - e2 + 127 
       
        q = (m1 * (2**24)) // m2
        
        if q >= (2**24):
            res_m = q // 2
            res_e += 1
        else:
            res_m = q
            
        return cls._pack(res_s, res_e, res_m)

class BCD2421:
    TABLE = {0:[0,0,0,0], 1:[0,0,0,1], 2:[0,0,1,0], 3:[0,0,1,1], 4:[0,1,0,0],
             5:[1,0,1,1], 6:[1,1,0,0], 7:[1,1,0,1], 8:[1,1,1,0], 9:[1,1,1,1]}
    
    @classmethod
    def encode(cls, n: int) -> list:
        digits = str(abs(n)).zfill(8)
        res = []
        for d in digits: res.extend(cls.TABLE[int(d)])
        return res

    @classmethod
    def decode(cls, bits: list) -> int:
        val = 0
        for i in range(8):
            t = bits[i*4 : i*4+4]
            digit = sum(b*w for b,w in zip(t, [2,4,2,1]))
            if digit > 9: digit -= 6
            val = val * 10 + digit
        return val

    @classmethod
    def add(cls, n1: int, n2: int) -> list:
        b1, b2 = cls.encode(n1), cls.encode(n2)
        res, carry = [0]*32, 0
        for i in range(7, -1, -1):
            t1 = sum(b*w for b,w in zip(b1[i*4:i*4+4], [2,4,2,1]))
            t2 = sum(b*w for b,w in zip(b2[i*4:i*4+4], [2,4,2,1]))
            s = t1 + t2 + carry
            carry = 1 if s > 9 else 0
            res[i*4:i*4+4] = cls.TABLE[s % 10]
        return res


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
                    print(f"Ожидалось: {expected} (Погрешность стандарта)")

                elif choice == "6":
                    a, b = int(input("Введите A: ")), int(input("Введите B: "))
                    res_bcd = BCD2421.add(a, b)
                    print(f"\nСЛОЖЕНИЕ BCD 2421:\n2-ый: {self.format_bits(res_bcd)}\n10-ый: {BCD2421.decode(res_bcd)}")

                elif choice == "0":
                    break
                else:
                    print("Неверный ввод!")
            except Exception as e:
                print(f"Ошибка выполнения: {e}")

if __name__ == "__main__":
    app = AppInterface()
    app.run()
