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

        q = (m1 << 23) // m2
        
        if q < (1 << 23):
            q = (m1 << 24) // m2
            res_e -= 1
            
        return cls._pack(res_s, res_e, q)