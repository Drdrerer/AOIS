from IntRepresenter import IntRepresenter 
from BitUtils import BitUtils

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