from BitUtils import BitUtils

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
