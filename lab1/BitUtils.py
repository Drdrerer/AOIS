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