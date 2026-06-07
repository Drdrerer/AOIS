class BCD2421:
    TABLE = {0:[0,0,0,0], 1:[0,0,0,1], 2:[0,0,1,0], 3:[0,0,1,1], 4:[0,1,0,0],
             5:[1,0,1,1], 6:[1,1,0,0], 7:[1,1,0,1], 8:[1,1,1,0], 9:[1,1,1,1]}
    
    @classmethod
    def encode(cls, n: int) -> list:
        if n < 0:
            n = (10**8) + n
            
        res = [0] * 32
        temp = n
        
        for i in range(7, -1, -1):
            digit = temp % 10  
            temp //= 10         
            
            res[i*4 : i*4+4] = cls.TABLE[digit]
            
        return res

    @classmethod
    def decode(cls, bits: list) -> int:
        val = 0
        
        for i in range(8):
            t = bits[i*4 : i*4+4]
            digit = sum(b*w for b,w in zip(t, [2, 4, 2, 1]))
            val = val * 10 + digit
            
        if val >= 50000000:
            val -= 10**8
            
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