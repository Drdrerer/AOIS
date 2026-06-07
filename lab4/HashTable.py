from AVLTree import AVLTree

class HashTable:
    def __init__(self, size: int = 20, b: int = 0):
        self.size = size
        self.b = b
        self.table = [AVLTree() for _ in range(size)]
        self.alphabet = "абвгдеёжзийклмнопрстуфхцчшщъыьэюя"

    def calculate_v(self, key: str) -> int:
        """Вычисляет числовое значение V по первым двум буквам ключа."""
        k = key.lower()
        c1 = k[0] if len(k) > 0 else 'а'
        c2 = k[1] if len(k) > 1 else 'а'

        v1 = self.alphabet.find(c1) if c1 in self.alphabet else 0
        v2 = self.alphabet.find(c2) if c2 in self.alphabet else 0

        return v1 * 33 + v2

    def calculate_h(self, v: int) -> int:
        """Вычисляет хеш-адрес h(V)."""
        return (v % self.size) + self.b

    def insert(self, key: str, value: str):
        """Create / Update"""
        v = self.calculate_v(key)
        h = self.calculate_h(v)
        self.table[h].insert(key, value)

    def search(self, key: str):
        """Read"""
        v = self.calculate_v(key)
        h = self.calculate_h(v)
        node = self.table[h].search(key)
        return node.value if node else None

    def delete(self, key: str):
        """Delete"""
        v = self.calculate_v(key)
        h = self.calculate_h(v)
        self.table[h].delete(key)

    def display(self):
        """Вывод таблицы и цепочек коллизий."""
        print(f"{'h':<4} | {'Данные (цепочка дерева)'}")
        print("-" * 40)
        for i in range(self.size):
            items = self.table[i].get_all()
            if items:
                print(f"{i:<4} | {', '.join(items)}")
            else:
                print(f"{i:<4} | [Пусто]")