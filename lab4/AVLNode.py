class AVLNode:
    """Узел сбалансированного АВЛ-дерева для цепочек коллизий"""
    def __init__(self, key: str, value: str):
        self.key = key
        self.value = value
        self.left = None
        self.right = None
        self.height = 1