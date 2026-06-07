from AVLNode import AVLNode

class AVLTree:
    """Сбалансированное дерево для разрешения коллизий"""
    def __init__(self):
        self.root = None

    def get_height(self, node: AVLNode) -> int:
        return node.height if node else 0

    def get_balance(self, node: AVLNode) -> int:
        return self.get_height(node.left) - self.get_height(node.right) if node else 0

    def _right_rotate(self, y: AVLNode) -> AVLNode:
        x = y.left
        T2 = x.right
        x.right = y
        y.left = T2
        y.height = 1 + max(self.get_height(y.left), self.get_height(y.right))
        x.height = 1 + max(self.get_height(x.left), self.get_height(x.right))
        return x

    def _left_rotate(self, x: AVLNode) -> AVLNode:
        y = x.right
        T2 = y.left
        y.left = x
        x.right = T2
        x.height = 1 + max(self.get_height(x.left), self.get_height(x.right))
        y.height = 1 + max(self.get_height(y.left), self.get_height(y.right))
        return y

    def insert(self, key: str, value: str):
        self.root = self._insert_node(self.root, key, value)

    def _insert_node(self, root: AVLNode, key: str, value: str) -> AVLNode:
        if not root:
            return AVLNode(key, value)
        elif key < root.key:
            root.left = self._insert_node(root.left, key, value)
        elif key > root.key:
            root.right = self._insert_node(root.right, key, value)
        else:
            root.value = value
            return root

        root.height = 1 + max(self.get_height(root.left), self.get_height(root.right))
        balance = self.get_balance(root)

        if balance > 1 and key < root.left.key:
            return self._right_rotate(root)
        if balance < -1 and key > root.right.key:
            return self._left_rotate(root)
        if balance > 1 and key > root.left.key:
            root.left = self._left_rotate(root.left)
            return self._right_rotate(root)
        if balance < -1 and key < root.right.key:
            root.right = self._right_rotate(root.right)
            return self._left_rotate(root)

        return root

    def search(self, key: str) -> AVLNode:
        return self._search_node(self.root, key)

    def _search_node(self, root: AVLNode, key: str) -> AVLNode:
        if not root or root.key == key:
            return root
        if root.key < key:
            return self._search_node(root.right, key)
        return self._search_node(root.left, key)

    def _get_min_value_node(self, root: AVLNode) -> AVLNode:
        if root is None or root.left is None:
            return root
        return self._get_min_value_node(root.left)

    def delete(self, key: str):
        self.root = self._delete_node(self.root, key)

    def _delete_node(self, root: AVLNode, key: str) -> AVLNode:
        if not root:
            return root
        elif key < root.key:
            root.left = self._delete_node(root.left, key)
        elif key > root.key:
            root.right = self._delete_node(root.right, key)
        else:
            if root.left is None:
                temp = root.right
                root = None
                return temp
            elif root.right is None:
                temp = root.left
                root = None
                return temp
            
            temp = self._get_min_value_node(root.right)
            root.key = temp.key
            root.value = temp.value
            root.right = self._delete_node(root.right, temp.key)

        if root is None:
            return root

        root.height = 1 + max(self.get_height(root.left), self.get_height(root.right))
        balance = self.get_balance(root)

        if balance > 1 and self.get_balance(root.left) >= 0:
            return self._right_rotate(root)
        if balance < -1 and self.get_balance(root.right) <= 0:
            return self._left_rotate(root)
        if balance > 1 and self.get_balance(root.left) < 0:
            root.left = self._left_rotate(root.left)
            return self._right_rotate(root)
        if balance < -1 and self.get_balance(root.right) > 0:
            root.right = self._right_rotate(root.right)
            return self._left_rotate(root)

        return root

    def get_all(self) -> list:
        """Служебный метод для вывода содержимого дерева."""
        items = []
        self._inorder(self.root, items)
        return items

    def _inorder(self, root: AVLNode, items: list):
        if root:
            self._inorder(root.left, items)
            items.append(f"{root.key}: {root.value}")
            self._inorder(root.right, items)