import unittest
import coverage
import io
import sys

_cov = coverage.Coverage(source=['HashTable', 'AVLTree', 'AVLNode'])
_cov.start()

from HashTable import HashTable
from AVLTree import AVLTree
from AVLNode import AVLNode

class TestAVLTree(unittest.TestCase):
    def test_insert_and_rotations(self):
        tree1 = AVLTree()
        for key in ["C", "B", "A"]:
            tree1.insert(key, "val")
        self.assertEqual(tree1.root.key, "B")

        tree2 = AVLTree()
        for key in ["A", "B", "C"]:
            tree2.insert(key, "val")
        self.assertEqual(tree2.root.key, "B")

        tree3 = AVLTree()
        for key in ["C", "A", "B"]:
            tree3.insert(key, "val")
        self.assertEqual(tree3.root.key, "B")

        tree4 = AVLTree()
        for key in ["A", "C", "B"]:
            tree4.insert(key, "val")
        self.assertEqual(tree4.root.key, "B")

    def test_update_existing_key(self):
        tree = AVLTree()
        tree.insert("Key1", "Value1")
        tree.insert("Key1", "Value2")
        self.assertEqual(tree.search("Key1").value, "Value2")

    def test_search(self):
        tree = AVLTree()
        tree.insert("A", "1")
        self.assertIsNotNone(tree.search("A"))
        self.assertIsNone(tree.search("B"))
    
    def test_delete_complex_rotations(self):
        tree_lr = AVLTree()
        for k in ["40", "20", "50", "30"]:
            tree_lr.insert(k, "val")
        tree_lr.delete("50")
        self.assertEqual(tree_lr.root.key, "30")

        tree_rl = AVLTree()
        for k in ["40", "30", "60", "50"]:
            tree_rl.insert(k, "val")
        tree_rl.delete("30")
        self.assertEqual(tree_rl.root.key, "50")

        tree_ll = AVLTree()
        for k in ["40", "20", "50", "10"]:
            tree_ll.insert(k, "val")
        tree_ll.delete("50")
        self.assertEqual(tree_ll.root.key, "20")

        tree_rr = AVLTree()
        for k in ["40", "30", "60", "70"]:
            tree_rr.insert(k, "val")
        tree_rr.delete("30")
        self.assertEqual(tree_rr.root.key, "60")

    def test_delete(self):
        tree = AVLTree()
        nodes = ["5", "3", "7", "2", "4", "6", "8", "9"]
        for n in nodes:
            tree.insert(n, n)

        tree.delete("2")
        self.assertIsNone(tree.search("2"))

        tree.delete("8")
        self.assertIsNone(tree.search("8"))
        self.assertIsNotNone(tree.search("9"))

        tree.delete("3")
        self.assertIsNone(tree.search("3"))
        self.assertIsNotNone(tree.search("4")) # 4 заменит 3

        tree.delete("5")
        self.assertIsNone(tree.search("5"))

        tree.delete("999")

        empty_tree = AVLTree()
        empty_tree.delete("A")
        self.assertIsNone(empty_tree.root)

    def test_get_all(self):
        tree = AVLTree()
        tree.insert("B", "2")
        tree.insert("A", "1")
        tree.insert("C", "3")
        self.assertEqual(tree.get_all(), ["A: 1", "B: 2", "C: 3"])


class TestHashTable(unittest.TestCase):
    def test_calculate_v(self):
        ht = HashTable()
        
        self.assertEqual(ht.calculate_v("Вяткин"), 2 * 33 + 32)
        
        self.assertEqual(ht.calculate_v("В"), 2 * 33 + 0)
        
        self.assertEqual(ht.calculate_v(""), 0)
        
        self.assertEqual(ht.calculate_v("John"), 0)

    def test_calculate_h(self):
        ht = HashTable(size=20, b=5)
        self.assertEqual(ht.calculate_h(25), 25 % 20 + 5)

    def test_crud_operations(self):
        ht = HashTable(size=20)
        
        ht.insert("Вяткин", "Студент 1")
        ht.insert("Третьяк", "Студент 2")
        
        self.assertEqual(ht.search("Вяткин"), "Студент 1")
        self.assertEqual(ht.search("Третьяк"), "Студент 2")
        self.assertIsNone(ht.search("Неизвестный"))
        
        ht.insert("Третьяк", "Обновлен")
        self.assertEqual(ht.search("Третьяк"), "Обновлен")
        
        ht.delete("Вяткин")
        self.assertIsNone(ht.search("Вяткин"))
        
        ht.delete("Неизвестный")

    def test_display(self):
        ht = HashTable(size=3)
        ht.insert("А", "Данные 1")
        
        captured_output = io.StringIO()
        sys.stdout = captured_output
        ht.display()
        sys.stdout = sys.__stdout__
        
        output = captured_output.getvalue()
        
        self.assertIn("А: Данные 1", output)
        self.assertIn("[Пусто]", output)

if __name__ == '__main__':
    try:
        unittest.main(exit=False)
    finally:
        _cov.stop()
        _cov.save()
        print("\n" + "="*60 + "\nИТОГОВОЕ ПОКРЫТИЕ\n" + "="*60)
        _cov.report(show_missing=True)