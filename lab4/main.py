from HashTable import HashTable

def main():
    ht = HashTable(size=20)
    
    while True:
        print("\n" + "="*30)
        print(" МЕНЮ ХЕШ-ТАБЛИЦЫ (АВЛ-ДЕРЕВО)")
        print("="*30)
        print("1. Добавить / Обновить запись")
        print("2. Найти запись")
        print("3. Удалить запись")
        print("4. Вывести хеш-таблицу")
        print("5. Выход")
        print("="*30)
        
        choice = input("Выберите действие (1-5): ").strip()
        
        if choice == '1':
            name = input("Введите фамилию (ключ): ").strip()
            if not name:
                print("[!] Ошибка: Ключ не может быть пустым.")
                continue
            
            data = input("Введите данные студента (например, имя, увлечения): ").strip()
            
            v_val = ht.calculate_v(name)
            h_val = ht.calculate_h(v_val)
            ht.insert(name, data)
            
            print(f"\n[+] Успех: Запись '{name}' добавлена/обновлена.")
            print(f"    Служебная информация: V = {v_val}, h(V) = {h_val}")
            
        elif choice == '2':
            name = input("Введите фамилию для поиска: ").strip()
            if not name:
                continue
                
            result = ht.search(name)
            if result:
                print(f"\n[+] Найдено: {name} -> {result}")
            else:
                print(f"\n[-] Ошибка: Запись с ключом '{name}' не найдена.")
                
        elif choice == '3':
            name = input("Введите фамилию для удаления: ").strip()
            if not name:
                continue
                
            if ht.search(name):
                ht.delete(name)
                print(f"\n[+] Успех: Запись '{name}' удалена.")
            else:
                print(f"\n[-] Ошибка: Запись с ключом '{name}' не найдена.")
                
        elif choice == '4':
            print("\n--- Текущее состояние хеш-таблицы ---")
            ht.display()
            
        elif choice == '5':
            print("\nЗавершение работы программы. До свидания!")
            break
            
        else:
            print("\n[!] Неверный ввод. Пожалуйста, введите число от 1 до 5.")

if __name__ == "__main__":
    main()