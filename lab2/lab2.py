import itertools
import re
import math

class BooleanParser:
    """
    Рекурсивный нисходящий парсер для вычисления логических выражений
    с учетом приоритетов операций: ! -> & -> | -> -> -> ~
    """
    def __init__(self, expr_str):
        expr_str = expr_str.replace('/\\', '&').replace('V', '|').replace('v', '|').replace('\\/', '|').replace('→', '->')
        self.tokens = re.findall(r'[a-e]|->|!|&|\||~|\(|\)', expr_str.lower())
        self.pos = 0

    def peek(self):
        return self.tokens[self.pos] if self.pos < len(self.tokens) else None

    def match(self, token):
        if self.peek() == token:
            self.pos += 1
            return True
        return False

    def parse(self, env):
        self.pos = 0
        if not self.tokens:
            return False
        
        res = self.expr_equivalence(env)
        
        if self.pos < len(self.tokens):
            raise ValueError(f"Синтаксическая ошибка: лишние символы или несовпадение скобок на токене '{self.peek()}'")
        
        return res

    def expr_equivalence(self, env):
        res = self.expr_implication(env)
        while self.match('~'):
            right = self.expr_implication(env)
            res = (res == right)
        return res

    def expr_implication(self, env):
        res = self.expr_or(env)
        while self.match('->'):
            right = self.expr_or(env)
            res = (not res) or right
        return res

    def expr_or(self, env):
        res = self.expr_and(env)
        while self.match('|'):
            right = self.expr_and(env)
            res = res or right
        return res

    def expr_and(self, env):
        res = self.expr_not(env)
        while self.match('&'):
            right = self.expr_not(env)
            res = res and right
        return res

    def expr_not(self, env):
        if self.match('!'):
            return not self.expr_not(env)
        return self.factor(env)

    def factor(self, env):
        tok = self.peek()
        if tok in ('a', 'b', 'c', 'd', 'e'):
            self.pos += 1
            return env.get(tok, False)
        elif self.match('('):
            res = self.expr_equivalence(env)
            if not self.match(')'):
                raise ValueError("Ошибка: пропущена закрывающая скобка")
            return res
        raise ValueError(f"Неожиданный токен: {tok}")

def term_to_str(term, vars_list, is_cnf=False):
    """Превращает двоичную маску (напр. '10-') в строковое представление интервала"""
    parts = []
    for i, char in enumerate(term):
        if char == '-':
            continue
        if is_cnf:
            parts.append(vars_list[i] if char == '0' else f"!{vars_list[i]}")
        else:
            parts.append(vars_list[i] if char == '1' else f"!{vars_list[i]}")
    
    sep = " | " if is_cnf else " & "
    if not parts:
        return "1" if not is_cnf else "0"
    res = sep.join(parts)
    return f"({res})" if len(parts) > 1 else res


def quine_mccluskey(num_vars, target_indices, vars_list, is_cnf=False):
    """Реализация алгоритма Куайна — Мак-Класки с логированием шагов склеивания"""
    if not target_indices:
        return [], [], {}
    
    current_terms = {format(idx, f'0{num_vars}b'): {idx} for idx in target_indices}
    all_prime_implicants = {}
    stage = 1
    
    print(f"\n   --- Этап склеивания ({'КНФ' if is_cnf else 'ДНФ'}) ---")
    
    while current_terms:
        terms_list = sorted(list(current_terms.keys()))
        next_terms = {}
        combined = set()
        glued_pairs = []
        
        for i in range(len(terms_list)):
            for j in range(i + 1, len(terms_list)):
                t1, t2 = terms_list[i], terms_list[j]
                diff = 0
                diff_idx = -1
                for k in range(num_vars):
                    if t1[k] != t2[k]:
                        diff += 1
                        diff_idx = k
                if diff == 1 and t1[diff_idx] != '-' and t2[diff_idx] != '-':
                    combined.add(t1)
                    combined.add(t2)
                    new_term = t1[:diff_idx] + '-' + t1[diff_idx+1:]
                    new_covered = current_terms[t1].union(current_terms[t2])
                    if new_term in next_terms:
                        next_terms[new_term] = next_terms[new_term].union(new_covered)
                    else:
                        next_terms[new_term] = new_covered
                    glued_pairs.append((t1, t2, new_term))
        
        if glued_pairs:
            print(f"    Раунд {stage}:")
            seen_pairs = set()
            for t1, t2, nt in glued_pairs:
                pair_id = tuple(sorted([t1, t2]))
                if pair_id not in seen_pairs:
                    seen_pairs.add(pair_id)
                    print(f"      {term_to_str(t1, vars_list, is_cnf)} ∨ {term_to_str(t2, vars_list, is_cnf)} => {term_to_str(nt, vars_list, is_cnf)}")
        
        for t in current_terms:
            if t not in combined:
                all_prime_implicants[t] = current_terms[t]
                
        current_terms = next_terms
        stage += 1

    pi_keys = list(all_prime_implicants.keys())
    
    best_cover = []
    for r in range(1, len(pi_keys) + 1):
        found = False
        for comb in itertools.combinations(pi_keys, r):
            covered = set()
            for k in comb:
                covered = covered.union(all_prime_implicants[k])
            if covered == set(target_indices):
                best_cover = list(comb)
                found = True
                break
        if found:
            break
            
    return pi_keys, best_cover, all_prime_implicants


def print_coverage_table(prime_implicants, target_indices, pi_dict, vars_list, is_cnf):
    """Вывод таблицы покрытия импликантами (Импликантная матрица)"""
    if not target_indices:
        return
    header = f" {'Импликанта':<20} | " + " | ".join(f"{idx:^4}" for idx in target_indices)
    print("    " + header)
    print("    " + "-" * len(header))
    for pi in prime_implicants:
        pi_str = term_to_str(pi, vars_list, is_cnf)
        covered_indices = pi_dict[pi]
        row_cells = []
        for idx in target_indices:
            row_cells.append(" X  " if idx in covered_indices else "    ")
        print(f"    {pi_str:<20} | " + " | ".join(row_cells))


def print_karnaugh_map(vars_list, outputs):
    """Вывод Карты Карно для функций от 2 до 5 переменных"""
    n = len(vars_list)
    if n < 2:
        print("    [Карта Карно тривиальна или избыточна для < 2 переменных]")
        return
    
    num_row_vars = n // 2
    num_col_vars = n - num_row_vars
    
    row_vars = vars_list[:num_row_vars]
    col_vars = vars_list[num_row_vars:]
    
    gray_codes = {
        1: ['0', '1'],
        2: ['00', '01', '11', '10'],
        3: ['000', '001', '011', '010', '110', '111', '101', '100']
    }
    
    r_codes = gray_codes[num_row_vars]
    c_codes = gray_codes[num_col_vars]
    
    row_title = "".join(row_vars)
    col_title = "".join(col_vars)
    
    header = f"  {row_title}\\{col_title}".ljust(10) + " | " + " | ".join(c_codes)
    print("    " + header)
    print("    " + "-" * len(header))
    
    for r in r_codes:
        row_str = f"  {r}".ljust(10) + " | "
        vals = []
        for c in c_codes:
            full_bin = r + c
            idx = int(full_bin, 2)
            vals.append(f" {outputs[idx]} ")
        print("    " + row_str + " | ".join(vals))

def main():
    print("=== Лабораторная работа №2 ===")
    user_expr = input("Введите логическую функцию (например, !(!a->!b) | c): ").strip()
    if not user_expr:
        user_expr = "!(!a->!b) | c"
        print(f"Используется тестовая функция по умолчанию: {user_expr}")

    all_possible_vars = ['a', 'b', 'c', 'd', 'e']
    found_vars = sorted(list(set(re.findall(r'[a-e]', user_expr.lower()))))
    
    if not found_vars:
        print("Ошибка: В выражении не найдено переменных (a, b, c, d, e).")
        return
        
    print(f"Обнаруженные переменные: {', '.join(found_vars)}")
    n = len(found_vars)

    parser = BooleanParser(user_expr)
    
    table_rows = []
    outputs = []
    
    for combination in itertools.product([0, 1], repeat=n):
        env = dict(zip(found_vars, combination))
        try:
            res = 1 if parser.parse(env) else 0
        except Exception as e:
            print(f"Ошибка при вычислении выражения: {e}")
            return
        table_rows.append((combination, res))
        outputs.append(res)

    ones_indices = [i for i, r in enumerate(outputs) if r == 1]
    zeros_indices = [i for i, r in enumerate(outputs) if r == 0]

    print("\n2) Таблица истинности функции:")
    header = " | ".join(found_vars) + " |  F"
    print("   " + header)
    print("   " + "-" * len(header))
    for comb, res in table_rows:
        comb_str = " | ".join(str(x) for x in comb)
        print(f"   {comb_str} |  {res}")

    print("\n3) Совершенные формы:")
    
    sdnf_parts = []
    for comb, res in table_rows:
        if res == 1:
            part = " & ".join(found_vars[i] if comb[i] == 1 else f"!{found_vars[i]}" for i in range(n))
            sdnf_parts.append(f"({part})")
    sdnf_str = " | ".join(sdnf_parts) if sdnf_parts else "0"
    print(f"   СДНФ: {sdnf_str}")
    
    sknf_parts = []
    for comb, res in table_rows:
        if res == 0:
            part = " | ".join(found_vars[i] if comb[i] == 0 else f"!{found_vars[i]}" for i in range(n))
            sknf_parts.append(f"({part})")
    sknf_str = " & ".join(sknf_parts) if sknf_parts else "1"
    print(f"   СКНФ: {sknf_str}")

    print("\n4) Числовая форма:")
    print(f"   СДНФ: ⋁ v({', '.join(map(str, ones_indices))})")
    print(f"   СКНФ: ⋀ K({', '.join(map(str, zeros_indices))})")

    bin_index = "".join(str(x) for x in outputs)
    dec_index = int(bin_index, 2)
    print(f"\n5) Индексная форма функции: {dec_index} (двоичный вектор: {bin_index})")

    print("\n6) Проверка принадлежности к классам Поста:")
    
    t0 = (outputs[0] == 0)
    t1 = (outputs[-1] == 1)
    s = all(outputs[i] != outputs[(1 << n) - 1 - i] for i in range(1 << n))
    m = True
    for i in range(1 << n):
        for j in range(1 << n):
            if (i & j) == i:
                if outputs[i] > outputs[j]:
                    m = False
                    break
        if not m: break
        
    coefs = list(outputs)
    for i in range(n):
        bit = 1 << i
        for j in range(1 << n):
            if j & bit:
                coefs[j] = coefs[j] ^ coefs[j ^ bit]
                
    l = True
    for j in range(1 << n):
        if bin(j).count('1') > 1 and coefs[j] == 1:
            l = False
            break

    print(f"   Класс T0 (сохранение нуля):     {'Да' if t0 else 'Нет'}")
    print(f"   Класс T1 (сохранение единицы):  {'Да' if t1 else 'Нет'}")
    print(f"   Класс S  (самодвойственность):  {'Да' if s else 'Нет'}")
    print(f"   Класс M  (монотонность):        {'Да' if m else 'Нет'}")
    print(f"   Класс L  (линейность):          {'Да' if l else 'Нет'}")

    print("\n7) Построение Полинома Жегалкина:")
    zhegalkin_parts = []
    if coefs[0] == 1:
        zhegalkin_parts.append("1")
    for j in range(1, 1 << n):
        if coefs[j] == 1:
            mono = []
            for b in range(n):
                if j & (1 << (n - 1 - b)):
                    mono.append(found_vars[b])
            zhegalkin_parts.append("".join(mono))
    zhegalkin_str = " ⊕ ".join(zhegalkin_parts) if zhegalkin_parts else "0"
    print(f"   P(x) = {zhegalkin_str}")

    print("\n8) Поиск фиктивных переменных:")
    fictitious_vars = []
    for b in range(n):
        bit = 1 << (n - 1 - b)
        is_fict = True
        for j in range(1 << n):
            if (j & bit) == 0:
                if outputs[j] != outputs[j | bit]:
                    is_fict = False
                    break
        if is_fict:
            fictitious_vars.append(found_vars[b])
            
    if fictitious_vars:
        print(f"   Фиктивные переменные: {', '.join(fictitious_vars)}")
    else:
        print("   Фиктивных переменных нет (все переменные существенны).")

    print("\n9) Булева дифференциация (Частные производные):")
    for b in range(n):
        bit = 1 << (n - 1 - b)
        deriv_outputs = []
        for j in range(1 << n):
            deriv_outputs.append(outputs[j & ~bit] ^ outputs[j | bit])
        
        d_coefs = list(deriv_outputs)
        for i in range(n):
            dbit = 1 << i
            for j in range(1 << n):
                if j & dbit: d_coefs[j] = d_coefs[j] ^ d_coefs[j ^ dbit]
        d_parts = []
        if d_coefs[0] == 1: d_parts.append("1")
        for j in range(1, 1 << n):
            if d_coefs[j] == 1:
                mono = [found_vars[x] for x in range(n) if j & (1 << (n - 1 - x))]
                d_parts.append("".join(mono))
        d_str = " ⊕ ".join(d_parts) if d_parts else "0"
        print(f"   dF / d{found_vars[b]} = {d_str}")

    if n >= 2:
        print("   Смешанная производная:")
        bit1 = 1 << (n - 1)
        bit2 = 1 << (n - 2)
        mixed_outputs = []
        for j in range(1 << n):
            val00 = outputs[j & ~bit1 & ~bit2]
            val01 = outputs[(j & ~bit1) | bit2]
            val10 = outputs[(j | bit1) & ~bit2]
            val11 = outputs[j | bit1 | bit2]
            mixed_outputs.append(val00 ^ val01 ^ val10 ^ val11)
        print(f"   d²F / (d{found_vars[0]} d{found_vars[1]}) Вектор: {''.join(map(str, mixed_outputs))}")

    
    pi_dnf, cover_dnf, pi_dict_dnf = quine_mccluskey(n, ones_indices, found_vars, is_cnf=False)
    pi_cnf, cover_cnf, pi_dict_cnf = quine_mccluskey(n, zeros_indices, found_vars, is_cnf=True)

    print("\n10) Результат минимизации расчетным методом:")
    min_dnf_str = " ∨ ".join(term_to_str(pi, found_vars, is_cnf=False) for pi in cover_dnf) if cover_dnf else "0"
    min_cnf_str = " & ".join(term_to_str(pi, found_vars, is_cnf=True) for pi in cover_cnf) if cover_cnf else "1"
    print(f"    Минимизированная ДНФ: {min_dnf_str}")
    print(f"    Минимизированная КНФ: {min_cnf_str}")

    print("\n11) Расчетно-табличный метод (Импликантные матрицы):")
    print("   Таблица покрытия для ДНФ:")
    print_coverage_table(pi_dnf, ones_indices, pi_dict_dnf, found_vars, is_cnf=False)
    print("\n   Таблица покрытия для КНФ:")
    print_coverage_table(pi_cnf, zeros_indices, pi_dict_cnf, found_vars, is_cnf=True)

    print("\n12) Минимизация табличным методом (Карта Карно):")
    print_karnaugh_map(found_vars, outputs)
    print(f"\nВывод на основе выделенных областей склеивания:")
    print(f"  МДНФ = {min_dnf_str}")
    print(f"  МКНФ = {min_cnf_str}")


if __name__ == '__main__':
    main()