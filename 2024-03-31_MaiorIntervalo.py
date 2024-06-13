# scanear 2 numeros
x = int(input())
y = int(input())


def isPrimo(x: int, i:int) -> bool:
    '''
    Funcao que verifica se um numero eh primo.

    Args:
        x (int): Numero a ser verificado.
        i (int): Divisor. Deve ser inicializado como 2.

    Returns:
        bool: True se x for primo, False caso contrario.
    '''

    # nao existem primos menores ou iguais a 1
    if x <= 1:
        return False
    # x eh divisivel por i, logo nao eh primo
    if x % i == 0:
        return False
    # checamos ate a raiz quadrada de n
    if i * i > x:
        return True
    return isPrimo(x, i+1)


def listaPrimos(lista: list[int], x: int, y: int) -> list[int]:
    '''
    Funcao que retorna uma lista com todos os primos entre x e y.

    Args:
        lista (list[int]): Lista de primos. Deve ser inicializada como uma lista vazia.
        x (int): Limite inferior.
        y (int): Limite superior.

    Returns:
        list[int]: Lista de primos entre x e y.
    '''
    if x > y:
        return lista
    
    if isPrimo(x, 2):
        lista.append(x)

    return listaPrimos(lista, x+1, y)


def maiorIntervaloEntrePrimos(listaPrimos: list[int], intervalo: int) -> int:
    '''
    Funcao que retorna o maior intervalo entre dois primos da lista.

    Args:
        listaPrimos (list[int]): Lista de primos.
        intervalo (int): Maior intervalo entre dois primos. Deve ser inicializado como 0.

    Returns:
        int: Maior intervalo entre dois primos da lista
    '''
    
    if listaPrimos[1:] == []:
        return intervalo
    
    if listaPrimos[1] - listaPrimos[0] > intervalo:
        return maiorIntervaloEntrePrimos(listaPrimos[1:], listaPrimos[1] - listaPrimos[0])
    
    return maiorIntervaloEntrePrimos(listaPrimos[1:], intervalo)


def maiorIntervaloEntrePrimosMaiorQueXMenorQueY(x: int, y: int) -> int:
    '''
    Funcao que retorna o maior intervalo entre dois primos entre x e y.

    Args:
        x (int): Limite inferior.
        y (int): Limite superior.

    Returns:
        int: Maior intervalo entre dois primos entre x e y.
    '''
    return maiorIntervaloEntrePrimos(listaPrimos([], x, y), 0)
        

print(maiorIntervaloEntrePrimosMaiorQueXMenorQueY(x, y))