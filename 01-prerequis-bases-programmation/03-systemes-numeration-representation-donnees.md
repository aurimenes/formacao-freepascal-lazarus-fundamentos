🔝 Voltar para [Resumo](/SUMMARY.md)

# 1.3 Sistemas Numéricos e Representação de Dados

## Introdução

Nesta seção, exploraremos como os computadores representam e armazenam informações. Entender os sistemas numéricos é essencial para entender o funcionamento interno dos programas e otimizar seu código.

## Sistemas Numéricos

### O Sistema Decimal (Base 10)

Este é o sistema que usamos todos os dias.

**Características:**
- Utiliza 10 dígitos: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9
- Cada posição representa uma potência de 10

**Exemplo: Decomposição do número 2543**
```
2543 = (2 × 10³) + (5 × 10²) + (4 × 10¹) + (3 × 10⁰)
= (2 × 1000) + (5 × 100) + (4 × 10) + (3 × 1)
= 2000 + 500 + 40 + 3
= 2543
```

Cada posição tem um peso que é uma potência de 10:
- Posição 1 (unidades): 10⁰ = 1
- Posição 2 (dezenas): 10¹ = 10
- Posição 3 (centenas): 10² = 100
- Posição 4 (milhares): 10³ = 1000

### O sistema binário (base 2)

Este é o sistema usado pelos computadores.

**Características:**
- Utiliza apenas dois dígitos: 0 e 1
- Cada dígito é chamado de **bit** (dígito binário)
- Cada posição representa uma potência de 2

**Por que binário?**
Computadores usam eletricidade, que tem dois estados:
- Corrente fluindo = 1
- Corrente não fluindo = 0

**Exemplo: o número binário 1011**
```
1011 = (1 × 2³) + (0 × 2²) + (1 × 2¹) + (1 × 2⁰)
= (1 × 8) + (0 × 4) + (1 × 2) + (1 × 1)
= 8 + 0 + 2 + 1
= 11 em decimal
```

Potências de 2 para saber:
```
2⁰ = 1
2¹ = 2
2² = 4
2³ = 8
2⁴ = 16
2⁵ = 32
2⁶ = 64
2⁷ = 128
2⁸ = 256
2⁹ = 512
2¹⁰ = 1024
```

### Conversão Decimal → Binária

**Método de Divisão por 2:**

Converter 13 para binário:
```
13 ÷ 2 = 6 resto 1 ← bit de ordem inferior (direita)
6 ÷ 2 = 3 resto 0
3 ÷ 2 = 1 resto 1
1 ÷ 2 = 0 resto 1 ← bit de ordem superior (esquerda)

Resultado: 1101
```

**Verificação:**
```
1101 = (1×8) + (1×4) + (0×2) + (1×1) = 8 + 4 + 0 + 1 = 13 ✓
```

### Conversão de Binário → Decimal

**Método das Potências de 2:**

Vamos converter 10110 para decimal:
```
Posição: 4 3 2 1 0
Bit: 1 0 1 1 0
Peso: 16 8 4 2 1

Cálculo: (1×16) + (0×8) + (1×4) + (1×2) + (0×1)
= 16 + 0 + 4 + 2 + 0
= 22
```

### O Sistema Hexadecimal (Base 16)

O Sistema Hexadecimal é amplamente utilizado na computação como uma notação compacta para binário.

**Características:**
- Utiliza 16 símbolos: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, A, B, C, D, E, F
- A = 10, B = 11, C = 12, D = 13, E = 14, F = 15
- Cada posição representa uma potência de 16
- Prefixo comum: `0x` ou `$` (em Pascal)

**Por que hexadecimal?**
4 bits = 1 dígito hexadecimal, por isso é mais compacto que o binário:
```
Binário: 1111 0101 1010 0011
Hexadecimal: F 5 A 3
```

**Correspondência binário-hexadecimal:**
```
Decimal | Binário | Hexadecimal
0 | 0000 | 0
1 | 0001 | 1
2 | 0010 | 2
3 | 0011 | 3
4 | 0100 | 4
5 | 0101 | 5
6 | 0110 | 6
7 | 0111 | 7
8 | 1000 | 8
9 | 1001 | 9
10 | 1010 | TEM
11 | 1011 | B
12 | 1100 | C
13 | 1101 | D
14 | 1110 | E
15 | 1111 | F
```

**Exemplo: 2A3F em hexadecimal**
```
2A3F = (2 × 16³) + (10 × 16²) + (3 × 16¹) + (15 × 16⁰)
= (2 × 4096) + (10 × 256) + (3 × 16) + (15 × 1)
= 8192 + 2560 + 48 + 15
= 10815 em decimal
```

### O sistema octal (base 8)

Menos usado hoje em dia, mas ainda presente em certos contextos.

**Características:**
- Utiliza 8 dígitos: 0, 1, 2, 3, 4, 5, 6, 7
- Cada posição representa uma potência de 8
- 3 bits = 1 dígito octal
- Prefixo comum: `0` (no início do número)

**Exemplo: 157 em octal**
```
157 (octal) = (1 × 8²) + (5 × 8¹) + (7 × 8⁰)
= (1 × 64) + (5 × 8) + (7 × 1)
= 64 + 40 + 7
= 111 em decimal
```

## Representando Inteiros

### Inteiros sem Sinal (somente positivos)

Em 8 bits (1 byte), podemos representar de 0 a 255 :
```
00000000 = 0
00000001 = 1
00000010 = 2
...
11111110 = 254
11111111 = 255
```

**Fórmula:** Com n bits, podemos representar 2 valores diferentes.

**Exemplos:**
- 8 bits: 2⁸ = 256 valores (0 a 255)
- 16 bits: 2¹⁶ = 65.536 valores (0 a 65.535)
- 32 bits: 2³² = 4.294.967.296 valores (0 a 4.294.967.295)

### Inteiros com sinal (positivos e negativos)

Para representar números negativos, usamos o **complemento de dois**.

**Princípio:**
- O bit mais significativo (mais à esquerda) indica o sinal.
- 0 = positivo, 1 = negativo.

**Em 8 bits com sinal:**
- Intervalo: -128 a +127
- `0111 1111` = +127 (maior)
- `0000 0000` = 0
- `1000 0000` = -128 (menor)

**Como representar -5 em 8 bits?**

Método do complemento de dois:
1. Escreva 5 em binário: `0000 0101`
2. Inverta todos os bits: `1111 1010`
3. Some 1: `1111 1011`

Portanto, -5 = `1111 1011`

**Verificação:**
Se somarmos 5 e -5:
```
0000 0101 (5)
+ 1111 1011 (-5)
-----------
0000 0000 (0) ✓
```

### Estouro

O que acontece se excedermos a capacidade?

**Exemplo com 8 bits sem sinal:**
```
1111 1111 (255)
+ 1 (+1)
-----------
0000 0000 (0) ← O resultado estoura!
```

Isso é chamado de **estouro** ou **transbordamento**. O 9º bit é perdido.

**Aviso:** Em programação, você deve sempre escolher o tipo de variável apropriado para o intervalo de valores esperado.

## Representando Números de Ponto Flutuante

### Números de Ponto Fixo

Simples, mas limitado: fixamos o número de casas decimais.

**Exemplo:** em 16 bits com 2 casas decimais
- O número 123,45 é armazenado como 12345
- Dividimos mentalmente por 100

### Números de Ponto Flutuante (padrão IEEE 754)

Este é o método mais comum, usado para os tipos `Real`, `Simples` e `Duplo`.

**Princípio:** Notação científica em binário
```
Número = Sinal × Mantissa × 2^Expoente
```

**Formato de precisão simples (32 bits):**
```
| Sinal | Expoente | Mantissa |
| 1 bit | 8 bits | 23 bits |
```

**Formato de precisão dupla (64 bits):**
```
| Sinal | Expoente | Mantissa |
| 1 bit | 11 bits | 52 bits |
```

**Exemplo simplificado:** Representar 6,5
1. Em binário: 6,5 = 110,1
2. Normalizar: 1,101 × 2²
3. Armazenar: sinal (0), expoente (2), mantissa (101)

**Limitações importantes:**
- Precisão limitada (aproximadamente 7 dígitos para `Single`, 15 para `Double`)
- Alguns números decimais não podem ser representados exatamente
- Exemplo: 0,1 não tem representação exata em binário

**Consequência:** Evite comparações de igualdade estritas com floats
```pascal
// Ruim
se (x = 0,1) então ...

// Bom
se (Abs(x - 0,1) < 0,0001) então ...
```

## Representação de caracteres

### A tabela ASCII

ASCII (American Standard Code for Information Interchange) é o padrão histórico.

**Características:**
- Utiliza 7 bits (128 caracteres)
- Extensão de 8 bits (256 caracteres)

**Principais Intervalos ASCII:**
```
Decimal | Hexadecimal | Caractere
0-31 | 00-1F | Caracteres de Controle
32 | 20 | Espaço
48-57 | 30-39 | Dígitos '0' a '9'
65-90 | 41-5A | Letras Maiúsculas 'A' a 'Z'
97-122 | 61-7A | Letras minúsculas de 'a' a 'z'
```

**Exemplos de código ASCII:**
```
'A' = 65 (decimal) = 41 (hex) = 01000001 (binário)
'B' = 66
'a' = 97
'0' = 48 ← O caractere '0', não o número 0
' ' = 32 (espaço)
```

**Caracteres de controle comuns:**
```
0 = NUL (Nulo)
7 = BEL (Bip)
8 = BS (Backspace)
9 = TAB (Tabulação)
10 = LF (Avanço de Linha - Nova Linha Unix/Linux)
13 = CR (Retorno de Carro)
27 = ESC (Escape)
```

### ASCII Estendido

Os códigos 128-255 variam de acordo com a página de código:
- Página 437: IBM PC original
- Página 850: Europa Ocidental
- Página 1252: Windows Europa Ocidental

**Problema:** Incompatibilidade entre sistemas

### Unicode e UTF-8

Unicode é o padrão moderno para representar todos os alfabetos do mundo.

**Recursos:**
- Pode representar mais de um milhão de caracteres
- Inclui todos os alfabetos: latino, cirílico, árabe, chinês, emojis, etc.
- UTF-8 é a codificação mais comum

**UTF-8:**
- Usa de 1 a 4 bytes por caractere
- Compatível com ASCII (os primeiros 128 caracteres são idênticos)
- O tamanho varia dependendo do caractere

**Exemplos de UTF-8:**
```
'A' = 1 byte: 01000001
'é' = 2 bytes: 11000011 10101001
'€' = 3 bytes: 11100010 10000010 10101100
'😀' = 4 bytes: 11110000 10011111 10011000 10000000
```

**Em Pascal/Lazarus:**
- Tipo `AnsiString`: codificação ANSI/ASCII
- Tipo `UTF8String`: codificação UTF-8
- Tipo `UnicodeString`: codificação UTF-16

## Representações de String

### Strings de Comprimento Fixo

**ShortString em Pascal:**
- Comprimento máximo: 255 caracteres
- O primeiro byte contém o comprimento
- Os caracteres seguem

```
Exemplo: 'Hello'
[7]['B']['o']['n']['j']['o']['u']['r'][...bytes não utilizados...]
```

### Strings de Comprimento Variável

**String (AnsiString/UnicodeString):**
- Comprimento dinâmico
- Gerenciamento automático de memória
- Terminada por um caractere nulo (#0) para compatibilidade com C

### Strings Nulas (Estilo C)

Na linguagem C e em APIs de sistema:
- Matriz de caracteres
- Terminação nula (código 0)
- Sem armazenamento de comprimento

```
'Hello' em C: ['H']['e']['l']['l']['o'][0]
```

## Representação booleana

Um booleano só pode ter dois valores: TRUE ou FALSE.

**Armazenamento:**
- Teoricamente: 1 bit é suficiente
- Na prática: geralmente 1 byte (8 bits) por questões de endereçamento

**Em Pascal:**
- Tipo `Booleano`: 1 byte
- `False` = 0
- `True` = 1 (mas qualquer valor diferente de zero pode ser considerado verdadeiro)

## Unidades de medida em computação

### Bits e bytes

```1 bit = 0 ou 1
1 byte = 8 bits
1 palavra = 2 bytes (16 bits) ou 4 bytes (32 bits), dependendo da arquitetura
1 palavra dupla = 4 bytes (32 bits)
1 palavra quádrupla = 8 bytes (64 bits)
```

### Prefixos de Quantidade

**Prefixos Binários (base 2):**
```
1 KiB (Kibibyte) = 1024 bytes = 2¹⁰ bytes
1 MiB (Mebibyte) = 1024 KiB = 2²⁰ bytes
1 GiB (Gibibyte) = 1024 MiB = 2³⁰ bytes
1 TiB (Tebibyte) = 1024 GiB = 2⁴⁰ bytes
```

**Prefixos Decimais (base 10):**
```
1 KB (Kilobyte) = 1000 bytes = 10³ bytes
1 MB (Megabyte) = 1000 KB = 10⁶ bytes
1 GB (Gigabyte) = 1000 MB = 10⁹ bytes
1 TB (Terabyte) = 1000 GB = 10¹² bytes
```

**Atenção:** Confusão comum!
- Fabricantes de discos rígidos: usam prefixos decimais
- Sistemas operacionais: frequentemente usam prefixos binários
- Um disco de "1 TB" exibe aproximadamente 931 GiB no Windows

## Ordem dos Bytes (Endianness)

Quando um número ocupa vários bytes, em que ordem eles são armazenados?

### Little Endian (Intel x86, AMD64)

O byte menos significativo é armazenado primeiro.

**Exemplo: o número 0x12345678 (305.419.896)**
```
Endereço: 0x00 0x01 0x02 0x03
Valor: 0x78 0x56 0x34 0x12
```

### Big Endian (Motorola, PowerPC, Rede)

O byte mais significativo é armazenado primeiro.

**Exemplo: o mesmo número 0x12345678**
```
Endereço: 0x00 0x01 0x02 0x03
Valor: 0x12 0x34 0x56 0x78
```

**Impacto:** Importante ao se comunicar em rede ou ler arquivos binários criados em outros sistemas.

## Operações Bitwise

### Operadores Bitwise

Estes operadores funcionam diretamente na representação binária:

**AND (E lógico) - `e`**
```
1010
& 1100
------
1000
```
Resultado: 1 somente se ambos os bits forem 1.

**OR (OU lógico) - `ou`**
```
1010
| 1100
------
1110
```
Resultado: 1 se pelo menos um dos dois bits for 1.

**XOR (OU exclusivo) - `xor`**
```
1010
^ 1100
------
0110
```
Resultado: 1 se os dois bits forem diferentes.

**NOT (complemento) - `não`**
```
~ 1010
------
0101
```
Inverte todos os bits.

### Deslocamentos de Bits

**Deslocamento para a Esquerda (SHL)**
```
1011 shl 1 = 0110 (equivalente a multiplicar por 2)
1011 shl 2 = 1100 (equivalente a multiplicar por 4)
```

**Deslocamento para a Direita (SHR)**
```
1100 shr 1 = 0110 (equivalente a dividir por 2)
1100 shr 2 = 0011 (equivalente a dividir por 4)
```

**Usos:**
- Otimização (multiplicação/divisão por potências de 2)
- Extração de bits específicos
- Manipulação de sinalizadores

## Aplicação Prática: Cores RGB

As cores na tela são frequentemente representadas em RGB (Vermelho, Verde, Azul).

**Formato de 24 bits (True Color):**
```
| Vermelho | Verde | Azul |
| 8 bits | 8 bits | 8 bits |
```

Cada componente varia de 0 a 255.

**Exemplos em hexadecimal:**
```
#FF0000 = Vermelho Puro (255, 0, 0)
#00FF00 = Verde Puro (0, 255, 0)
#0000FF = Azul Puro (0, 0, 255)
#FFFFFF = Branco (255, 255, 255)
#000000 = Preto (0, 0, 0)
#FF00FF = Magenta (255, 0, 255)
#FFFF00 = Amarelo (255, 255, 0)
#808080 = Cinza (128, 128, 128)
```

## Conclusão

Agora você tem uma sólida compreensão dos sistemas numéricos e da representação de dados na computação.

**Principais Conclusões:**
- Computadores usam o sistema binário (base 2)
- Hexadecimal (base 16) é uma forma compacta do binário
- Inteiros podem ser com ou sem sinal
- Números de ponto flutuante usam a notação IEEE 754
- Caracteres são codificados em ASCII ou Unicode (UTF-8)
- Cada tipo de dado tem uma representação específica na memória
- Operações bit a bit permitem manipular bits diretamente

Essa compreensão ajudará você a:
- Escolher os tipos de dados corretos
- Entender limites e precisão
- Otimizar seu código
- Depurar problemas sutis
- Trabalhar com arquivos binários e protocolos de rede

Na próxima seção, discutiremos lógica booleana e tabelas verdade, essenciais para a compreensão de condicionais e testes em programação.

⏭️ [Lógica booleana e tabelas verdade](/01-prerequisites-programming-bases/04-logic-boolean-truth-tables.md)