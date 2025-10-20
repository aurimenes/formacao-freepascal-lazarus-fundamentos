🔝 Voltar para [Resumo](/SUMMARY.md)

# 1.5 Algoritmos e Pseudocódigo

## Introdução

Antes de escrever código em uma linguagem de programação, é essencial considerar a lógica por trás da resolução do problema. É aqui que entram os algoritmos e o pseudocódigo, ferramentas fundamentais para qualquer programador.

## O que é um algoritmo?

### Definição

Um **algoritmo** é uma sequência finita e ordenada de instruções claras e precisas para resolver um problema ou realizar uma tarefa.

**Características Essenciais:**
- **Finito**: O algoritmo deve ser concluído após um número finito de etapas
- **Definido**: Cada etapa deve ser precisa e inequívoca
- **Eficiente**: Cada etapa deve ser viável
- **Entradas**: Dados necessários no início
- **Saídas**: Resultados produzidos no final

### Algoritmos no Dia a Dia

Usamos algoritmos todos os dias sem perceber:

**Algoritmo para fazer café:**
```
1. Encha o tanque de água
2. Coloque um filtro no porta-filtro
3. Adicione café moído ao filtro
4. Coloque a cafeteira sob o porta-filtro
5. Ligue a máquina
6. Espere o café sair
7. Desligue a máquina
8. Sirva o café
```

**Algoritmo para ir ao trabalho:**
```
1. Sair de casa
2. Caminhar até o ponto de ônibus
3. Esperar o ônibus 12
4. Embarcar no Ônibus
5. Desça no ponto "Centre-ville"
6. Caminhe até o escritório
```

### Algoritmos em Ciência da Computação

Em programação, algoritmos descrevem como transformar dados de entrada em resultados desejados.

**Exemplo: Calculando a média de três notas**
```
Entradas: nota1, nota2, nota3
Processamento: média = (nota1 + nota2 + nota3) / 3
Saída: média
```

## Pseudocódigo

### O que é pseudocódigo?

**Pseudocódigo** é uma maneira de escrever um algoritmo usando uma linguagem informal, um meio-termo entre a linguagem natural e uma linguagem de programação.

**Vantagens do pseudocódigo:**
- Independente de linguagem de programação
- Mais fácil de ler do que código real
- Permite que você se concentre na lógica, não na sintaxe
- Facilita a comunicação entre desenvolvedores
- Serve como documentação

**Desvantagens:**
- Não pode ser executado diretamente por um computador
- Sem sintaxe padronizada (varia de acordo com o autor)

### Por que usar pseudocódigo?

**Antes de Programar:**
- Esclareça seu pensamento
- Detecte erros lógicos antecipadamente
- Planeje a estrutura do programa

**Para se comunicar:**
- Explique sua solução para outras pessoas
- Documente sua abordagem
- Discuta possíveis melhorias

## Convenções de Pseudocódigo

Não existe um padrão único, mas aqui estão algumas convenções comuns:

### Instruções Básicas

**Atribuição (atribuir um valor a uma variável):**
```
variável ← valor
número ← 10
nome ← "Alice"
```

**Exibir (mostrar algo ao usuário):**
```
EXIBIR "mensagem"
EXIBIR variável
EXIBIR "A soma é: ", soma
```

**Entrada (solicitar informações ao usuário):**
```
GET variável
ENTER idade
ASK Nome
```

**Comentários:**
```
// Este é um comentário de uma única linha
/* Este é um Comentário multilinha
*/
```

### Estrutura de Seleção (Condição)

**SE ... ENTÃO ... SENÃO**
```
SE condição ENTÃO
instruções se verdadeiro
SENÃO
instruções se falso
FIM SE
```

**Exemplo:**
```
SE idade >= 18 ENTÃO
EXIBIR "Você é adulto"
SENÃO
EXIBIR "Você é menor de idade"
FIM SE
```

**Seleção Múltipla (Escolha):**
```
DE ACORDO COM variável
CASO valor1:
instruções
CASO valor2:
instruções
PADRÃO:
instruções
FIM DE ACORDO COM
```

### Estruturas de Repetição (Loops)

**PARA (número conhecido de iterações):**
```
PARA variável DE início A fim DO
instruções
FIM PARA
```

**Exemplo:**
```
PARA i DE 1 A 10 FAÇA
EXIBIR i
FIM PARA
```

**ENQUANTO (condição no início):**
```
ENQUANTO condição FAÇA
instruções
FIM ENQUANTO
```

**Exemplo:**
```
contador ← 1
ENQUANTO contador <= 10 FAÇA
EXIBIR contador
contador ← contador + 1
FIM ENQUANTO
```

**REPETIR ... ATÉ (condição no final):**
```
REPETIR
instruções
ATÉ condição
```

**Exemplo:**
```
REPETIR
EXIBIR "Digite um número positivo: "
LER número
ATÉ número > 0
```

## Exemplos de algoritmos simples

### Exemplo 1: Calcular a Área de um Retângulo

**Problema:** Calcule a área de um retângulo, dados seu comprimento e largura.

**Pseudocódigo:**
```
ALGORITMO RectangleArea

VARIÁVEIS
comprimento: real
largura: real
área: real

INÍCIO
EXIBIR "Digite o comprimento: "
LER comprimento

EXIBIR "Digite a largura: "
LER largura

área ← comprimento × largura

EXIBIR "A área do retângulo é: ", área
FIM
```

**Em Pascal:**
```pascal
program RectangleArea;
var
comprimento, largura, área: Real;
início
WriteLn('Digite o comprimento: ');
ReadLn(comprimento);

WriteLn('Digite a largura: ');
ReadLn(largura);

área := comprimento * largura;

WriteLn('A área do retângulo é: ', área: 0:2);
fim.
```

### Exemplo 2: Determinando se um número é par ou ímpar

**Problema:** Verifique se um inteiro é par ou ímpar.

**Pseudocódigo:**
```
ALGORITMO ParOuÍmpar

VARIÁVEIS
número: inteiro

INÍCIO
EXIBIR "Digite um inteiro: "
LER número

SE (número módulo 2) = 0 ENTÃO
EXIBIR número, " é par"
SENÃO
EXIBIR número, " é ímpar"
FIM SE
FIM
```

**Em Pascal:**
```pascal
programa ParOuÍmpar;
var
número: Inteiro;
begin
WriteLn('Digite um inteiro: ');
ReadLn(número);

if (número mod 2) = 0 then
WriteLn(número, ' é par')
else
WriteLn(número, ' é ímpar');
end.
```

### Exemplo 3: Calcular a soma dos primeiros N números inteiros

**Problema:** Calcular 1 + 2 + 3 + ... + N

**Pseudocódigo (versão 1 - com loop):**
```
AlgoritmoSomaInteiro

VARIÁVEIS
N: inteiro
soma: inteiro
i: inteiro

INÍCIO
EXIBIR "Digite N: "
LER N

soma ← 0

PARA i DE 1 ATÉ N FAÇA
soma ← soma + i
FIM PARA

EXIBIR "A soma é: ", soma
FIM
```

**Pseudocódigo (versão 2 - com fórmula matemática):**
```
AlgoritmoSomaInteiro

VARIÁVEIS
N: inteiro
soma: inteiro

INÍCIO
EXIBIR "Digite N: "
LER N

soma ← N × (N + 1) / 2

EXIBIR "A soma é: ", soma
FIM
```

**Observação:** A versão 2 é mais eficiente porque não utiliza um laço!

### Exemplo 4: Encontrando o máximo de três números

**Problema:** Determine o maior de três números.

**Pseudocódigo:**
```
ALGORITMO MaximumThreeNumbers

VARIÁVEIS
a, b, c: real
máximo: real

INÍCIO
EXIBIR "Digite o primeiro número: "
LER a

EXIBIR "Digite o segundo número: "
LER b

EXIBIR "Digite o terceiro número: "
LER c

// Suponha que a seja o máximo
máximo ← a

// Verifique se b é maior
SE b > máximo ENTÃO
máximo ← b
FIM SE

// Verifique se c é maior
SE c > máximo ENTÃO
máximo ← c
FIM SE

EXIBIR "O máximo é: ", máximo
FIM
```

### Exemplo 5: Tabuada

**Problema:** Exiba a tabuada de um número.

**Pseudocódigo:**
```
ALGORITMO DA TABUADA DE MULTIPLICAÇÃO

VARIÁVEIS
número: inteiro
i: inteiro
resultado: inteiro

INÍCIO
EXIBIR "TABUADA DE QUAL NÚMERO?"
LEITURA número

EXIBIR "TABUADA DE ", número, " :"
EXIBIR "-------------------"

PARA i DE 1 A 10 FAÇA
resultado ← número × i
EXIBIR número, " × ", i, " = ", resultado
FIM PARA
FIM
```

## Algoritmos com validação de entrada

### Exemplo: Solicitar um número positivo

**Problema:** Continue solicitando um número até que ele seja positivo.

**Pseudocódigo:**
```
ALGORITMO AskPositiveNumber

VARIÁVEIS
número: inteiro

INICIAR
REPETIR
EXIBIR "Digite um número positivo: "
LER número

SE número <= 0 ENTÃO
EXIBIR "Erro! O número deve ser positivo."
FIM SE

ATÉ número > 0

EXIBIR "Obrigado, você digitou: ", número
FIM
```

### Exemplo: Menu com opções

**Pseudocódigo:**
```
ALGORITMO Menu Principal

VARIÁVEIS
escolha: inteiro

INICIAR
REPETIR
EXIBIR "=== MENU PRINCIPAL ==="
EXIBIR "1. Opção 1"
EXIBIR "2. Opção 2"
EXIBIR "3. Opção 3"
EXIBIR "0. Sair"
EXIBIR "Sua escolha: "
LER opções

BASEADO EM opções
CASO 1:
EXIBIR "Você escolheu a opção 1"
CASO 2:
EXIBIR "Você escolheu a opção 2"
CASO 3:
EXIBIR "Você escolheu a opção 3"
CASO 0:
EXIBIR "Adeus!"
PADRÃO:
EXIBIR "Escolha inválida!"
FIM DE ACORDO COM

ATÉ escolha = 0
FIM
```

## Algoritmos de Busca

### Busca Linear

**Problema:** Descubra se um elemento existe em um array.

**Pseudocódigo:**
```
ALGORITMO DE Busca Linear

VARIÁVEIS
array: array de N inteiros
searchValue: inteiro
i: inteiro
found: booleano
position: inteiro

BEGIN
// Suponha que o array já esteja cheio

EXIBIR "Qual valor você está procurando?"
LEIA valorPesquisar

encontrado ← FALSO
posição ← -1

PARA i DE 0 A N-1 FAÇA
SE array[i] = valorPesquisar ENTÃO
encontrado ← VERDADEIRO
posição ← i
SAÍDA DO LOOP
FIM SE
FIM PARA

SE encontrado ENTÃO
EXIBIR "Valor encontrado na posição ", posição
SENÃO
EXIBIR "Valor não encontrado"
FIM SE
FIM
```

## Algoritmos de Ordenação (Introdução)

### Ordenação por Seleção (princípio simplificado)

**Problema:** Ordene um array de números em ordem crescente.

**Pseudocódigo Simplificado:**
```
ALGORITMO SortSelection

VARIÁVEIS
matriz: matriz de N inteiros
i, j: inteiro
índicemin: inteiro
temp: inteiro

INÍCIO
// Para cada posição
PARA i DE 0 A N-2 FAÇA
// Encontre o mínimo na parte não ordenada
índicemin ← i

PARA j DE i+1 A N-1 FAÇA
SE matriz[j] < matriz[índicemin] ENTÃO
índicemin ← j
FIM SE
FIM PARA

// Troca os elementos
SE Índicemin ≠ i ENTÃO
temp ← matriz[i]
matriz[i] ← matriz[índicemin]
matriz[índicemin] ← temp
FIM SE
FIM PARA

EXIBIR "Matriz ordenada!"
FIM
```

## Decomposição em subproblemas

### Uso de procedimentos e funções

Para algoritmos complexos, nós os decompomos em partes menores.

**Exemplo: Calculadora Simples**

**Pseudocódigo:**
```
FUNÇÃO Adição(a, b: real): real
INÍCIO
RETORNA a + b
FIM

FUNÇÃO Subtração(a, b: real): real
INÍCIO
RETORNA a - b
FIM

FUNÇÃO Multiplicação(a, b: real): real
INÍCIO
RETORNA a × b
FIM

FUNÇÃO Divisão(a, b: real): real
INÍCIO
SE b = 0 ENTÃO
EXIBIR "Erro: divisão por zero"
RETORNA 0
SENÃO
RETORNA a / b
FIM SE
FIM

ALGORITMO Calculadora Simples

VARIÁVEIS
a, b: real
operação: caractere
resultado: real

INÍCIO
EXIBIR "Digite o primeiro número: "
LER a

EXIBIR "Digite a operação (+, -, *, /): "
LER operação

EXIBIR "Digite o segundo número: "
LER b

DE ACORDO COM a operação
CASO '+':
Resultado ← Adição(a, b)
CASO '-':
Resultado ← Subtração(a, b)
CASO '*':
Resultado ← Multiplicação(a, b)
CASO '/':
Resultado ← Divisão(a, b)
PADRÃO:
EXIBIR "Operação desconhecida"
RETORNAR
FIM DE ACORDO COM

EXIBIR "Resultado: ", resultado
FIM
```

## Análise de Algoritmos (Noções Básicas)

### Eficiência Algorítmica

Nem todos os algoritmos são criados iguais. Alguns são mais rápidos que outros.

**Critérios de avaliação:**
- **Tempo de execução**: Quanto tempo o algoritmo leva?
- **Uso de memória**: Quanta memória ele requer?
- **Simplicidade**: É fácil de entender e manter?

### Operações de Contagem

**Exemplo 1: Algoritmo Linear**
```
PARA i DE 1 A N DO
IMPRIMA i
FIM PARA
```
Número de operações: N (proporcional ao tamanho da entrada)

**Exemplo 2: Algoritmo Quadrático**
```
PARA i DE 1 A N DO
PARA j DE 1 A N DO
IMPRIMA i, j
FIM PARA
FIM PARA
```
Número de operações: N × N = N² (cresce rapidamente)

**Exemplo 3: Algoritmo Constante**
```
Resultado ← N × (N + 1) / 2
```
Número de operações: apenas algumas operações (independentes de N)

### Notação Big O (Introdução)

Esta é uma notação matemática para descrever a eficiência:

- **O(1)**: Constante - sempre ao mesmo tempo
- **O(log N)**: Logarítmico - muito eficiente
- **O(N)**: Linear - proporcional ao tamanho
- **O(N log N)**: Quase linear - bastante eficiente
- **O(N²)**: Quadrático - menos eficiente para dados grandes
- **O(2^N)**: Exponencial - muito ineficiente

**Exemplo prático:**
- Buscando em um array não ordenado: O(N)
- Ordenação por seleção: O(N²)
- Cálculo matemático direto: O(1)

## Do pseudocódigo ao código Pascal

### Principais correspondências

| Pseudocódigo | Pascal |
|-------------|--------|
| `variável ← valor` | `variável := valor;` |
| `DISPLAY` | `WriteLn()` |
| `READ` | `ReadLn()` |
| `SE ... ENTÃO ... SENÃO` | `se ... então ... senão` |
| `PARA i DE 1 A N` | `para i := 1 a N faça` |
| `ENQUANTO condição` | `enquanto condição faça` |
| `REPITA ... ATÉ` | `repita ... até` |
| `DE ACORDO COM a variável` | `caso variável de` |
| `FUNÇÃO/PROCEDIMENTO` | `função/procedimento` |

### Exemplo de Tradução Completo

**Pseudocódigo:**
```
ALGORITMO Fatorial

FUNÇÃO Fato(n: inteiro): inteiro
INÍCIO
SE n <= 1 ENTÃO
RETORNA 1
SENÃO
RETORNA n × Fato(n - 1)
FIM SE
FIM

INÍCIO PRINCIPAL
VARIÁVEIS
número: inteiro
resultado: inteiro

EXIBIR "Digite um número: "
LER número

resultado ← Fato(número)

EXIBIR "Fatorial de ", número, " = ", resultado
FIM
```

**Código Pascal:**
```pascal
programa Fatorial;

função Fato(n: Inteiro): Inteiro;
início
se n <= 1 então
Fato := 1
senão
Fato := n * Fato(n - 1);
fim;

var
número, resultado: Inteiro;

begin
WriteLn('Digite um número: ');
ReadLn(número);

result := Fact(número);

WriteLn('Fatorial de ', número, ' = ', resultado);
end.
```

## Melhores Práticas para Escrever Algoritmos

### 1. Comece Simples

Não procure a solução perfeita imediatamente. Escreva uma versão funcional primeiro e depois otimize.

### 2. Decomponha o Problema

Divida problemas complexos em subproblemas mais simples.

```
ProbLeme: Gerenciando uma Biblioteca

Subproblemas:
- Adicionar um livro
- Buscar um livro
- Pegar um livro emprestado
- Devolver um livro
- Exibir a lista de livros
```

### 3. Use nomes significativos

```
// Ruim
PARA i DE 1 PARA n FAÇA
x ← x + y[i]
FIM PARA

// Bom
PARA índice_livros DE 1 PARA número_de_livros FAÇA
preço_total ← preço_total + preço_livro[índice_livro]
FIM PARA
```

### 4. Comente seu algoritmo

```
// Calcular a média apenas das avaliações positivas
soma ← 0
contador ← 0

PARA cada avaliação FAÇA
SE avaliação > 0 ENTÃO
soma ← soma + avaliação
contador ← contador + 1
FIM SE
FIM PARA

SE contador > 0 ENTÃO
média ← soma / contador
FIM SE
```

### 5. Teste seu algoritmo mentalmente

Siga o algoritmo passo a passo com valores de exemplo para verificar se ele funciona.

**Exemplo: Verificando se um número é primo**
```
Entrada: n = 7

i = 2 : 7 mod 2 = 1 (diferente de 0, continuar)
i = 3 : 7 mod 3 = 1 (diferente de 0, continuar)
i = 4 : 7 mod 4 = 3 (diferente de 0, continuar)
i = 5 : 7 mod 5 = 2 (diferente de 0, continuar)
i = 6 : 7 mod 6 = 1 (diferente de 0, continuar)

Nenhum divisor encontrado → 7 é primo ✓
```

### 6. Lidar com casos extremos

Não se esqueça dos casos especiais:
- Matriz vazia
- Valor nulo
- Número negativo
- Divisão por zero
```
ALGORITMO DE MÍNIMA BUSCA

SE a matriz estiver vazia, ENTÃO
IMPRIMA "Erro: matriz vazia"
RETORNAR
FIM SE

mínimo ← array[0]
PARA i DE 1 A comprimento-1 FAÇA
SE array[i] < mínimo ENTÃO
mínimo ← array[i]
FIM SE
FIM PARA
```

## Erros comuns a evitar

### 1. Laços infinitos

```
// ERRO: Condição nunca se torna falsa
i ← 1
ENQUANTO i > 0 FAÇA
IMPRIMA i
i ← i + 1 // i aumenta, nunca diminui!
FIM ENQUANTO
```

### 2. Subscritos de array inválidos

```
// ERRO: Estouro de array
Array de tamanho 10 (subscritos de 0 a 9)
PARA i DE 0 A 10 FAÇA // i = 10 está fora dos limites!
DISPLAY array[i]
FIM PARA
```

### 3. Variáveis ​​Não Inicializadas

```
// ERRO: soma não foi inicializada
PARA i DE 1 A 10 FAÇA
soma ← soma + i // O que é soma no início?
FIM PARA

// CORRETO
soma ← 0 // Inicialização
PARA i DE 1 A 10 FAÇA
soma ← soma + i
FIM PARA
```

### 4. Confusão entre Atribuição e Comparação

```
// ERRO: Usando = em vez de ←
SE x = 5 ENTÃO // Comparação (correto)
x = 10 // ERRO! Deveria ser x ← 10
FIM SE
```

## Conclusão

Pseudocódigo e algoritmos são ferramentas essenciais para qualquer programador. Eles permitem que você:
- Pense na lógica antes de codificar
- Comunique suas ideias com clareza
- Detecte erros logo no início do processo
- Crie soluções eficientes

**Principais conclusões:**
- Um algoritmo é uma sequência de instruções para resolver um problema
- Pseudocódigo é uma descrição informal de um algoritmo
- Use estruturas básicas: sequência, seleção, repetição
- Divida problemas complexos em subproblemas
- Teste mentalmente seus algoritmos antes de codificá-los
- Eficiência importa: alguns algoritmos são melhores que outros
- Clareza é importante: escreva para ser compreendido

**Próximos passos:**
Na próxima seção, aprenderemos como representar visualmente esses algoritmos com fluxogramas, outra ferramenta valiosa para o design de programas.

**Dica prática:**
Adquira o hábito de escrever seus algoritmos em pseudocódigo antes de programar. Isso economizará muito tempo e evitará muitos erros!

⏭️ [Organogramas e estruturação do pensamento](/01-prerequis-bases-programmation/06-organigrammes-structuration-pensee.md)