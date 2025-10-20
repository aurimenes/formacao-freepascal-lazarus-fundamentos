🔝 Voltar para [Resumo](/SUMMARY.md)

# 1.4 Lógica Booleana e Tabelas Verdade

## Introdução

A lógica booleana é a base de toda programação. Ela permite que você tome decisões, controle o fluxo de execução do programa e teste condições. Esta seção fornecerá as ferramentas para entender e dominar a lógica em seus programas.

## O que é Lógica Booleana?

### História e Origens

A lógica booleana recebeu esse nome em homenagem ao matemático britânico George Boole (1815-1864), que desenvolveu um sistema matemático para representar a lógica.

**Princípio Fundamental:** Na lógica booleana, qualquer afirmação só pode ser **verdadeira** ou **falsa**, sem estados intermediários.

### Valores Booleanos

Existem apenas dois valores possíveis:
- **TRUE** (Verdadeiro, 1, Sim)
- **FALSE** (Falso, 0, Não)

**Exemplos de declarações booleanas:**
```
"Está chovendo" → VERDADEIRO ou FALSO
"5 é maior que 3" → VERDADEIRO
"10 é igual a 20" → FALSO
"A porta está aberta" → VERDADEIRO ou FALSO
```

**Em Pascal:**
```pascal
var
isTrue: Boolean;

begin
isTrue := True; // Atribui TRUE
isTrue := False; // Atribui FALSE
end. ```

## Operadores Lógicos Básicos

### O Operador NOT

**Símbolo em Pascal:** `not`

**Função:** Inverte um valor booleano.

**Tabela Verdade:**
```

| A | NOT A |
|-------|-------|
| TRUE | FALSE |
| FALSE | TRUE |
```

**Exemplos Concretos:**
```
NOT (Está chovendo) = Não está chovendo
NOT (A porta está aberta) = A porta está fechada
NOT (TRUE) = FALSE
NOT (FALSE) = TRUE
```

**Em Pascal:**
```pascal
var
aFaim: Boolean;
nAPasFaim: Boolean;

begin
aFaim := True;
nAPasFaim := not aFaim; // nAPasFaim é Falso
fim.
```

### O Operador AND

**Símbolo de Pascal:** `and`

**Função:** Retorna VERDADEIRO somente se **todas** as condições forem verdadeiras.

**Tabela Verdade:**
```
| A | B | A E B |
|-------|-------|---------|
| FALSO | FALSO | FALSO |
| FALSO | VERDADEIRO | FALSO |
| VERDADEIRO | VERDADEIRO | VERDADEIRO |
```

**Memorização:** O resultado é VERDADEIRO somente se A **E** B forem ambos verdadeiros.

**Exemplos concretos:**
```
(Estou com fome) E (Tem comida) → Eu posso comer
(Está chovendo) E (Eu tenho um guarda-chuva) → Eu posso sair sem me molhar
(Idade >= 18) E (Tem carteira de habilitação) → Posso dirigir
```

**Em Pascal:**
```pascal
var
aHunger, aFood, canEat: Boolean;

begin
aHunger := True;
aFood := True;
canEat := aHunger and aFood; // TRUE

// Exemplo com condições
if (idade >= 18) and (aLicense) then
WriteLn('Você pode dirigir');
end.
```

### O Operador OU

**Símbolo em Pascal:** `ou`

**Função:** Retorna VERDADEIRO se **pelo menos uma** das condições for verdadeira.

**Tabela Verdade:**
```
| A | B | A OU B |
|-------|-------|--------|
| FALSO | FALSO | FALSO |
| FALSO | VERDADEIRO | VERDADEIRO |
| VERDADEIRO | FALSO | VERDADEIRO |
```

**Memorização:** O resultado é VERDADEIRO se A **OU** B (ou ambos) forem verdadeiros.

**Exemplos concretos:**
```
(É sábado) OU (É domingo) → É fim de semana
(Está calor) OU (Estou com sede) → Quero beber
(Nota >= 10) OU (Abdominais aprovados) → Exame aprovado
```

**Em Pascal:**
```pascal
var
isSaturday, isSunday, isWeekend: Boolean;

begin
isSaturday := False;
isSunday := True;
isWeekend := isSaturday or isSunday; // TRUE

// Exemplo com condições
if (temperatura > 30) or (solBrilho) then
WriteLn('Usar protetor solar');
end.
```

## Operadores Lógicos Compostos

### O Operador XOR (OU Exclusivo)

**Símbolo em Pascal:** `xor`

**Função:** Retorna VERDADEIRO se **apenas** uma das duas condições for verdadeira (mas não ambas).

**Tabela Verdade:**
```
| A | B | A XOR B |
|-------|-------|---------|
| FALSO | FALSO | FALSO |
| FALSO | VERDADEIRO | VERDADEIRO |
| VERDADEIRO | VERDADEIRO | FALSO |
```

**Memorização:** O resultado é VERDADEIRO se A e B forem **diferentes**.

**Exemplos concretos:**
```
(Chá) XOR (Café) → Eu pego um OU o outro, mas não ambos
(Cara) XOR (Coroa) → Uma moeda só pode ser cara ou coroa
```

**Em Pascal:**
```pascal
var
choiceThe, choiceCafe: Boolean;

begin
choiceThe := True;
choiceCafe := False;

if choiceThe xor choiceCafe then
WriteLn('Você fez uma escolha exclusiva');
// Resultado: VERDADEIRO (apenas uma é verdadeira)
end.
```

### Os Operadores NAND e NOR

Esses operadores são menos comuns em programação, mas importantes em eletrônica.

**NAND (NÃO E):**
```
| A | B | A NAND B |
|-------|-------|----------|
| FALSE | FALSO | VERDADEIRO |
| FALSO | VERDADEIRO | VERDADEIRO |
| VERDADEIRO | FALSO | VERDADEIRO |
| VERDADEIRO | VERDADEIRO | FALSO |
```

NAND = NOT (A AND B)

**NOR (NOT OR):**
```
| A | B | A NOR B |
|-------|-------|---------|
| FALSO | FALSO | VERDADEIRO |
| FALSO | VERDADEIRO | FALSO |
| VERDADEIRO | FALSO | FALSO |
```

NOR = NOT (A OR B)

**Em Pascal:**
```pascal
// NAND é escrito:
result := not (A e B);

/ NOR é escrito:
result := not (A ou B);
```

## Operadores de Comparação

Esses operadores retornam valores booleanos.

### Operadores disponíveis em Pascal

```
= : Igual a
<> : Diferente de
< : Menor que
> : Maior que
<= : Menor ou igual a
>= : Maior ou igual a
```

**Exemplos:**
```pascal
5 = 5 // VERDADEIRO
5 <> 3 // VERDADEIRO (5 não é igual a 3)
10 > 7 // VERDADEIRO
10 < 7 // FALSO
10 >= 10 // VERDADEIRO
8 <= 5 // FALSO
```

### Uso em expressões

```pascal
var
idade: Inteiro;
éMaior, éFilho: Booleano;

begin
idade := 20;

éMaior := (idade >= 18); // VERDADEIRO
éFilho := (idade < 12); // FALSO

se (idade >= 18) e (idade < 65), então
WriteLn('Você é um adulto ativo');
fim.
```

## Expressões Booleanas Complexas

### Operadores Combinadores

Você pode combinar vários operadores lógicos em uma única expressão.

**Exemplo 1:** Para entrar em um parque de diversões
```pascal
var
idade, altura: Inteiro;
comAdulto, podeEntrar: Booleano;

começo
idade:= 10;
altura:= 140;
comAdulto:= Verdadeiro;

// Condições: (acima de 12 anos) OU (acima de 130 cm E acompanhado)
podeEntrar:= (idade >= 12) ou ((altura >= 130) e comAdulto);
// Resultado: VERDADEIRO
fim.
```

**Exemplo 2:** Condições para acessar um desconto
```pascal
// Desconto se: (Estudante OU Aposentado) E ainda não for cliente
reductionApplicable := (éEstudante ou éAposentado) e (não éJáJáUmCliente);
```

### Precedência de operadores

Como na matemática, há uma ordem de precedência:

**1. Parênteses**: `( )`
**2. NÃO** (negação)
**3. E** (multiplicação lógica)
**4. OU, XOR** (adição lógica)
**5. Comparações**: `=`, `<>`, `<`, `>`, `<=`, `>=`

**Exemplo sem parênteses:**
```pascal
A ou B e C
// Equivalente a: A ou (B e C)
// AND é avaliado antes de OR
```

**Dica:** Sempre use parênteses para esclarecer suas intenções, mesmo que não seja estritamente necessário.

```pascal
// Ambíguo
se idade >= 18 e idade <= 65 ou estRetirement então ...

// Limpar
se ((idade >= 18) e (idade <= 65)) ou estRetirement então ...
```

## As leis da lógica booleana

### Leis básicas

**Lei da comutatividade:**
```
A E B = B E A
A OU B = B OU A
```

**Lei da associatividade:**
```
(A E B) E C = A E (B E C)
(A OU B) OU C = A OU (B OU C)
```

**Lei distributiva:**
```
A E (B OU C) = (A E B) OU (A E C)
A OU (B E C) = (A OU B) E (A OU C)
```

**Lei da Identidade:**
```
A E VERDADEIRO = A
A OU FALSO = A
```

**Lei dos Elementos Absorventes:**
```
A E FALSO = FALSO
A OU VERDADEIRO = VERDADEIRO
```

**Lei da Complementaridade:**
```
A E (NÃO A) = FALSO
A OU (NÃO A) = VERDADEIRO
```

**Lei da Dupla Negação:**
```
NÃO (NÃO A) = A
```

### Leis de De Morgan

Estas leis são muito importantes para simplificar expressões:

**Primeira Lei:**
```
NÃO (A E B) = (NÃO A) OU (NÃO B)
```

**Segunda Lei:**
```
NÃO (A OU B) = (NÃO A) E (NÃO B)
```

**Exemplo Prático :**
```pascal
// Expressão original
if not ((age < 18) or (age > 65)) then
WriteLn('Adulto ativo');

/ Equivalente de acordo com De Morgan
if (age >= 18) and (age <= 65) then
WriteLn('Adulto ativo');
```

## Tabelas verdade para expressões complexas

### Construindo uma tabela verdade

Para analisar uma expressão complexa, construímos uma tabela com todas as combinações possíveis.

**Exemplo: (A AND B) OR (NOT A AND C)**

```
| A | B | C | A AND B | NOT A | NOT A AND C | Resultado |
|-------|-------|-------|---------|----------|------|----------|----------|----------|
| FALSO | FALSO | FALSO | VERDADEIRO | FALSO | FALSO |
| FALSO | FALSO | VERDADEIRO | FALSO | VERDADEIRO | VERDADEIRO | VERDADEIRO | VERDADEIRO |
| FALSO | VERDADEIRO | FALSO | FALSO | VERDADEIRO | FALSO | FALSO |
| VERDADEIRO | FALSO | FALSO | FALSO | FALSO | FALSO |
| VERDADEIRO | FALSO | FALSO | FALSO | FALSO | FALSO | FALSO |
| VERDADEIRO | VERDADEIRO | FALSO | FALSO | FALSO | FALSO | VERDADEIRO |
| VERDADEIRO | VERDADEIRO | VERDADEIRO | FALSO | FALSO | FALSO | VERDADEIRO |
```

**Método:**
1. Liste todas as combinações possíveis de variáveis ​​(2 linhas para n variáveis)
2. Calcule as expressões intermediárias
3. Calcule o resultado final

## Aplicações de Programação

### Condições SE

Expressões booleanas são o cerne das instruções condicionais.

```pascal
var
temperatura: Inteiro;
éVerão, éQuente: Booleano;

begin
temperatura := 28;
éVerão := Verdadeiro;

/ Condição Simples
se temperatura > 25 então
WriteLn('Está quente');

/ Condição Composta
se (temperatura > 25) e éVerão então
WriteLn('É verão e está quente');

// Condição com ELSE
se (temperatura < 10) ou (não estEte) então
WriteLn('Leve um casaco')
senão
WriteLn('Roupas leves são suficientes');
fim.
```

### Laços WHILE

Uso de laços também expressões booleanas.

```pascal
var
contador, soma: Inteiro;
continue: Booleano;

begin
contador := 1;
soma := 0;
continue := Verdadeiro;

/ O loop continua enquanto a condição for VERDADEIRA
while (contador <= 10) and continue do
begin
soma := soma + contador;
contador := contador + 1;

/ O loop pode ser interrompido se uma condição for atendida
if soma > 30 then
continue := Falso;
fim;
fim.
```

### Sinalizadores

Variáveis ​​booleanas são frequentemente usadas como sinalizadores para controlar o estado do programa.

```pascal
var
encontrado, erro, finalizado: Booleano;
i: Inteiro;

begin
encontrado := Falso;
erro := Falso;
i := 1;

enquanto (i <= 100) e (não encontrado) e (não erro)
começar
// Procurar um elemento
se array[i] = valorDePesquisa, então
encontrado:= Verdadeiro
senão se array[i] < 0, então
erro:= Verdadeiro;

i := i + 1;
fim;

se encontrado, então
WriteLn('Elemento encontrado')
senão se erro, então
WriteLn('Erro detectado')
senão
WriteLn('Elemento não encontrado');
fim.
```

## Curto-circuito na Avaliação

### O que é curto-circuito?

Em Pascal (e em muitas outras linguagens), as expressões booleanas são avaliadas da esquerda para a direita e a avaliação para assim que o resultado é conhecido.

**Para E:**
- Se a primeira condição for FALSA, o resultado será necessariamente FALSA
- A segunda condição não será avaliada

**Para OU:**
- Se a primeira condição for VERDADEIRA, o resultado será necessariamente VERDADEIRA
- A segunda condição não será avaliada

### Exemplo prático

pascal
// Evitar divisão por zero
if (denominador <> 0) and (numerador / denominador > 10) then
WriteLn('Resultado maior que 10');
// Se denominador = 0, a divisão nunca será realizada

// Verificar se um array não está vazio antes de acessar um elemento
if (Comprimento(array) > 0) and (array[0] = valor) then
WriteLn('O primeiro elemento corresponde');
```

**Aviso:** No FreePascal, para garantir o curto-circuito, use as diretivas do compilador ou os operadores especiais `and then` e `or else`:

```pascal
// Curto-circuito garantido
if (x <> 0) and then (y / x > 5) then
WriteLn('OK');
```

## Simplificando Expressões Booleanas

### Por que simplificar?

Uma expressão simplificada é:
- Mais fácil de entender
- Mais rápida de executar
- Menos propensa a erros

### Exemplos de Simplificação

**Exemplo 1: Usando Leis**
```pascal
// Expressão Original
if (age >= 18) and (True) then
...

// Simplificada (Lei da Identidade: A AND TRUE = A)
if age >= 18 then
...
```

**Exemplo 2: Eliminando Negações Duplas**
```pascal
// Expressão Original
if not (not isActive) then
...

// Simplificada (negação dupla)
if isActive then
...
```

**Exemplo 3: Aplicando De Morgan**
```pascal
// Expressão Original
if not (isClosed or isComplete) then
...

// Simplificada (De Morgan)
if (not isClosed) and (not isFull) então
...

// Ou melhor, com nomes positivos
se isOpen e hasAvailablePlaces então
...
```

## Melhores Práticas

### 1. Use nomes de variáveis ​​explícitos

```pascal
// Ruim
se x e y ou não z então ...

// Bom
se isLoggedIn e hasPermissions ou não isLocked então ...
```

### 2. Evite comparações desnecessárias de Verdadeiro/Falso

```pascal
// Ruim
se isActive = True então ...
se isClosed = False então ...

// Bom
se isActive então ...
se não isClosed então ...
```

### 3. Use parênteses para maior clareza

```pascal
// Menos claro
se age >= 18 and age <= 65 or isRetired então ...

// Mais claro
se ((age >= 18) and (age <= 65)) ou isRetired então ...
```

### 4. Prefira expressões positivas

```pascal
// Menos claro
if not (not isOpen) then ...

// Mais claro
if isOpen then ...
```

### 5. Decomponha expressões complexas

```pascal
// Complexo
if (age >= 18) and (age <= 65) and (hasLicense) and (not isSuspended) then ...

// Mais legível
var
isAdult, canDrive: Boolean;
begin
isAdult := (age >= 18) and (age <= 65);
canDrive := hasLicense and (not isSuspended);

if isAdult and canDrive then ...
end.
```

## Armadilhas comuns a evitar

### 1. Confusão entre = e :=

```pascal
// Erro: = é para comparação, não atribuição
if x = 5 then
x = 10; // ERRO!

/ Correto
if x = 5 then
x := 10; // Tarefa
```

### 2. Esquecendo Parênteses

```pascal
// Ambíguo devido à precedência de operadores
if age >= 18 and aPermis then ...

// Melhor
if (age >= 18) and aPermis then ...
```

### 3. Negações Complexas

```pascal
// Difícil de entender
if not (not A and not B) then ...

// Aplique De Morgan para simplificar
if A or B then ...
```

### 4. Curto-circuito não intencional

```pascal
// Pode causar problemas se a função tiver efeitos colaterais
if (a > 0) and FunctionThatModifies() then ...
// Se a <= 0, FunctionThatModifies() nunca é chamada!
```

## Conclusão

A lógica booleana é uma ferramenta fundamental em Programação que permitirá a você criar programas inteligentes capazes de tomar decisões.

**Principais Conclusões:**
- Existem apenas dois valores booleanos: TRUE e FALSE
- Os operadores básicos são NOT, AND e OR
- Tabelas verdade permitem analisar expressões
- Leis booleanas permitem simplificar expressões
- Expressões booleanas estão no centro das estruturas de controle (if, while)
- Use nomes e parênteses explícitos para maior clareza
- Cuidado com avaliações em curto-circuito

**O que você precisa dominar:**
- Construir e compreender tabelas verdade
- Combinar condições com AND, OR e NOT
- Simplificar expressões booleanas
- Usar operadores de comparação corretamente
- Escrever condições claras e fáceis de manter

Na próxima seção, colocaremos esse conhecimento em prática com algoritmos e pseudocódigo, antes de começarmos a programar em Pascal.

⏭️ [Algoritmos e pseudocódigo](/01-prerequisites-programming-bases/05-algorithms-pseudo-code.md)