🔝 Voltar para [Resumo](/SUMMARY.md)

# 1.6 Fluxogramas e Estruturação do Pensamento

## Introdução

Fluxogramas são representações gráficas de algoritmos. Eles nos permitem visualizar o fluxo de execução de um programa de forma clara e intuitiva. Nesta seção, aprenderemos como ler, criar e usar fluxogramas para estruturar nosso pensamento antes de programar.

## O que é um fluxograma?

### Definição

Um **fluxograma** é um diagrama que representa visualmente a sequência de operações a serem realizadas para resolver um problema ou realizar uma tarefa.

**Vantagens dos Fluxogramas:**
- **Visual**: Mais fácil de entender do que texto
- **Universal**: Compreensível por todos, independentemente da linguagem de programação
- **Claro**: Destaca a lógica do programa
- **Comunicação**: Facilita as discussões em equipe
- **Documentação**: Serve como referência para a compreensão do código

**Desvantagens:**
- Pode se tornar complexo para programas grandes
- Demorado para desenhar
- Difícil de manter à medida que o código evolui

### Quando usar fluxogramas?

Os fluxogramas são particularmente úteis para:
- Planejar um novo programa
- Entender um algoritmo existente
- Identificar erros de lógica
- Explicar um conceito para outras pessoas
- Documentar processos complexos

## Símbolos Padrão de Fluxograma

Os fluxogramas usam formas geométricas padronizadas. Aqui estão os símbolos mais comuns:

### Símbolos Básicos

**1. Terminal (Início/Fim)**
```
╔═══════════╗
║ INÍCIO ║
╚══════════╝
```
- Formato: Oval ou retângulo com cantos arredondados
- Uso: Marca o início e o fim de um programa
- Texto: "INÍCIO", "FIM", "INÍCIO", "FIM"

**2. Processamento (Instrução)**
```
┌─────────────┐
│ Instrução │
└────────────┘
```
- Forma: Retângulo
- Uso: Representa uma ação, um cálculo, uma atribuição
- Exemplos: "soma ← a + b", "contador ← contador + 1"

**3. Entrada/Saída**
```
╱────────────╲
╱ Exibir X ╲
╲───────────────╱
```
- Formato: Paralelogramo
- Uso: Leitura de dados ou exibição de resultados
- Exemplos: "LER número", "EXIBIR resultado"

**4. Decisão (Condição)**
```
╱╲
╱ ╲
╱Cond╲
╲ ? ╱
╲ ╱
╲╱
```
- Forma: Diamante
- Uso: Testa uma condição, escolhe entre dois caminhos
- Saídas: Duas setas (SIM/NÃO, VERDADEIRO/FALSO)
- Exemplo: "x > 0?"

**5. Conector**
```
( A )
```
- Forma: Círculo
- Uso: Conecta partes de um fluxograma (evita setas muito longas)
- Contém: Uma letra ou um número

**6. Seta de Conexão**
```
→ ou ↓
```
- Uso: Indica a direção do fluxo, a ordem de execução
- Sempre orientado

**7. Subrotina (Chamada de Função/Procedimento)**
```
┌─────────────┐
│ Função() │
│──────────────│
└───────────┘
```
- Formato: Retângulo com duas barras verticais nas laterais
- Uso: Chamar uma função ou procedimento
- Exemplo: "CalcularMédia()"

**8. Laço (Preparação)**
```
┌─────────────┐
│ i = 1,10 │
└────────────┘
```
- Forma: Hexágono
- Uso: Inicializar um laço FOR
- Exemplo: "i DE 1 A 10"

## Construindo Fluxogramas Simples

### Exemplo 1: Programa Sequencial Simples

**Problema:** Calcular e exibir a soma de dois números.

**Fluxograma:**
```
╔═══════════╗
║ INICIAR ║
╚════╤═════╝
│
╱─────┴──────╲
╱ LEIA a, b ╲
╲───────┬─────────╱
│
┌──────┴──────┐
│ soma ← a+b│
└──────┬──────┘
│
╱─────┴───────╲
╱ EXIBIR ╲
╲ soma ╱
╲─────┬───────╱
│
╔═════╧═════╗
║ FIM ║
╚═══════════╝
```

**Pseudocódigo correspondente:**
```
BEGIN
LEIA a, b
soma ← a + b
IMPRIMA soma
FIM
```

### Exemplo 2: Estrutura de Decisão (SE...ENTÃO...SENÃO)

**Problema:** Determine se um número é positivo ou negativo.

**Fluxograma:**
```
╔════════════╗
║ INICIAR ║
╚════╤═════╝
│
╱─────┴──────╲
╱ LEITURA número ╲
╲───────┬─────────╱
│
╱╲
╱ ╲
╱ nb ╲
╱ >=0 ╲____NÃO___
╲? ╱ ╲
╲ ╱ │
╲ ╱ │
╲╱ │
SIM │ │
│ │
╱────┴──────╲ ╱────┴──────╲
╱ EXIBIR ╲ ╱ EXIBIR ╲
╲ "Positivo" ╱ ╲ "Negativo" ╱
╲─────┬────────╱ ╲────┬──────
│ │
└───────────┘
│
╔═════╧═════╗
║ FIM ║
╚══════════╝
```

**Pseudocódigo correspondente:**
```
INÍCIO
LEIA o número
SE o número for >= 0 ENTÃO
EXIBIR "Positivo"
SENÃO
EXIBIR "Negativo"
FIM SE
FIM
```

### Exemplo
3: Loop FOR

**Problema:** Exibir os números de 1 a 5.

**Fluxograma:**
```
╔════════════╗
║ INICIAR ║
╚════╤═════╝
│
┌─────┴──────┐
│ i ← 1 │
└─────┬──────┘
│
╱╲
╱ ╲
╱i╲
╱ <=5 ╲____NÃO___
╲? ╱ ╲
╲ ╱ │
╲ ╱ │
╲╱ │
SIM │ │
│ │
╱────┴───────╲ │
╱ MOSTRAR i ╲ │
╲───────┬─────────╱ │
│ │
┌──────┴──────┐ │
│i←i+1││
└─────┬──────┘ │
│ │
└─────────────────┘
│
╔═════╧═════╗
║ FIM ║
╚═══════════╝
```

**Correspondente pseudocódigo:**
```
BEGIN
i ← 1
WHILE i <= 5 DO
DISPLAY i
i ← i + 1
END WHILE
END
```

### Exemplo 4: Loop Repeat...until

**Problema:** Solicita uma senha até que ela esteja correta.

**Fluxograma:**
```
╔════════════╗
║ INICIAR ║
╚════╤═════╝
│
╱─────┴──────╲
╱ LER ╲
╲ Senha ╱
╲──────┬───────╱
│
╱╲
╱ ╲
╱ SENHA╲
╱correta╲___NÃO___
╲ ? ╱ ╲
╲ ╱ │
╲ ╱ │
╲╱ │
SIM │ │
│ ╱────┴─────╲
│ ╱ EXIBIÇÃO ╲
│ ╲ "Erro" ╱
│ ╲────┬─────╱
│ │
│ │
│ │
│ ╱───────────┘
│ ╱
╱─────┴──────╲
╱ EXIBIR ╲
╲ "Acesso OK" ╱
╲─────┬────────╱
│
╔═════╧═════╗
║ FIM ║
╚═══════════╝
```

## Exemplos de Fluxogramas Completos

### Exemplo 5: Calculando um Fatorial

**Problema:** Calcule n! (fatorial de n)

**Fluxograma:**
```
╔════════════╗
║ INICIAR ║
╚════╤═════╝
│
╱─────┴──────╲
╱ LEIA n ╲
╲───────┬──────────╱
│
┌─────┴──────┐
│ fato ← 1 │
│ i ← 1 │
└──────┬──────┘
│
╱╲
╱ ╲
╱i╲
╱ <= n╲____NÃO___
╲? ╱ ╲
╲ ╱ │
╲ ╱ │
╲╱ │
SIM │ │
│ │
┌─────┴──────┐ │
│fato ← fato │ │
│ × i │ │
└─────┬──────┘ │
│ │
┌─────┴──────┐ │
│i←i+1││
└─────┬──────┘ │
│ │
└────────────────┘
│
╱──────┴─────╲
╱ MOSTRAR ╲
╲ fato ╱
╲─────┬───────╱
│
╔═════╧═════╗
║ FIM ║
╚══════════╝
```

### Exemplo 6: Encontrando o máximo em um array

**Problema:** Encontre o maior elemento em um array.

**Fluxograma:**
```
╔════════════╗
║ INICIAR ║
╚════╤════╝
│
╱──────┴──────╲
╱ LEIA a tabela ╲
╲ e tamanho ╱
╲──────┬──────╱
│
┌─────┴──────┐
│ max ← │
│ array[0] │
│ i ← 1 │
└──────┬──────┘
│
╱╲
╱ ╲
╱i╲
╱<tamanho╲___NÃO___
╲? ╱ ╲
╲ ╱ │
╲ ╱ │
╲╱ │
SIM │ │
│ │
╱╲ │
╱ ╲ │
╱tab[i]╲ │
╱ > max╲__NÃO__ │
╲ ? ╱ ╲ │
╲ ╱ │ │
╲ ╱ │ │
╲╱ │ │
SIM │ │ │
│ │ │
┌────┴──────┐ │ │
│ máx ← │ │ │
│ array[i] │ │ │
└────┬──────┘ │ │
│ │ │
└────┬───────┘ │
│ │
┌──────────┴──────┐ │
│i←i+1││
└───────────┬──────┘ │
│ │
└──────────┘
│
╱──────┴──────╲
╱ MOSTRAR ╲
╲ máx. ╱
╲──────┬───────╱
│
╔═════╧═════╗
║ FIM ║
╚════════════╝
```

### Exemplo 7: Menu com Múltiplas Opções

**Problema:** Exibir um menu e executar a opção escolhida.

**Fluxograma Simplificado:**
```
╔═══════════╗
║ INICIAR ║
╚═════╤═════╝
│
╱──────┴───────╲
╱ MOSTRAR menu ╲
╲───────┬─────────╱
│
╱──────┴───────╲
╱ LEIA a opção ╲
╲───────┬─────────╱
│
╱╲
╱ ╲
╱escolha╲
╱ = 1 ╲___NÃO___
╲? ╱ ╲
╲ ╱ │
╲ ╱ │
╲╱ │
SIM │ ╱╲
│ ╱ ╲
┌─────┴──────┐ ╱escolha╲
│ Opção 1 │ ╱ = 2 ╲___NÃO___
└─────┬──────┘ ╲ ? ╱ ╲
│ ╲ ╱ │
│ ╲ ╱ ╱╲
│ ╲╱ ╱ ╲
│ SIM │ ╱escolha╲
│ │ ╱ = 3 ╲___NÃO___
│ ┌─────┴──────┐ ╲ ? ╱ ╲
│ │ Opção 2 │ ╲ ╱ │
│ └─────┬──────┘ ╲ ╱ │
│ │ ╲╱ │
│ │ SIM │ │
│ │ │ │ │
│ │ │ ┌────┴──────┐ ┌─────┴──────┐
│ │ │ Opção 3 │ │ Erro │
│ │ └─────┬──────┘ └──────┬──────┘
│ │ │ │
└───────┬───────┴───────┬───────┴────────┬─────────┘
│ │ │
╔═════╧═══════════════════╧════════════════╧═════╗
║ FIM ║
╚═════════════════════════════════════════════════
```

## Estruturando o Pensamento com Fluxogramas

### Método de Design Top-Down

O design top-down envolve a divisão de um problema complexo em subproblemas mais simples.

**Etapas:**

1. **Identifique o Problema Principal**
- O que o programa deve fazer em geral?

2. **Divida em etapas principais**
- Quais são as etapas principais?

3. **Refine cada etapa**
- Divida cada fase em subetapas

4. **Continue com as operações básicas**
- Pare quando cada bloco estiver simples

**Exemplo: Programa de Caderneta de Notas do Aluno**

**Nível 1 (Global):**
```
╔═══════════╗
║ INICIAR ║
╚════╤═════╝
│
┌────┴──────┐
│ Insira │
│ notas │
└─────┬──────┘
│
┌──────┴──────┐
│ Calcular │
│ média │
└──────┬──────┘
│
┌────┴──────┐
│ Mostrar │
│ resultados │
└─────┬───────┘
│
╔═════╧═════╗
║ FIM ║
╚══════════╝
```

**Nível 2 (Detalhado):**
Cada bloco do Nível 1 se torna um fluxograma completo.

### Identificando Estruturas

Aprenda a reconhecer padrões:

**1. Sequência (série de ações)**
```
Ação A → Ação B → Ação C
```

**2. Alternativa (escolha)**
```
Condição?
╱ ╲
SIM NÃO
│ │
Ação A Ação B
```

**3. Repetição (loop)**
```
┌───────────┐
│ Condição? ──NÃO──> Sair
└──┬───────┘
SIM
│
┌──┴────┐
│ Ação│
└──┬────┘
│
└──> (retornar)
```

## Fluxograma para Código

### Correspondência Direta

**Estrutura Sequencial:**
```
Fluxograma: Pascal:
┌─────────┐ a := 5;
│ a ← 5 │ b := 10;
└────┬────┘ c := a + b;
┌────┴────┐
│ b ← 10 │
└────┬────┘
┌────┴────┐
│c ← a+b │
└─────────┘
```

**Estrutura alternativa:**
```
Organograma: Pascal:
╱╲ se x > 0 então
╱x>0╲ WriteLn('Positivo')
╲ ?╱ senão
SIM NÃO WriteLn('Negativo');
│ │
┌─┴─┐ ┌─┴─┐
│Pos│ │Neg│
└───┘ └───┘
```

**Estrutura de Repetição:**
```
Fluxograma: Pascal:
┌─────────┐ i := 1;
│ i ← 1 │ enquanto i <= 10 do
└────┬────┘ begin
╱╲WriteLn(i);
╱i≤10╲ i := i + 1;
╲ ?╱ fim;
SIM │ NÃO
┌──┴──┐
│Mostrar i│
└──┬──┘
┌──┴──┐
│i←i+1│
└──┬──┘
│
(voltar)
```

## Ferramentas para criar fluxogramas

### Ferramentas online (grátis)

**1. Draw.io (diagrams.net)**
- Gratuito e de código aberto
- Interface intuitiva
- Vários símbolos predefinidos
- Exportar para PNG, SVG e PDF

**2. Lucidchart**
- Versão gratuita disponível
- Colaboração em tempo real
- Bibliotecas de formas

**3. Desenhos Google**
- Integrado ao Google Drive
- Simples e acessível
- Fácil de compartilhar

### Software Office

**1. Microsoft Visio**
- Profissional e abrangente
- Pago
- Padrão empresarial

**2. LibreOffice Draw**
- Gratuito e de código aberto
- Completo
- Compatível com Windows, Linux e macOS

**3. Dia**
- Gratuito e de código aberto
- Especializado em diagramas técnicos
- Leve e simples

### Ferramentas de Programação

**1. Flowgorithm**
- Especialmente projetado para aprendizagem
- Permite executar fluxogramas
- Gera código em várias linguagens

**2. yEd**
- Gratuito
- Layout automático
- Excelente para diagramas grandes

## Melhores práticas para fluxogramas

### 1. Sempre comece com BEGIN e termine com END

```
✓ Correto: ✗ Incorreto:
╔════════╗ ┌─────────┐
║ BEGIN ║ │ Ação │
╚═══╤════╝ └────┬────┘
│ ┌────┴────┐
┌───┴────┐ │ Ação │
│ Ação │ └─────────┘
└───┬────┘
╔═══╧════╗
║ FIM ║
╚════════╝
```

### 2. Use setas claras

- Apenas uma direção por seta
- Evite interseções
- Favoreça o fluxo de cima para baixo e da esquerda para a direita

### 3. Seja consistente com os símbolos

Use sempre as mesmas formas para os mesmos tipos de operações.

### 4. Mantenha a simplicidade

Se o fluxograma ficar muito complexo:
- Divida-o em subfluxogramas
- Use sub-rotinas
- Crie vários níveis de detalhe

### 5. Anote, se necessário

Adicione comentários para esclarecer partes complexas.

### 6. Teste seu fluxograma mentalmente

Siga o caminho com valores de exemplo para verificar a lógica.

## Erros comuns a evitar

### 1. Loop infinito

```
✗ Incorreto:
╱╲
╱ ╲
╱ i>0╲
╲ ? ╱
╲ ╱
╲╱
SIM│
┌──┴──┐
│i←i+1│ ← i aumenta, nunca diminui!
└──┬──┘
│
(retornar ao teste)
```

### 2. Condição sem ação

```
✗ Incorreto:
╱╲
╱ ╲
╱ a>b╲
╲ ? ╱
╲ ╱
╲╱
SIM│NÃO ← O que deve ser feito em cada caso?
│
```

### 3. Fluxo Indefinido

Todo caminho deve levar a algum lugar (FIM ou retorno).

### 4. Muitos Detalhes

Não inclua detalhes triviais como "abrir programa" ou "fechar janela".

### 5. Esquecer Casos Extremos

Lembre-se de lidar com:
- Matrizes Vazias
- Valores Nulos
- Divisão por Zero

## Fluxogramas vs. Pseudocódigo

### Quando Usar um ou Outro?

**Fluxogramas:**
- ✓ Visualizam a lógica geral
- ✓ Explicam para não programadores
- ✓ Identificam fluxos alternativos
- ✓ Algoritmos de curta e média duração

**Pseudocódigo:**
- ✓ Programas longos e complexos
- ✓ Mais próximos do código real
- ✓ Mais rápidos de escrever
- ✓ Facilitam a tradução para código

**Melhor abordagem:**
Use os dois juntos:
1. Fluxograma para o panorama geral
2. Pseudocódigo para os detalhes

## Conclusão

Fluxogramas são uma ferramenta poderosa para visualizar e estruturar seu pensamento antes de programar. Eles permitem que você:
- Esclareça a lógica do seu programa
- Identifique erros antes de codificar
- Comunique suas ideias de forma eficaz
- Planeje a estrutura do seu código

**Principais conclusões:**
- Os símbolos padrão (oval, retângulo, losango, paralelogramo) têm uma finalidade específica
- Um fluxograma começa com BEGIN e termina com END
- As setas indicam o fluxo de execução
- Divida problemas complexos em subfluxogramas
- Teste mentalmente seu fluxograma antes de codificar
- Combine fluxogramas e pseudocódigo para um design melhor

**Dica prática:**
Para seus primeiros programas, sempre desenhe um fluxograma simples antes de começar a codificar. Esse hábito economizará muito tempo e evitará muitos erros de lógica.

Na próxima seção, aprenderemos sobre sistemas operacionais e como interagir com eles, o que nos preparará para instalar e usar o FreePascal e o Lazarus.

⏭️ [Introdução aos Sistemas Operacionais](/01-prerequisites-programming-bases/07-introduction-operating-systems.md)
