🔝 Voltar para [Resumo](/SUMMARY.md)

# 1.2 O que é um programa?

## Introdução

Agora que você entende os conceitos fundamentais da ciência da computação, é hora de responder a uma pergunta essencial: o que é um programa de computador? Esta seção ajudará você a entender a natureza de um programa e como ele funciona.

## Definição de um Programa

### Em termos simples

Um programa de computador é uma **sequência de instruções** escrita em uma linguagem que um computador pode entender e executar. É como uma receita, mas para um computador.

Assim como uma receita lhe diz passo a passo como preparar um prato, um programa lhe diz passo a passo o que o computador deve fazer para realizar uma tarefa.

### Os Blocos de Construção

Um programa é composto por:
- **Instruções**: as ações que o computador deve executar
- **Dados**: as informações com as quais o programa trabalha
- **Lógica**: a ordem e as condições para executar as instruções

## Analogia com o Mundo Real

### A Receita Culinária

Vejamos o exemplo de uma receita de crepe:

```
Ingredientes (dados):
- 250 g de farinha
- 3 ovos
- 500 ml de leite
- Uma pitada de sal

Instruções:
1. Misture a farinha e os ovos
2. Adicione o leite aos poucos
3. Adicione o sal
4. Deixe descansar por 1 hora
5. Aqueça uma panela
6. Para cada crepe:
- Despeje uma concha de massa
- Espere 2 minutos
- Vire o crepe
- Espere 2 minutos
7. Sirva
```

Um programa funciona exatamente da mesma maneira:
- Ele recebe **entrada** (ingredientes)
- Realiza **processamento** (mistura, aquecimento)
- Utiliza **loops** (para cada crepe)
- Produz um **resultado** (crepes prontos)

### GPS

Outro exemplo: seu GPS é um programa que:
1. **Recebe** sua localização atual e destino (entradas)
2. **Calcula** a melhor rota (processamento)
3. **Exibe** as direções a seguir (saída)
4. **Atualiza** em tempo real com base na sua localização (loop)

## Diferentes Tipos de Programas

### Programas de Linha de Comando (Console)

Estes são os programas mais simples:
- Executam em uma janela de texto
- Sem interface gráfica
- O usuário digita comandos no teclado
- Os resultados são exibidos como texto

**Exemplo:** um programa que calcula a média das avaliações
```
Digite a primeira avaliação: 15
Digite a segunda avaliação: 18
Entrada na terceira classificação: 12
Média: 15
```

**Vantagens:**
- Fácil de criar
- Perfeito para aprender
- Eficaz para tarefas automatizadas

### Programas com interface gráfica do usuário (GUI)

Estes são os programas que você usa todos os dias:
- Janelas, botões, menus
- Interação com o mouse
- Mais amigável

**Exemplos:** Navegador da web, processador de texto, reprodutor de vídeo

**Vantagens:**
- Mais intuitivo
- Mais agradável de usar
- Permite interações ricas

### Serviços e programas em segundo plano

Estes são programas que rodam sem uma interface visível:
- Eles rodam constantemente no seu computador
- Você não os vê, mas eles estão ativos
- Eles executam tarefas específicas

**Exemplos:** Antivírus, servidor web, serviço de sincronização em nuvem

### Aplicativos da web

Estes são programas que rodam em um navegador:
- O código roda em um servidor remoto
- A exibição é feita no seu navegador
- Sem instalação obrigatório

**Exemplos:** webmail, redes sociais, Google Docs

## O Ciclo de Vida de um Programa

### 1. Design

Antes de escrever uma única linha de código, você deve:
- **Definir o problema** a ser resolvido
- **Analisar os requisitos**: o que o programa deve fazer?
- **Projetar o algoritmo**: como o programa funcionará?
- **Planejar a estrutura**: como o código deve ser organizado?

### 2. Escrevendo o Código (Programação)

Esta é a fase em que o programador:
- Escreve o código-fonte em uma linguagem de programação
- Utiliza um editor de texto ou um IDE (Ambiente Integrado de Desenvolvimento)
- Segue as regras de sintaxe da linguagem

**Exemplo de código-fonte em Pascal:**
```pascal
program Hello;
begin
WriteLn('Hello world!');
end.
```

### 3. Compilação

O código-fonte deve ser traduzido para a linguagem de máquina:
- O **compilador** lê o código-fonte
- Ele verifica se há erros de sintaxe
- Ele traduz o código em instruções binárias
- Ele produz um **arquivo executável**

**No Windows:** o arquivo tem a extensão `.exe`
**No Linux:** o arquivo geralmente não tem extensão

### 4. Teste e Depuração

Após a compilação, o programa deve ser testado:
- **Teste funcional**: O programa faz o que deveria?
- **Detecção de bugs**: Há algum erro de lógica?
- **Depuração**: Corrige quaisquer erros encontrados
- **Teste de limite**: O que acontece em casos extremos?

### 5. Implantação

Quando o programa estiver pronto:
- Distribuído aos usuários
- Instalação nos computadores de destino
- Documentação fornecida

### 6. Manutenção

Após a implantação:
- Correções de bugs descobertas pelos usuários
- Adição de novos recursos
- Atualizações para manter a compatibilidade com novos sistemas

## Como um programa é executado

### Carregamento na memória

Ao iniciar um programa:
1. O sistema operacional **lê** o arquivo executável do disco rígido
2. Ele **carrega** o programa na RAM
3. Ele **aloca** espaço de memória para os dados do programa
4. Ele **transfere** o controle para o processador

### Execução sequencial

O processador executa as instruções **uma a uma**, na seguinte ordem:

```
Instrução 1 → Instrução 2 → Instrução 3 → ...
```

Cada instrução corresponde a uma operação básica:
- Executar um cálculo
- Ler ou gravar na memória
- Exibir algo na tela
- Ler a entrada do usuário
- Etc.

### O ponteiro de instrução

O processador lembra onde está no programa:
- Ele usa um "ponteiro de instrução"
- Este ponteiro indica a próxima instrução a ser executada
- Após cada instrução, o ponteiro avança

### Ramificações e laços

O programa nem sempre é estritamente linear:
- **Condições**: o programa pode pular certas instruções
- **Laços**: o programa pode voltar e repetir instruções
- **Chamadas de função**: o programa pode pular para outro local e então retornar

## Linguagens de programação

### Por que linguagens diferentes?

Existem centenas de linguagens de programação porque:
- Cada linguagem tem seus pontos fortes
- Algumas são adequadas a domínios específicos
- Algumas são mais fáceis de aprender
- As tecnologias evoluem e novas linguagens surgem

### Classificação das Linguagens

**Linguagens de Baixo Nível**
- Semelhantes à linguagem de máquina
- Alto desempenho
- Difíceis de ler e escrever
- Exemplos: Assembler

**Linguagens de Nível Intermediário**
- Bom equilíbrio entre desempenho e legibilidade
- Controle de memória refinado
- Exemplos: C, Pascal, Rust

**Linguagens de Alto Nível**
- Altamente legíveis, semelhantes à linguagem humana
- Gerenciamento automático de memória
- Mais lentas, mas fáceis de usar
- Exemplos: Python, Java, JavaScript

### Linguagens Compiladas vs. Interpretadas

**Linguagens Compiladas** (como Pascal)
- O código é traduzido **uma vez** para um arquivo executável
- A execução é rápida
- O arquivo pode ser distribuído sem o código-fonte
- Exemplos: Pascal, C, C++, Rust

**Linguagens Interpretadas**
- O código é traduzido **linha por linha** durante a execução
- Execução mais lenta
- Requer um interpretador instalado
- Exemplos: Python, JavaScript, PHP

**Linguagens Híbridas**
- Compilado em um formato intermediário
- Em seguida, interpretado por uma máquina virtual
- Exemplos: Java, C#

## Pascal: Uma Excelente Escolha para Iniciantes

### Por que Aprender Pascal?

**1. Clareza e Legibilidade**
- A sintaxe é muito próxima da linguagem natural
- O código é fácil de ler e entender
- As instruções são autoexplicativas

**2. Educacional**
- Originalmente projetado para ensino
- Incentiva boas práticas
- Estrutura clara e lógica

**3. Abrangente**
- Permite que você aprenda todos os conceitos fundamentais
- Da programação simples à programação orientada a objetos
- Do modo console às interfaces gráficas

**4. Eficiente**
- Linguagem compilada, portanto rápida de executar
- Adequado para aplicações profissionais

**5. Multiplataforma**
- FreePascal funciona em Windows, Linux e macOS
- O mesmo código pode ser executado em diferentes sistemas

### Comparação com outras linguagens

**Pascal vs. Python**
- Pascal: compilado, mais rápido, tipagem estrita
- Python: interpretado, mais fácil de começar, tipagem dinâmica

**Pascal vs. C**
- Pascal: mais legível, menos armadilhas para iniciantes
- C: mais próximo do hardware, usado para sistemas

**Pascal vs. Java**
- Pascal: compilação nativa mais simples
- Java: máquina virtual puramente orientada a objetos

## Estrutura Geral de um Programa Pascal

Veja como é um programa Pascal simples:

```pascal
program NomeDoPrograma;

{ Seção de Declarações }
var
variável1: Inteiro;
variável2: String;

{ Corpo do Programa }
begin
{ Instruções }
WriteLn('Início do programa');
variável1 := 42;
WriteLn('Valor: ', variável1);
WriteLn('Fim do programa');
end.
```

**Elementos Principais:**
- `program`: Define o nome do programa
- `var`: Seção de declarações de variáveis
- `begin` ... `end.`: Bloco principal de instruções
- `;`: Finaliza cada instrução
- `{ }`: Comentários

Estudaremos tudo isso em detalhes nos próximos capítulos.

## Erros de Programação

### Tipos de Erros

**1. Erros de Sintaxe**
- O código não segue as regras da linguagem
- Detectados durante a compilação
- O programa não pode ser compilado

Exemplo: falta um ponto e vírgula
```pascal
WriteLn('Hello') // Erro: faltando ;
```

**2. Erros de Tempo de Execução**
- O programa compila, mas trava durante a execução
- Causado por operações impossíveis

Exemplo: divisão por zero
```pascal
resultado := 10 / 0; // Erro de tempo de execução!
```

**3. Erros de Lógica**
- O programa funciona, mas não faz o que deveria
- O mais difícil de detectar
- Requer testes extensivos

Exemplo: fórmula de cálculo incorreta
```pascal
média := (nota1 + nota2) * 2; // Deve ser / 2
```

### A Importância da Depuração

Depuração é a arte de encontrar e corrigir erros:
- É uma habilidade essencial do programador
- Você deve ser paciente e metódico
- Usar ferramentas (depurador, exibição de variáveis)
- Entender a lógica do programa

**Dica:** Não desanime! Todos os programadores, mesmo os especialistas, gastam muito tempo depurando seu código.

## Boas Práticas de Programação

Mesmo sendo iniciante, adote estes bons hábitos:

### 1. Comente seu código
Explique o que seu código faz com comentários:
```pascal
{ Esta função calcula a média de dois números }
```

### 2. Use nomes significativos
```pascal
// Ruim
var x, y, z: Integer;

// Bom
var numberStudents, totalPoints, average: Integer;
```

### 3. Indentação correta
```pascal
// Bom
se condição então
começar
instrução1;
instrução2;
fim;
```

### 4. Teste regularmente
Não codifique tudo de uma vez. Teste frequentemente à medida que avança.

### 5. Mantenha as coisas simples
Código simples é mais fácil de entender e manter.

## Conclusão

Agora você sabe o que é um programa de computador:
- Uma sequência de instruções executadas pelo computador
- Que passa por várias etapas: design, escrita, compilação, teste
- Que pode assumir diferentes formas: console, gráfico, web
- Que pode conter diferentes tipos de erros

Pascal é uma excelente linguagem para aprender a programar graças à sua clareza e estrutura lógica. Nas seções a seguir, nos aprofundaremos nos conceitos necessários antes de escrever nossos primeiros programas.

**Principais Conclusões:**
- Um programa é como uma receita que o computador segue
- O código-fonte é compilado em um arquivo executável
- Existem diferentes tipos de linguagens de programação
- Pascal é particularmente adequado para iniciantes
- Programar também envolve testar e depurar seu código

⏭️ [Sistemas Numéricos e Representação de Dados](/01-prerequisites-programming-basics/03-number-systems-data-representation.md)