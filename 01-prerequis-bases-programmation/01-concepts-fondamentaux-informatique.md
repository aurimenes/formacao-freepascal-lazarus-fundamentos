🔝 Voltar para [Resumo](/SUMMARY.md)

# 1.1 Conceitos Fundamentais de Informática

## Introdução

Antes de se aprofundar na programação com FreePascal e Lazarus, é essencial entender alguns conceitos básicos de informática. Esta seção fornecerá a base necessária para começar.

## O que é um computador?

Um computador é uma máquina eletrônica capaz de:
- **Receber** informações (dados de entrada)
- **Processar** essas informações de acordo com instruções específicas
- **Armazenar** informações na memória
- **Retornar** resultados (dados de saída)

Ao contrário de uma calculadora simples, um computador pode executar uma ampla variedade de tarefas diferentes simplesmente alterando as instruções fornecidas.

## Os Componentes de um Computador

Um computador é composto por duas categorias principais de componentes:

### Hardware

Estes são os componentes físicos que você pode tocar:

#### O Processador (CPU - Unidade Central de Processamento)
- Este é o "cérebro" do computador
- Ele executa instruções de programas
- Sua velocidade é medida em GHz (Gigahertz)
- Quanto mais rápido, mais operações ele pode processar por segundo

#### Memória de Acesso Aleatório (RAM - Random Access Memory)
- Esta é a memória de trabalho temporária
- Ela armazena programas e dados em uso
- Seu conteúdo é apagado quando o computador é desligado
- Quanto mais RAM você tiver, mais coisas você pode fazer simultaneamente

#### Armazenamento (Disco Rígido, SSD)
- Esta é a memória permanente
- Ela mantém seus arquivos mesmo quando o computador está desligado
- Os programas são armazenados aqui antes de serem carregados na RAM para serem executados

#### Dispositivos de Entrada/Saída
- **Entrada**: teclado, mouse, Microfone, webcam
- **Saída**: tela, impressora, alto-falantes
- **Entrada/Saída**: tela sensível ao toque, unidade USB, rede

### Software

Estes são os programas, as instruções intangíveis:

#### Sistema Operacional (SO)
- Este é o software básico que gerencia o computador
- Exemplos: Windows, Linux (Ubuntu), macOS
- Ele conecta o hardware e outros programas
- Ele gerencia arquivos, memória e periféricos

#### Aplicativos
- Estes são os programas que você usa diariamente
- Exemplos: navegador da web, processador de texto, jogos
- Cada aplicativo é projetado para uma tarefa específica

#### Drivers
- Estes são pequenos programas que permitem que o sistema operacional se comunique com o hardware
- Cada periférico (impressora, placa de vídeo, etc.) precisa de seu próprio Pilot

## Linguagem binária: a base de tudo

### Por que binário?

Os computadores só entendem uma coisa: eletricidade. Um componente eletrônico pode ter dois estados:
- **Corrente fluindo**: estado "1" (verdadeiro)
- **Corrente não fluindo**: estado "0" (falso)

É por isso que os computadores usam o **sistema binário**, que usa apenas dois dígitos: 0 e 1.

### Bits e Bytes

**O Bit** (dígito binário)
- É a menor unidade na computação
- Um bit pode ser 0 ou 1
- É como um interruptor: ligado ou desligado

**O Byte**
- Um byte = 8 bits
- Exemplo: 10110011 é um byte
- Um byte pode representar 256 valores diferentes (de 0 a 255)

**Unidades de Medida**
- 1 quilobyte (KB) = 1.024 bytes
- 1 megabyte (MB) = 1.024 KB = aproximadamente 1 milhão de bytes
- 1 gigabyte (GB) = 1.024 MB = aproximadamente 1 bilhão de bytes
- 1 terabyte (TB) = 1.024 GB = aproximadamente 1 trilhão de bytes

## Como um programa funciona?

### Do Código-Fonte para um Programa Executável

1. **Escrevendo o Código-Fonte**
- O programador escreve instruções em uma linguagem de programação (como Pascal)
- Este código é legível por humanos
- Ele é armazenado em um arquivo de texto

2. **Compilação**
- Um programa especial chamado "compilador" traduz o código-fonte
- Ele o transforma em linguagem de máquina (binário)
- O resultado é um arquivo executável (.exe no Windows, sem extensão no Linux)

3. **Execução**
- O usuário inicia o programa
- O sistema operacional carrega o programa na RAM
- O processador executa as instruções uma a uma

### Linguagem de Máquina vs. Linguagem de Alto Nível

**Linguagem de Máquina (ou Assembler)**
- Esta é a linguagem que o processador entende diretamente
- Muito difícil para um humano ler e escrever
- Exemplo: `MOV AX, 5` (move o valor 5 para um registrador)

**Linguagem de Alto Nível nível**
- Projetado para ser facilmente lido e escrito por humanos
- Exemplos: Pascal, C, Python, Java
- Deve ser traduzido para linguagem de máquina para ser executado
- Exemplo em Pascal: `x := 5;` (atribui o valor 5 à variável x)

## Dados e sua representação

### Tipos básicos de dados

Na programação, os dados podem ser de diferentes tipos:

**Inteiros**
- Exemplos: -5, 0, 42, 1000
- Usado para contagem, numeração, etc.

**Números de Ponto Flutuante**
- Exemplos: 3,14, -0,5, 2,71828
- Usado para cálculos científicos, preços, etc.

**Caracteres e Texto**
- Caractere único: 'A', 'z', '5', '$'
- Texto único (string): "Olá", "Pascal", "123"

**Booleanos**
- Apenas dois valores possíveis: VERDADEIRO ou FALSO
- Usado para decisões e testes lógicos

### Como os dados são armazenados?

Todos os dados são armazenados em formato binário na memória, mas em formatos diferentes:

**Inteiros**
- Um inteiro é armazenado diretamente em formato binário.
- Exemplo: o número 5 em binário = 00000101 (formato de 8 bits).

**Caracteres**
- Cada caractere possui um código numérico.
- Tabela ASCII: 'A' = 65, 'B' = 66, 'a' = 97, '0' = 48, etc.
- Em Unicode (mais moderno): pode representar todos os alfabetos do mundo.

**Números de Ponto Flutuante**
- Armazenados em notação científica (mantissa e expoente).
- Formato padronizado: IEEE 754.

## A Importância dos Algoritmos

### O que é um Algoritmo?

Um algoritmo é uma sequência de instruções precisas e ordenadas que resolvem um problema ou executam uma tarefa.

**Características de um bom algoritmo:**
- **Preciso**: cada etapa deve ser clara e inequívoca
- **Finito**: deve ser concluído após um número finito de etapas
- **Eficiente**: deve resolver o problema em um tempo razoável

### Exemplo simples: fazer chá

Aqui está um algoritmo para fazer chá (em linguagem natural):

```
1. Encha a chaleira com água
2. Ligue a chaleira
3. Espere a água ferver
4. Coloque um sachê de chá em uma xícara
5. Despeje a água fervente na xícara
6. Aguarde de 3 a 5 minutos
7. Retire o sachê de chá
8. Adicione açúcar, se desejar
9. Finalize
```

Um programa de computador segue o mesmo princípio: é uma série de instruções executadas em uma ordem específica.

## Lógica de Programação

### As Três Estruturas Básicas

Qualquer programa pode ser construído com apenas três estruturas fundamentais:

**1. Sequência**
- Execução de instruções uma após a outra
- Exemplo: Primeiro A, depois B, depois C

**2. Seleção (ou Condição)**
- Escolha entre diferentes ações com base em uma condição
- Exemplo: SE estiver chovendo, ENTÃO leve um guarda-chuva, CASO CONTRÁRIO, não leve nada

**3. Repetição (ou Loop)**
- Repetição de uma ação várias vezes
- Exemplo: ENQUANTO houver páginas restantes, leia a próxima página

Essas três estruturas serão estudadas em detalhes nos próximos capítulos.

## Abstração e Resolução de Problemas

### O Conceito de Abstração

Abstração consiste em simplificar um problema complexo por meio de:
- Ignorar detalhes não essenciais
- Focar no que é importante
- Dividir o problema em partes menores

**Exemplo:**
Quando você dirige um carro, não precisa entender detalhadamente como o motor funciona. Você usa uma interface simplificada: volante, pedais, alavanca de câmbio.

### Decomposição do Problema

Para resolver um problema complexo:
1. **Divida** o problema em subproblemas menores
2. **Resolva** cada subproblema separadamente
3. **Monte** as soluções para obter a solução completa

Essa abordagem é chamada de "dividir para conquistar" e é fundamental para a programação.

## Conclusão

Agora você tem uma compreensão básica de como um computador funciona e dos conceitos fundamentais da ciência da computação. Esses conceitos serão úteis ao longo de sua jornada de programação.

Principais conclusões:
- Um computador é composto de hardware e software
- Tudo é armazenado em binário (0s e 1s) na memória
- Um programa é uma sequência de instruções traduzidas para linguagem de máquina
- Algoritmos são o cerne da programação
- A lógica de programação é baseada em três estruturas: sequência, seleção e repetição

Na próxima seção, aprenderemos mais sobre o que é um programa e como ele é estruturado.

⏭️ [O que é um programa?](/01-prerequisites-programming-basics/02-what-is-a-program.md)