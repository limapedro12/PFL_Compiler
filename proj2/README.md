# Segundo Trabalho Prático de PFL 2023

It should also describe the strategy used for solving both parts of the assignment, detailing the decisions made when defining the data and functions defined in the program.

## Grupo e Distribuição de Trabalho

Grupo **T05_G01**:

- Pedro de Almeida Lima (<a href="mailto:up202108806@up.pt">up202108806@up.pt</a>) - contributo de 50%
- Pedro Simão Januário Vieira (<a href="mailto:up202108768@up.pt">up202108768@up.pt</a>) - contributo de 50%

## Âmbito do Trabalho e Problema

O problema proposto para abordagem no trabalho a que o presente documento diz respeito centra-se numa pequena linguagem de programação imperativa.

Num primeiro momento, foi implementado um _assembler_ que, dadas instruções em código análogo ao Assembly para as operações oferecidas pela pequena linguagem, é capaz de simular a execução do código, recorrendo a estruturas de dados que fornecem abstrações para uma pilha (_stack_), um pequeno armazenamento (_state_) e a própria sequência de instruções.

Já numa segunda parte, desenvolveu-se um _parser_ e um compilador responsáveis por converter código-fonte da pequena linguagem de programação para o referido código-máquina a ser interpretado pela solução desenvolvida para a primeira parte.

## Detalhes da Implementação

### Primeira parte &mdash; _Assembler_

Tendo em conta que o propósito do _assembler_ é o de processar instruções em _assembly_, é fundamental uma estrutura de dados que represente as ditas instruções. Tal é assegurado por ```data Inst```, como fornecido no modelo para o código do trabalho. Um conjunto de instruções é representado por ```type Code = [Inst]```.

Dada a necessidade de suporte para variáveis com nome e conteúdo, recorreu-se a ```type Var = (VarName, VarValue)```, que, para representar uma variável, consiste num par composto pelo nome (```type VarName = String```) e pelo conteúdo (```data VarValue```) dessa variável. Esta última estrutura distingue o conteúdo entre inteiros e booleanos.

Para responder à necessidade de ter uma pilha (_stack_) auxiliar à execução do código, bem como algo que cumpra o propósito de memória/armazenamento para os programas (_state_), foram criadas, respetivamente, ```type Stack = [VarValue]``` e ```type State = [Var]```. Recordando que ```Var``` engloba o nome e o conteúdo de uma variável, o tipo apenas foi usado no âmbito do estado, dado que não é esperado nem desejável que a pilha contenha tal informação.

Foram, ainda, definidas funções que permitem visualizar os conteúdos da pilha e da memória de uma forma mais inteligível (```stack2Str :: Stack -> String``` e ```state2Str :: State -> String```, respetivamente), bem como funções que inicializam uma instância de cada uma das duas estruturas (```createEmptyStack :: Stack``` e ```createEmptyState :: State```, respetivamente).

Implementou-se funções de manipulação da pilha (```top``` (obter o elemento no topo), ```pop``` (remover o elemento no topo) e ```push``` (colocar um elemento no topo da pilha)).

Concretizámos as funções que efetivamente cumprem o papel das instruções do código _assembly_, conforme definidas em ```data Inst```, fazendo os cálculos necessários e manipulando a pilha e o estado. Para cada instrução, foi criada uma função com o mesmo nome, à exceção da instrução de ```Loop```, uma vez que pode ser reescrita recursivamente como uma instrução ```Branch``` que encapsula o ```Loop```.

Para o descrito efeito, algumas funções principais recorrem a funções auxiliares, de maneira a separar as preocupações e facilitar a leitura (e escrita) do código, permitindo que uma função que desempenhe o papel de uma instrução possa ser lida como uma abstração de mais alto nível tendo em conta a operação que se realiza, "escondendo" em funções auxiliares os meandros da operação.

Tomemos como exemplo as funções afetas à instrução de adição (```Add```):

```haskell
addValues :: VarValue -> VarValue -> VarValue
addValues (IntegerValue a) (IntegerValue b) = IntegerValue (a + b)
addValues _ _ = error "Run-time error"

add :: Stack -> Stack
add stack = push result (pop (pop stack))
            where result = top stack `addValues` top (pop stack)
```

No caso, podemos constatar que o trabalho de verificação de verificação da viabilidade da operação (dado que, concretamente, apenas podemos adicionar dois inteiros e não um inteiro e um booleano, ou dois booleanos) e de efetuar a adição em si é deixado para ```addValues```, ficando a função primária, ```add```, com a responsabilidade de manipular a pilha recorrendo ao resultado da operação.

Por último, temos a função ```run :: (Code, Stack, State) -> (Code, Stack, State)``` que garante o fluxo de processamento das instruções desejadas. É recursiva; para uma dada instrução, após invocar a função correspondente, chama-se a si própria com as instruções restantes, bem como a pilha e o estado resultantes da operação efetuada.

### Segunda parte &mdash; Compilador e _Parser_

#### Compilador

Lorem ipsum dolor sit amet.

#### _Parser_

Lorem ipsum dolor sit amet.

### Destaque

Lorem ipsum dolor sit amet.

### Casos de uso principais e Exemplos

Lorem ipsum dolor sit amet.

### Conclusões

Lorem ipsum dolor sit amet.

***
Grupo T05_G01, 01/01/2024
