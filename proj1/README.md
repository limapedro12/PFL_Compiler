# Shakti

## Visão Geral

Shakti é um jogo de tabuleiro estratégico. Este jogo é uma variante do xadrez que é jogada em um tabuleiro de 7x7. Ambos os jogadores têm 1 Rei e 2 Guerreiros. A característica única deste jogo é que durante o jogo, derivado de movimentos específicos, algumas casas são removidas do tabuleiro, reduzindo assim a área de jogo.

No nosso caso, o Shakti foi implementado em Prolog, projetado para ser jogado na linha de comando (no SICStus), no ambito da unidade curricular "Programação Funcional e em Lógica"(L.EIC024), do 1º semestre do 3º ano curricular da Licenciatura em Engenharia Informática e Computação, da Faculdade de Engenharia e Faculdade de Ciências da Universidade do Porto.

## Regras do Jogo

### Objetivo

O objetivo do Shakti é vencer o oponente capturando o rei adversário. Para isso acontecer o jogador tem que conseguir dar um "xeque-mate", uma situação em que o rei adversário não tem nenhuma jogada legal para escapar à ameaça. 

### Componentes do Tabuleiro

- Tabuleiro 7x7.
- Dois jogadores: branco(peças: "u", "b", "d") e preto(peças: "n", "p", "q").
- Cada jogador começa com um rei("u" ou "n") e dois guerreiros("b", "d" ou "p", "q").

### Movimentos

- O rei pode mover-se em qualquer direção, uma casa de cada vez.
- Os guerreiros podem mover-se em qualquer direção, uma ou duas casas de cada vez. 
Os guerreiros so podem mover duas casas duma vez se não existir nenhuma peça entre a posição inicial e a posição final.
Quando se movem duas casas, a casa intermédia é removida do tabuleiro.
- As peças podem saltar sobre as casas removidas, mas não podem parar na mesma.

### Captura

- O objetivo é capturar o rei do oponente, deixando-o em uma posição onde ele não tem nenhuma jogada legal para escapar à ameaça. 

### Vitória

- O jogador que capturar o rei do oponente primeiro vence o jogo.

## Como Jogar

1. Baixe os arquivos do jogo para sua máquina local.

3. Abra o terminal do SICStus Prolog e consulte o ficheiro `tabuleiro.pl`

4. Inicie o jogo executando:

   ```
   play.
   ```

5. Siga as instruções na linha de comando para fazer movimentos no jogo.

6. Jogue contra outro jogador ou contra um adversário controlado por IA(com duas dificuldades: nivel 1 e nivel 2).

## Créditos

O jogo Shakti foi desenvolvido por:
- Pedro Januário - up202108768@up.pt
- Pedro Lima - up202108806@up.pt

Divirta-se a jogar Shakti e boa sorte!
