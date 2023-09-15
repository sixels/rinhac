# rinhac

Compilador AOT para a Rinha de Compiladores.

## Instruções de uso \[TODO: criar uma imagem docker\]

Este compilador ainda está em desenvolvimento inicial, para testar algum programa instale as dependências e rode o executável do programa.
O compilador espera como argumento o caminho para um arquivo de AST em JSON e gera de objeto `.o`, que precisa ser linkado com a runtime da linguagem.

### Exemplo

```sh
cargo run -- ./files/print.rinha.json
gcc -c print.o target/debug/librinha_rt.a -o ./print
chmod u+x  ./print
./print
```

## Dependências

- llvm-15.0 (TODO: linkar estático)
- libpolly-15
- [rinha](https://github.com/aripiprazole/rinha-de-compiler) (apenas para desenvolvimento) - gera uma AST JSON a partir de um source `.rinha`.

### Instalando as dependências

#### Ubuntu

```sh
sudo apt install llvm-15-dev libpolly-15-dev
```
