# SimpleORM - Field Mapping (issue #83)

Demonstra a correcao da issue #83 (**Field Map Error**): o mapeamento de um
`DataSet` para a entidade agora trata corretamente os tipos `TDate`, `Boolean`
(vindo de `tinyint`/`integer` no MySQL), `Double` e `Int64`, incluindo colunas
`NULL` — que antes provocavam erro de conversao ao chamar `Find(Lista)`.

## O que o sample faz

1. Cria um `TFDMemTable` (FireDAC em memoria) simulando o retorno de um SELECT
   no MySQL, com um registro completo e outro com todas as colunas nao-chave
   `NULL`.
2. Usa `TSimpleRTTI<T>.DataSetToEntityList` — o mesmo caminho interno de
   `TSimpleDAO<T>.Find(Lista)` — para mapear o dataset para a lista de entidades.
3. Imprime os valores mapeados, mostrando que nenhum "Field Map Error" ocorre.

## Como executar

1. Abra `SimpleORMFieldMapping.dpr` no Delphi (a IDE gera `.dproj` e `.res`).
2. Compile e execute (console).

Nenhum banco de dados real e necessario — o `TFDMemTable` roda em memoria.

## Tipos exercitados

| Tipo Delphi | Coluna simulada    | Observacao                              |
|-------------|--------------------|-----------------------------------------|
| `Integer`   | `ftInteger`        | Chave primaria                          |
| `String`    | `ftString`         | -                                       |
| `TDate`     | `ftDate`           | Mapeado via `AsDateTime`                |
| `Double`    | `ftFloat`          | Mapeado via `AsFloat`                   |
| `Boolean`   | `ftInteger` (0/1)  | MySQL `tinyint(1)` -> `Boolean`         |
| colunas NULL| qualquer           | Ignoradas (mantem default), sem erro    |
