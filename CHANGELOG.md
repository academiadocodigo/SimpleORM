# Changelog

Todas as mudancas notaveis neste projeto serao documentadas neste arquivo.

O formato e baseado em [Keep a Changelog](https://keepachangelog.com/pt-BR/1.1.0/).

> **OBRIGATORIO:** Todo novo recurso, correcao ou modificacao DEVE ser documentado aqui antes do commit.

---

## [Unreleased]

### Changed
- **Drivers opcionais protegidos por diretiva** - SimpleQueryRestDW, SimpleQueryUnidac e SimpleQueryZeos agora tem todo o corpo envolto em {$IFDEF USE_RESTDW}/{$IFDEF USE_UNIDAC}/{$IFDEF USE_ZEOS}. Sem o simbolo definido, compilam como unit vazio valido, evitando falha do SimpleORM.dpk em ambientes sem RestDataware/UniDAC/Zeos instalados (issue #82) (SimpleQueryRestDW.pas, SimpleQueryUnidac.pas, SimpleQueryZeos.pas)

### Fixed
- **Field Map Error no mapeamento DataSet -> entidade** - DataSetToEntity/DataSetToEntityList agora ignoram colunas NULL antes de converter, usam AsLargeInt para Int64 (evita overflow em BIGINT), mapeiam TDate/TTime/TDateTime via AsDateTime e ignoram tipos nao suportados em vez de gravar TValue vazio. Corrige erro de conversao ao dar Find em entidades com TDate/Boolean/Double no MySQL (issue #83) (SimpleRTTI.pas)



### Added
- **Exists** - Metodo DAO.Exists(campo, valor) para verificar existencia de registro sem carregar entidade (SimpleDAO.pas)
- **Automapping** - Atributo [Automapping] para convencao sobre configuracao: classe sem [Tabela] deriva nome da tabela automaticamente (SimpleAttributes.pas, SimpleRTTI.pas)
- **TSimpleEventBus** - Sistema global de eventos/observers para Insert/Update/Delete com Subscribe por classe ou global (SimpleEvents.pas)
- **BulkInsert** - INSERT otimizado com multiplos VALUES em unico SQL, batches de 100, suporte a Enumerator/UUID (SimpleDAO.pas)
- **FindAs** - Mapeamento de resultado SQL para classes DTO sem [Tabela], usando RawSQL ou query builder (SimpleDAO.pas)
- **TSimpleNLQuery** - Natural Language Query: pergunta em linguagem natural gera SQL via LLM e executa automaticamente, com validacao de seguranca contra DDL/injection (SimpleNLQuery.pas)
- **Ask** - Metodo DAO.Ask(pergunta) para Natural Language Query integrado ao DAO com AIClient (SimpleDAO.pas)
- **TSimpleSwagger** - Geracao automatica de spec OpenAPI 3.0 a partir de entidades registradas via RTTI (SimpleSwagger.pas)
- **EnableSwagger** - Integracao Horse: GET /swagger.json auto-gerado com TSimpleHorseRouter.EnableSwagger (SimpleHorseRouter.pas)
- **TSimpleSeeder** - Smart Seeder: geracao de dados de teste realistas via LLM baseado no schema RTTI da entidade (SimpleSeeder.pas)
- **TSimpleAutoIndex** - AI Auto-Index: analise de queries lentas com sugestao automatica de CREATE INDEX via LLM (SimpleAutoIndex.pas)
- **TSimpleQueryLoggerCollector** - Logger que coleta queries em memoria para analise pelo AutoIndex (SimpleAutoIndex.pas)
- **TSimpleQueryOptimizer** - AI Query Optimizer: analise e sugestao de otimizacao de SQL via LLM (SimpleQueryOptimizer.pas)
- **TSkillQueryOptimizer** - Skill wrapper para Query Optimizer integravel ao DAO (SimpleQueryOptimizer.pas)
- **TSkillTelegram** - Skill para envio de notificacoes via Telegram Bot API (SimpleSkillMessaging.pas)
- **TSkillDiscord** - Skill para envio de notificacoes via Discord Webhook (SimpleSkillMessaging.pas)
- **TSimpleExportSheets** - Exportacao de entidades para Google Sheets via API REST (SimpleExportSheets.pas)

### Fixed
- **SimpleLogger** - Inclusao de System.Variants para VarToStr e qualificacao de Format como System.SysUtils.Format (SimpleLogger.pas)
- **SimpleTypes** - Inclusao de System.SysUtils para resolver declaracao de Exception em TSimpleErrorCallback (SimpleTypes.pas)
- **SimpleInterface** - Forward declaration de iSimpleAIClient para garantir compatibilidade de referencias (SimpleInterface.pas)
- **SimpleDAO** - Movidas units SimpleTypes e SimpleSkill para uses da interface para visibilidade de TSimpleErrorCallback e TSimpleSkillRunner (SimpleDAO.pas)
- **SimpleValidator** - Qualificacao de todas as chamadas Format para System.SysUtils.Format evitando conflito com SimpleAttributes.Format (SimpleValidator.pas)
- **SimpleSkill** - Inclusao de System.Classes e Winapi.Windows para TStringStream e OutputDebugString (SimpleSkill.pas)
- **SimpleRTTI** - Remocao de redeclaracao de System.TypInfo na implementation uses (SimpleRTTI.pas)
- **SimpleRTTI** - Variaveis inline LOrdinal movidas para bloco var em DataSetToEntity e DataSetToEntityList (SimpleRTTI.pas)
- **SimpleRTTI** - Funcao TryStrToEnumOrdinal promovida para interface para corrigir erro E2506 em generics (SimpleRTTI.pas)
- **SimpleAIProcessor** - Inclusao de SimpleAttributes na interface uses para visibilidade dos atributos AI (SimpleAIProcessor.pas)
- **SimpleAgent** - Inclusao de System.Classes para TStringList e variaveis inline movidas para bloco var em Plan (SimpleAgent.pas)
- **SimpleRules** - Variavel inline LFloat movida para bloco var em ParseSimpleExpression (SimpleRules.pas)
- **SimpleRTTIHelper** - Unit RTTI qualificada para System.Rtti (SimpleRTTIHelper.pas)
- **SimpleValidator** - Unit RTTI qualificada para System.Rtti (SimpleValidator.pas)
- **SimpleRTTI** - Unit TypInfo qualificada para System.TypInfo e Variants para System.Variants (SimpleRTTI.pas)
- **SimpleDAO** - Unit Variants qualificada para System.Variants (SimpleDAO.pas)
- **SimpleUtil** - Unit SysUtils qualificada para System.SysUtils, removida dependencia FireDAC.Comp.Client desnecessaria (SimpleUtil.pas)
- **SimpleJSON** - Units Classes e SysUtils qualificadas para System.Classes e System.SysUtils (SimpleJSON.pas)
- **SimpleUtil** - DisableControls/EnableControls protegidos com try/finally em DataSetToObjectList (SimpleUtil.pas)
- **SimpleDAO** - DisableControls/EnableControls protegidos com try/finally em Find (SimpleDAO.pas)
- **SimpleSupabaseAuth** - Excecao de refresh de token agora loga mensagem em vez de ser engolida silenciosamente (SimpleSupabaseAuth.pas)
- **SimpleSupabaseRealtime** - Erros de polling agora logam mensagem em vez de serem engolidos silenciosamente (SimpleSupabaseRealtime.pas)
- **SimpleAISkill** - Adicionado metodo RunMode faltante nas 7 classes que implementam iSimpleSkill (SimpleAISkill.pas)
- **SimpleDAO.DataSetToJSON** - Corrigida chamada a metodo inexistente DictionaryFieldClass, substituido por RTTI inline (SimpleDAO.DataSetToJSON.pas)
- **SimpleMCPServer** - Corrigido destructor usando FreeAndNil em vez de Free (SimpleMCPServer.pas)
- **SimpleMigration** - Qualificacao de SysUtils.Format para System.SysUtils.Format (SimpleMigration.pas)
- **SimpleDAO** - Corrigido RelObj.Free para FreeAndNil(RelObj) em LoadRelationships (SimpleDAO.pas)
- **SimpleQueryZeos** - ExecSQL agora faz Rollback antes de re-raise em caso de erro, seguindo padrao do FireDAC (SimpleQueryZeos.pas)
- **SimpleQueryUnidac** - ExecSQL agora faz Rollback antes de re-raise em caso de erro, seguindo padrao do FireDAC (SimpleQueryUnidac.pas)
- **SimpleHorseRouter** - Corrigido memory leak de TSimpleHorseRouterConfig com gerenciamento via class var e finalization (SimpleHorseRouter.pas)
- **SimpleInterface** - Corrigida convencao de nomenclatura ISimpleRTTI para iSimpleRTTI (SimpleInterface.pas)
- **SimpleRTTI** - Corrigida convencao de nomenclatura ISimpleRTTI para iSimpleRTTI (SimpleRTTI.pas)

## [3.06.00] - 2026-03-10

### Added
- **TSimpleDataMigration** - Framework fluent para migracao de dados entre bancos/sistemas (SimpleDataMigration.pas)
- **TFieldMap** - Mapeamento tabela-a-tabela com Field, Transform, DefaultValue, Lookup e Ignore (SimpleDataMigration.pas)
- **TFieldTransform** - Transformacoes built-in: Upper, Lower, Trim, Replace, DateFormat, Split, Concat, Custom (SimpleDataMigration.pas)
- **TMigrationReport** - Relatorio estruturado com TotalRecords, Migrated, Failed, Skipped, ToJSON, ToCSV (SimpleDataMigration.pas)
- **TTableReport** - Relatorio por tabela com lista de erros detalhada (SimpleDataMigration.pas)
- **TCSVReader** - Leitor de CSV com headers e iteracao sequencial (SimpleDataMigration.pas)
- **TCSVWriter** - Escritor de CSV com headers e flush (SimpleDataMigration.pas)
- **TMigrationFormat** - Enum para formato de fonte/destino: CSV, JSON (SimpleTypes.pas)
- **TMigrationError** - Record com detalhes de erro por registro (SimpleTypes.pas)
- **Persistencia JSON** - SaveToJSON/LoadFromJSON para reutilizar mapeamentos de migracao
- **Migracao CSV** - Suporte a CSV como fonte e/ou destino de migracao
- **Callbacks de progresso** - OnProgress e OnError para controle durante migracao
- **Sample DataMigration** - Projeto demonstrando API fluent, transformacoes e relatorio (samples/DataMigration/)

## [3.05.00] - 2026-03-10

### Added
- **TSkillRunMode** - Enum para modo de execucao de Skills: Normal e OnError (SimpleTypes.pas)
- **TSimpleErrorCallback** - Tipo callback para tratamento de erros no DAO (SimpleTypes.pas)
- **RunMode** - Metodo em iSimpleSkill para controlar modo de execucao Normal/OnError (SimpleInterface.pas)
- **ErrorMessage** - Metodo em iSimpleSkillContext para acessar mensagem de erro (SimpleInterface.pas)
- **RunOnError** - Metodo no TSimpleSkillRunner para executar skills em modo OnError (SimpleSkill.pas)
- **OnError** - Callback generico no TSimpleDAO para tratamento de erros em Insert/Update/Delete (SimpleDAO.pas)
- **TSkillGitHubIssue** - Skill para criacao automatica de Issues no GitHub via REST API (SimpleSkill.pas)
- **Sample GitHubIssue** - Projeto demonstrando uso do TSkillGitHubIssue (samples/GitHubIssue/)

## [3.04.00] - 2026-03-10

### Added
- **Rule** - Atributo declarativo para regras de negocio deterministicas na entidade (SimpleRules.pas)
- **AIRule** - Atributo para regras de negocio inteligentes avaliadas por LLM (SimpleRules.pas)
- **TSimpleRuleEngine** - Motor de avaliacao de regras com parser de expressoes simples
- **ESimpleRuleViolation** - Excecao para violacoes de regras de negocio
- **iSimpleSkill** - Interface para plugins reutilizaveis no pipeline do DAO
- **iSimpleSkillContext** - Contexto com Query, AIClient e Logger disponivel para Skills
- **TSkillLog** - Skill built-in para logging de operacoes
- **TSkillNotify** - Skill built-in para callbacks/notificacoes
- **TSkillAudit** - Skill built-in para auditoria em tabela do banco
- **TSimpleAgent** - Agente com modo reativo (When/Condition/Execute) e proativo (Plan/Execute via LLM)
- **iAgentResult** - Interface para resultados de execucao de agentes
- **iAgentPlan** - Interface para planos de execucao com analise de risco
- **Pipeline DAO** - Integracao de Rules, Skills e Agents no pipeline Insert/Update/Delete do TSimpleDAO
- **Sample AgentsSkillsRules** - Projeto demonstrando Rules, Skills e Agents
- **TSkillTimestamp** - Skill built-in para preencher campos de data automaticamente via RTTI (`SimpleSkill.pas`)
- **TSkillGuardDelete** - Skill built-in para bloquear delete quando existem registros dependentes (`SimpleSkill.pas`)
- **TSkillHistory** - Skill built-in para gravar snapshot de valores antes de update/delete (`SimpleSkill.pas`)
- **TSkillValidate** - Skill built-in para validacao automatica via TSimpleValidator (`SimpleSkill.pas`)
- **TSkillWebhook** - Skill built-in para HTTP POST fire-and-forget apos operacoes CRUD (`SimpleSkill.pas`)
- **ESimpleGuardDelete** - Exception especifica para bloqueio de delete com dependencias (`SimpleSkill.pas`)
- **Sample BuiltinSkills** - Projeto demonstrando uso das Skills built-in (`samples/BuiltinSkills/`)
- **[CPF]** - Atributo de validacao de CPF brasileiro com algoritmo completo (`SimpleAttributes.pas`)
- **[CNPJ]** - Atributo de validacao de CNPJ brasileiro com algoritmo completo (`SimpleAttributes.pas`)
- **TSkillSequence** - Skill ERP para numeracao sequencial via tabela de controle (`SimpleSkill.pas`)
- **TSkillCalcTotal** - Skill ERP para calculo de total (qtd * preco - desconto) via RTTI (`SimpleSkill.pas`)
- **TSkillStockMove** - Skill ERP para movimentacao de estoque (entrada/saida) (`SimpleSkill.pas`)
- **TSkillDuplicate** - Skill ERP para geracao de parcelas financeiras (`SimpleSkill.pas`)
- **Sample ERPSkills** - Projeto demonstrando Skills ERP e validacao CPF/CNPJ (`samples/ERPSkills/`)
- **SimpleAISkill.pas** - Nova unit com 7 Skills baseadas em IA
- **TSkillAIEnrich** - Skill AI para gerar conteudo via prompt template com `{PropertyName}` (`SimpleAISkill.pas`)
- **TSkillAITranslate** - Skill AI para traducao automatica entre campos (`SimpleAISkill.pas`)
- **TSkillAISummarize** - Skill AI para resumo automatico de texto (`SimpleAISkill.pas`)
- **TSkillAITags** - Skill AI para geracao automatica de tags/keywords (`SimpleAISkill.pas`)
- **TSkillAIModerate** - Skill AI para moderacao de conteudo com bloqueio (`SimpleAISkill.pas`)
- **TSkillAIValidate** - Skill AI para validacao de dados com regra em linguagem natural (`SimpleAISkill.pas`)
- **TSkillAISentiment** - Skill AI para analise de sentimento (POSITIVO/NEGATIVO/NEUTRO) (`SimpleAISkill.pas`)
- **ESimpleAIModeration** - Exception para bloqueio por moderacao/validacao AI (`SimpleAISkill.pas`)
- **Sample AISkills** - Projeto demonstrando as 7 Skills AI com mock client (`samples/AISkills/`)
- **AI Query** - Perguntas em linguagem natural ao banco de dados via LLM (SimpleAIQuery.pas)
- **NaturalLanguageQuery** - Traduz pergunta para SQL, executa e retorna TDataSet
- **AskQuestion** - Traduz, executa e retorna resposta em linguagem natural
- **ExplainQuery** - Explica SQL em linguagem natural via LLM
- **SuggestQuery** - Sugere SQL baseado em objetivo descrito em linguagem natural
- **Validacao SQL AIQuery** - Bloqueio automatico de operacoes nao-SELECT em queries geradas por LLM
- **Sample AIQuery** - Projeto demonstrando AI Query com mock client
- **iSimpleAIClient** - Interface generica para comunicacao com LLMs Claude e OpenAI (SimpleAIClient.pas)
- **TSimpleAIClient** - Client HTTP para APIs de LLM com suporte a Claude e OpenAI (SimpleAIClient.pas)
- **AIGenerated** - Atributo para geracao automatica de conteudo via LLM com template de prompt (SimpleAIAttributes.pas)
- **AISummarize** - Atributo para resumo automatico de propriedades via LLM (SimpleAIAttributes.pas)
- **AITranslate** - Atributo para traducao automatica de propriedades via LLM (SimpleAIAttributes.pas)
- **AIClassify** - Atributo para classificacao automatica de conteudo via LLM (SimpleAIAttributes.pas)
- **AIValidate** - Atributo para validacao de conteudo via LLM (SimpleAIAttributes.pas)
- **TSimpleAIProcessor** - Motor de processamento que detecta atributos AI e executa via LLM (SimpleAIProcessor.pas)
- **AIClient** - Metodo no TSimpleDAO para integrar AI automaticamente em Insert/Update
- **Sample AIEnrichment** - Projeto demonstrando AI Entity Enrichment com mock client
- **SimpleSerializer** - Serializador Entity <-> JSON via RTTI usando atributos `[Campo]`, sem dependencias externas (`SimpleSerializer.pas`)
- **SimpleHorseRouter** - Auto-geracao de rotas CRUD no Horse a partir de entidades SimpleORM com callbacks opcionais OnBeforeInsert/OnAfterInsert/OnBeforeUpdate/OnBeforeDelete (`SimpleHorseRouter.pas`)
- **SimpleQueryHorse** - Driver REST cliente que implementa `iSimpleQuery` via HTTP, com suporte a Bearer token e hook `OnBeforeRequest` (`SimpleQueryHorse.pas`)
- **Sample horse-integration** - Exemplos de servidor (HorseServer.dpr) e cliente (HorseClient.dpr) usando a integracao Horse
- **DUnit Test Suite** - Suite completa de testes unitarios com 99 testes cobrindo SimpleAttributes, SimpleRTTIHelper, SimpleSQL, SimpleValidator e SimpleSerializer (`tests/SimpleORMTests.dpr`)
- **Regra de testes obrigatorios** - Toda nova feature deve incluir testes DUnit (`.claude/rules/testing.md`)
- **TSimpleQuerySupabase** - Novo driver `iSimpleQuery` para conexao direta com Supabase via PostgREST API (`SimpleQuerySupabase.pas`)
- **Supabase CRUD** - Suporte completo a INSERT (POST), UPDATE (PATCH), DELETE (DELETE) e SELECT (GET) via REST
- **Supabase Auth** - Suporte a API Key (service_role) e JWT token para Row Level Security
- **Supabase Paginacao** - Traducao automatica de Skip/Take para query params `limit`/`offset`
- **Supabase Sample** - Projeto exemplo demonstrando CRUD com Supabase (`samples/Supabase/`)
- **Entidade.Produto** - Entidade compartilhada TProduto para uso em samples (`samples/Entidades/Entidade.Produto.pas`)
- **TSimpleSupabaseAuth** - Autenticacao Supabase com SignIn, SignUp, SignOut e RefreshToken (`SimpleSupabaseAuth.pas`)
- **Supabase Auto-Refresh** - Token JWT renovado automaticamente quando proximo da expiracao
- **Supabase Auth + Query** - Novo construtor `TSimpleQuerySupabase.New(url, key, auth)` para integrar autenticacao com queries
- **TSimpleSupabaseRealtime** - Monitoramento de mudancas em tabelas Supabase com callbacks (`SimpleSupabaseRealtime.pas`)
- **Supabase Realtime Events** - Callbacks globais (OnInsert/OnUpdate/OnDelete) e por tabela (OnChange)
- **TSupabaseRealtimeEvent** - Record com Table, EventType, OldRecord e NewRecord para notificacoes

## [2.0.0] - 2026-03-08

### Added
- **NotZero** - Novo atributo de validacao para impedir valores zero (separado de NotNull)
- **Controle de transacoes** - Metodos `StartTransaction`, `Commit`, `Rollback`, `InTransaction` em `iSimpleQuery`, implementados nos 4 drivers (FireDAC, UniDAC, Zeos, RestDW)
- **SQLType** - Propriedade em `iSimpleQuery` para identificar o banco (Firebird, MySQL, SQLite, Oracle)
- **Paginacao** - Metodos `Skip(n)` e `Take(n)` em `iSimpleDAOSQLAttribute` com geracao de SQL especifica por banco (FIRST/SKIP, LIMIT/OFFSET, FETCH NEXT)
- **ForeignKey em relacionamentos** - Construtor `Create(aEntityName, aForeignKey)` na classe base `Relationship`
- **Eager loading** - Carregamento automatico de entidades `HasOne`/`BelongsTo` no `Find`
- **Lazy loading** - `TSimpleLazyLoader<T>` em `SimpleProxy.pas` para relacionamentos `HasMany`
- **Validacao expandida** - Novos atributos `Email`, `MinValue`, `MaxValue`, `Regex`; validacao de `Format` (MaxSize/MinSize) agora funcional
- **Soft Delete** - Atributo `SoftDelete('CAMPO')` para exclusao logica; `ForceDelete` para exclusao fisica; filtro automatico no `SELECT`
- **Batch Operations** - Metodos `InsertBatch`, `UpdateBatch`, `DeleteBatch` com transacao automatica
- **Query Logging** - Interface `iSimpleQueryLogger` com implementacao `TSimpleQueryLoggerConsole`; metodo `Logger(...)` em `iSimpleDAO`
- **SimpleProxy.pas** - Nova unit com `TSimpleLazyLoader<T>`
- **SimpleLogger.pas** - Nova unit com `iSimpleQueryLogger` e `TSimpleQueryLoggerConsole`
- **.claudeignore** - Arquivo para excluir binarios e configs do contexto do Claude Code

### Fixed
- **SQL Injection** - Metodo `Delete(aField, aValue)` agora usa query parametrizada em vez de concatenacao
- **Excecoes engolidas** - `SimpleQueryFiredac.ExecSQL` agora re-lanca excecoes apos rollback
- **EndTransaction sem retorno** - `SimpleQueryFiredac.EndTransaction` agora retorna `Result := Self`
- **Error handling nos drivers** - Adicionado tratamento de erro em RestDW (verifica `aErro`), UniDAC e Zeos (try/except com re-raise)
- **NotNull para Integer** - Valor 0 nao e mais tratado como nulo (usar `NotZero` para esse comportamento)
- **Variavel nao utilizada** - Removida variavel `a: string` em `SimpleQueryZeos.pas`

### Changed
- **Transacoes explicitas** - FireDAC nao inicia mais transacao automaticamente no construtor
- **EndTransaction** - Agora delega para `Commit` em todos os drivers
- **Estrutura do repositorio** - Fontes movidos para `src/`, exemplos renomeados de `Sample/` para `samples/`
- **boss.json** - `mainsrc` atualizado de `"./"` para `"./src"`
- **Paths nos projetos** - `.dpk`, `.dpr`, `.dproj`, `.groupproj` atualizados com novos caminhos

### Deprecated
- **SimpleJSON.pas** - Todas as classes marcadas como `deprecated`. Usar `SimpleJSONUtil.pas`

---

## [1.0.0] - Versoes anteriores

Versao original do SimpleORM com suporte a CRUD basico, mapeamento de entidades via atributos RTTI, drivers FireDAC/RestDW/UniDAC/Zeos, bind de formularios VCL/FMX, e validacao NotNull.
