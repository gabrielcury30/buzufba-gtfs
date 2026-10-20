# Instruções do repositório

## 1. Objetivo do projeto

Este repositório modela o sistema de ônibus BUZUFBA da Universidade Federal da Bahia (UFBA), em Salvador. O código gera feeds no padrão GTFS, representa a rede de transporte e as edificações da UFBA, calcula acessibilidade potencial por transporte ativo e coletivo e produz materiais de análise e visualização.

Os resultados são simulações. Não os apresente como medições de operação real sem validação com dados observados.

## 2. Arquitetura e organização

- `src/R/gtfs/gtfs.R`: gera o feed GTFS de referência em `data/gtfs/buzufba_gtfs.zip`.
- `src/R/gtfs/gtfs_intervencao.R`: gera uma variante de GTFS para análises de intervenção, em `data/gtfs/buzufba_gtfs_intervencao.zip`.
- `src/R/analise/analise_acessibilidade.R`: análise exploratória de uma rede e produção de gráficos.
- `src/R/analise/analise_comparacao_cenarios_ufba.R`: compara os cenários atual e de intervenção e produz gráficos em `data/img/comparacao_cenarios/`.
- `src/R/animacao/animacao_buzufba.R`: lê o GTFS de referência e os polígonos das edificações, interpola posições de viagens e grava `index.html` na raiz.
- `notebooks/documentacao_script_gtfs.qmd`: manual Quarto sobre geração de GTFS. Parte do conteúdo descreve uma versão anterior do processo.
- `notebooks/relatorio_estudo_buzufba_completo/relatorio_estudo_buzufba/`: relatório de um estudo de intervenção, seus insumos e figuras.
- `data/edificacoes/`: GeoPackages de edificações para os cenários atual e de intervenção.
- `data/shapes/`: geometria de shapes desenhados manualmente, usada pelo gerador GTFS quando disponível.
- `data/pbf/` e `data/tif/`: insumos de rede viária OpenStreetMap e elevação.
- `data/r5r_atual/` e `data/r5r_intervencao/`: diretórios locais de construção das redes R5. PBF, GTFS e TIF são insumos; `network.dat`, `*.mapdb*`, logs, relatórios de erros e `network_settings.json` são saídas locais regeneráveis e ignoradas pelo Git.
- `data/gtfs/`: feeds GTFS exportados.
- `data/img/`: gráficos e figuras gerados.

Os scripts usam caminhos relativos à raiz do repositório. Execute-os com a raiz como diretório de trabalho (por exemplo, abra `buzufba-gtfs.Rproj` no RStudio).

### Fluxo de dados

1. Os scripts em `src/R/gtfs/` montam tabelas GTFS, sequências de paradas e horários; consultam o Valhalla local para obter tempos e, conforme a configuração e os dados disponíveis, shapes; exportam feeds ZIP.
2. Os feeds são combinados com arquivos PBF e, quando aplicável, TIF para construir redes R5 nos diretórios dos cenários.
3. Os scripts de análise usam `r5r` e GeoPackages para calcular tempos de viagem/acessibilidade e gerar gráficos.
4. O script de animação usa o feed de referência para criar o mapa HTML interativo.
5. O relatório Quarto reúne métodos, resultados e figuras do estudo de intervenção.

## 3. Regras e restrições

- Preserve os insumos originais em `data/`, especialmente GeoPackages, PBFs, GeoTIFFs e shapes manuais. Os arquivos de rede R5 compilados são derivados: podem ser reconstruídos dos insumos correspondentes e não devem ser adicionados ao Git.
- Não leia nem carregue arquivos geoespaciais, redes compiladas ou outros binários grandes sem necessidade. Prefira inspecionar código, metadados e nomes/tamanhos dos arquivos.
- Scripts podem criar ou sobrescrever artefatos (`.zip`, imagens, arquivos R5 e `index.html`). Antes de executá-los, verifique os caminhos de saída e os efeitos esperados. Não execute o pipeline completo para uma verificação simples.
- A geração GTFS depende de um servidor Valhalla acessível em `http://localhost:8002`; o código atual usa `httr2`, não OSRM.
- As análises R5 dependem de Java e da instalação compatível de `r5r`. O `.Rprofile` local configura `JAVA_HOME` por meio de `rJavaEnv`; não suponha que esse caminho exista em outras máquinas.
- Respeite os identificadores e as relações do GTFS: `stop_id`, `route_id`, `trip_id`, `shape_id`, `service_id`, `stop_sequence` e os horários precisam permanecer consistentes entre tabelas.
- Ao alterar paradas, sequências, shapes ou parâmetros de tempo, confira implicações para rotas circulares, passagens repetidas por uma parada e operação noturna.
- O arquivo `data/shapes/shapes_manuais.gpkg`, quando presente, seleciona o modo de shapes manuais nos geradores. Não remova nem renomeie esse insumo como parte de uma alteração incidental.
- Os PNGs de `data/img/comparacao_cenarios/` são saídas regeneráveis e ignoradas. As figuras em `notebooks/relatorio_estudo_buzufba_completo/relatorio_estudo_buzufba/figuras/` são snapshots preservados usados para renderizar o relatório histórico.
- Os feeds em `data/gtfs/` e suas cópias de cenário são mantidos versionados: análises e animação os consomem diretamente, e a geração depende de Valhalla local. Ao mudar geradores ou insumos, não os remova sem definir e validar uma substituição reprodutível.
- Separe resultados do cenário atual e de intervenção. Confira quais GTFS e edificações cada análise consome antes de interpretar ou atualizar figuras.
- Não inclua chaves, credenciais, dados pessoais ou segredos em código, documentação ou saídas versionadas.

## 4. Como executar e verificar

Use R a partir da raiz do repositório. No RStudio, abra `buzufba-gtfs.Rproj`.

### Geração de GTFS

```r
source("src/R/gtfs/gtfs.R")
```

Para a variante de intervenção:

```r
source("src/R/gtfs/gtfs_intervencao.R")
```

Ambos os scripts podem chamar o Valhalla local e gravam um ZIP em `data/gtfs/`.

### Análise exploratória

```r
source("src/R/analise/analise_acessibilidade.R")
```

Este script espera atualmente uma rede em `data/r5r`, mas essa pasta não aparece na estrutura observada do repositório; confirme e ajuste a configuração antes de executá-lo.

### Comparação de cenários

```r
source("src/R/analise/analise_comparacao_cenarios_ufba.R")
```

O script usa `data/r5r_atual/`, `data/r5r_intervencao/` e os GeoPackages correspondentes. Confira a opção de reconstrução das redes: reconstruí-las pode levar tempo e usar recursos relevantes.

### Animação

```r
source("src/R/animacao/animacao_buzufba.R")
```

O script grava `index.html` na raiz, podendo substituir o arquivo existente.

Não há, no momento, uma suíte de testes automatizados ou manifesto de dependências versionado identificado. Para alterações de código, faça verificações proporcionais e direcionadas: revise diferenças, valide sintaxe/parse de R quando aplicável e execute somente o menor fluxo relevante, considerando dependências locais e arquivos que podem ser sobrescritos. Não trate execução de uma análise que grava saídas como uma verificação sem efeitos colaterais.

## 5. Padrões de desenvolvimento

- Mantenha código e documentação em português, seguindo o idioma predominante do repositório.
- Prefira mudanças pequenas, localizadas e coerentes com a organização atual em `src/R/`.
- Use caminhos relativos à raiz do projeto para os insumos e saídas já organizados no repositório.
- Preserve nomes e esquemas GTFS e nomes de campos espaciais já consumidos pelos scripts, como `name` nos GeoPackages.
- Torne explícitos os parâmetros que afetam resultados — data/hora, janela de partidas, modos, percentil, limite de viagens, margem de parada e cenário — e documente alterações relevantes.
- Ao mudar comportamento ou caminhos, atualize a documentação correspondente e indique claramente se o material documenta o código atual ou um estudo/versão anterior.
- Evite adicionar dependências sem necessidade. Como não há `renv.lock` ou `DESCRIPTION`, registre novas dependências e requisitos de ambiente na documentação pertinente.
- Não reformate grandes arquivos ou relatórios inteiros por causa de uma alteração pontual.

## 6. Tecnologias e dependências

- Linguagem e ambiente: R, projeto RStudio, Quarto para documentos `.qmd`.
- GTFS e dados: `gtfstools`, `tidyverse`, `data.table`, `tidytransit`, `dplyr`, `tidyr`, `lubridate`.
- Geoespacial: `sf`, GeoPackage, GeoTIFF, PBF/OpenStreetMap.
- Roteamento/análise: serviço Valhalla via HTTP (`httr2`) para geração de GTFS; `r5r` e Java para análise multimodal.
- Visualização: `mapview`, `ggplot2`, `ggspatial`, `viridis`, `scales`, `forcats`, `leaflet`, `leaftime`, `geojsonio`, `htmlwidgets`, `stinepack`.
- A dependência exata pode variar por script; consulte os `library()` no arquivo antes de executar.

## 7. Fontes de verdade e divergências conhecidas

- O código atual de `src/R/gtfs/` é a referência para a implementação de geração GTFS. O README e `notebooks/documentacao_script_gtfs.qmd` ainda citam OSRM e podem descrever uma versão antiga.
- O README também mostra uma estrutura de dados diferente da existente (por exemplo, `data/r5r/ufba.pbf`); confira sempre os caminhos efetivos nos scripts e a estrutura atual antes de seguir instruções antigas.
- `src/R/analise/analise_acessibilidade.R` aponta para `data/r5r`, enquanto as pastas observadas para redes são `data/r5r_atual/` e `data/r5r_intervencao/`.
- Há uma cópia do script de comparação dentro do pacote do relatório. Ao alterar a análise, compare essa cópia com `src/R/analise/analise_comparacao_cenarios_ufba.R`; não assuma que sejam sincronizadas.
- `index.html` é um artefato renderizado, mas permanece versionado como entrada do GitHub Pages; só o regenere quando a tarefa envolver a publicação da animação. `README.html` e o HTML renderizado do relatório são saídas locais ignoradas pelo Git.
- O manual `notebooks/documentacao_script_gtfs.qmd` referencia `paradas_gtfs.png` e `rotab1_gtfs.png`, que não existem atualmente no repositório. Considere isso ao renderizar ou atualizar esse documento.

## 8. Questões de domínio a confirmar

Antes de alterar resultados operacionais, confirme com o responsável pelo projeto os horários, trajetos noturnos, tempos reais de volta e a validade dos cenários. O código documenta algumas decisões como provisórias, incluindo a margem de parada e a hipótese de trajeto noturno. Não as trate como dados operacionais confirmados.

## Fluxo de Trabalho de Desenvolvimento Padrão

Para cada solicitação do usuário que exija a modificação de arquivos do projeto, siga este fluxo de trabalho do Git automaticamente. O usuário não deve precisar solicitar individualmente a criação de branches, commits, pushes ou pull requests.

### 1. Inspecionar o repositório
- Leia as instruções pertinentes no arquivo `AGENTS.md`.
- Verifique o `git status`, a branch atual, os remotos configurados e os commits recentes.
- Identifique alterações feitas previamente pelo usuário antes de realizar qualquer edição.
- Nunca sobrescreva, descarte, adicione ao *staging* ou faça commit de alterações preexistentes que não estejam relacionadas à tarefa.
- Se a árvore de trabalho (*working tree*) contiver alterações pendentes e estas não puderem ser isoladas com segurança, interrompa o processo e explique a situação.

### 2. Criar uma branch isolada para a tarefa
- Mantenha a branch `main` estável; nunca implemente uma tarefa diretamente nela.
- Para uma nova tarefa, crie uma branch a partir da versão mais recente da `main` (ou da branch base designada).
- Utilize prefixos como `feat/`, `fix/`, `docs/`, `refactor/`, `test/` ou `chore/`, seguidos por uma descrição concisa em formato *kebab-case*.
- Se a branch atual contiver outra tarefa inacabada, não a reutilize para um trabalho não relacionado.
- Utilize um *worktree* do Git separado, caso seja necessário isolar tarefas simultâneas.

### 3. Implementar e verificar
- Analise a implementação existente antes de realizar edições.
- Realize a menor alteração coerente que atenda à solicitação.
- Execute os testes, verificações e comandos de validação pertinentes.
- Revise o *diff* completo em busca de modificações não relacionadas, alterações acidentais em dados, exposição de segredos ou arquivos gerados automaticamente.
- Nunca afirme que um teste passou sem que ele tenha sido, de fato, executado com sucesso.

### 4. Realizar commits das alterações
- Adicione ao *staging* apenas os arquivos relevantes para a tarefa.
- Siga o padrão *Conventional Commits*: `tipo(escopo): descrição`.
- Utilize mensagens de commit concisas e específicas, em inglês.
- Prefira criar um ou poucos commits coerentes, em vez de um único conjunto de alterações sem relação entre si.
- Nunca altere commits existentes (*amend*) nem reescreva o histórico sem autorização explícita.

### 5. Publicar a tarefa
- Assim que a implementação e a validação forem concluídas com sucesso, realize o commit das alterações específicas da tarefa e envie (*push*) a branch da tarefa para o repositório remoto (`origin`). - Defina o *upstream branch* (ramo de origem) quando necessário.
- Crie um *pull request* (PR) direcionado à *branch* `main`, utilizando a integração do GitHub disponível ou a CLI do GitHub.
- Inclua na descrição do PR: o objetivo, as principais alterações, evidências de validação e eventuais limitações conhecidas.
- Caso não possua o acesso ou as ferramentas necessárias no GitHub, informe o impedimento de forma transparente.

### 6. Segurança e etapas de aprovação
- Não realize *merges* de *pull requests* de forma automática.
- Não envie alterações (*push*) diretamente para a *branch* `main`.
- Não utilize *force-push*, não exclua *branches*, não descarte alterações feitas pelo usuário nem execute operações destrutivas no Git sem autorização explícita.
- Interrompa o processo caso existam conflitos de *merge* não resolvidos, falhas em testes críticos, alterações ambíguas ou um estado inesperado no repositório.
- Não realize *commits* nem publique segredos, credenciais, conjuntos de dados privados ou arquivos grandes gerados automaticamente sem aprovação explícita.

### 7. Relatório final
Ao concluir cada tarefa, informe:
- O que foi implementado.
- Os testes executados e seus respectivos resultados.
- O nome da *branch* e o *hash* do *commit*.
- O *status* do envio (*push*).
- A URL do *pull request*, caso tenha sido criado com sucesso.
- Quaisquer limitações remanescentes ou decisões que exijam uma definição por parte do usuário.

Solicitações de apenas leitura, explicações e revisões de código que não envolvam a modificação de arquivos não exigem a criação de uma nova *branch* ou *commit*.
