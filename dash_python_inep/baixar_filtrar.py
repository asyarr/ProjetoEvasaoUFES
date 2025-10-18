## ---------------------
# Esse script é pra baixar os microdados no INEP e filtrar apenas os dados do UFES-CCE Campus Goiabeiras
## ---------------------

import os
import re
import requests
import zipfile
import shutil
import pandas as pd

def baixar_e_extrair_microdados(ano_inicio, ano_fim, pasta_extracao='microdados', pasta_temporaria='temp_zips'):
    os.makedirs(pasta_extracao, exist_ok=True)
    os.makedirs(pasta_temporaria, exist_ok=True)

    for ano in range(ano_inicio, ano_fim + 1):
        url = f"https://download.inep.gov.br/microdados/microdados_censo_da_educacao_superior_{ano}.zip"
        zip_path = os.path.join(pasta_temporaria, f"microdados_{ano}.zip")

        print(f"\n--- Processando ano {ano} ---")
        try:
            with requests.get(url, stream=True) as r:
                r.raise_for_status()
                with open(zip_path, 'wb') as f:
                    for chunk in r.iter_content(chunk_size=8192):
                        f.write(chunk)
            print(f"Download concluído: {zip_path}")

            with zipfile.ZipFile(zip_path, 'r') as zip_ref:
                zip_ref.extractall(pasta_extracao)
            print(f"Extração concluída para {ano}.")

            os.remove(zip_path)
        except Exception as e:
            print(f"Erro ao processar o ano {ano}: {e}")

    # Limpar pasta temporária se estiver vazia
    if not os.listdir(pasta_temporaria):
        os.rmdir(pasta_temporaria)

def renomear_pastas_para_ano(pasta_mae='microdados'):
    padrao_ano = re.compile(r'.*?(\d{4})$')
    for nome in os.listdir(pasta_mae):
        caminho_antigo = os.path.join(pasta_mae, nome)
        if os.path.isdir(caminho_antigo):
            match = padrao_ano.match(nome)
            if match:
                ano = match.group(1)
                caminho_novo = os.path.join(pasta_mae, ano)
                if caminho_antigo != caminho_novo:
                    os.rename(caminho_antigo, caminho_novo)
                    print(f"Renomeado: {nome} -> {ano}")

def padronizar_pastas_e_arquivos(pasta_mae='microdados'):
    for nome_ano in os.listdir(pasta_mae):
        caminho_ano = os.path.join(pasta_mae, nome_ano)
        if os.path.isdir(caminho_ano) and nome_ano.isdigit():
            pasta_dados_antiga = os.path.join(caminho_ano, 'DADOS')
            pasta_dados_nova = os.path.join(caminho_ano, 'dados')

            if os.path.exists(pasta_dados_antiga):
                os.rename(pasta_dados_antiga, pasta_dados_nova)
                print(f"Renomeada: {pasta_dados_antiga} -> {pasta_dados_nova}")

            arquivo_antigo = os.path.join(pasta_dados_nova, 'GRADUACAO_PRESENCIAL.CSV')
            arquivo_novo = os.path.join(pasta_dados_nova, f'MICRODADOS_CADASTRO_CURSOS_{nome_ano}.csv')
            if os.path.exists(arquivo_antigo):
                os.rename(arquivo_antigo, arquivo_novo)
                print(f"Arquivo renomeado: {arquivo_antigo} -> {arquivo_novo}")

def copiar_arquivos_ano_para_pasta_dados(pasta_mae='microdados', pasta_destino='dados'):
    os.makedirs(pasta_destino, exist_ok=True)

    for nome_ano in os.listdir(pasta_mae):
        caminho_ano = os.path.join(pasta_mae, nome_ano)
        if os.path.isdir(caminho_ano) and nome_ano.isdigit():
            pasta_dados = os.path.join(caminho_ano, "dados")
            if os.path.isdir(pasta_dados):
                nome_base = f"MICRODADOS_CADASTRO_CURSOS_{nome_ano}"
                for ext in [".CSV", ".txt", ".zip"]:
                    arquivo_origem = os.path.join(pasta_dados, nome_base + ext)
                    if os.path.exists(arquivo_origem):
                        destino = os.path.join(pasta_destino, os.path.basename(arquivo_origem))
                        shutil.copy2(arquivo_origem, destino)
                        print(f"Copiado: {arquivo_origem} -> {destino}")
                        break

def filtrar_e_salvar_cursos(
    pasta_dados='dados',
    output_file='dados/cursos_filtrados.csv',
    anos=None,  # se None: detecta automaticamente os anos disponíveis na pasta
    co_ies=573,
    uf='ES',
    codigos_cursos_desejados=None,
):
    if codigos_cursos_desejados is None:
        codigos_cursos_desejados = [12835, 1112909, 12810, 1112889, 12833, 12806, 312806]

    substituicoes = {
        'Matematica': 'Matemática',
        'Fisica': 'Física',
        'Quimica': 'Química',
        'Estatistica': 'Estatística',
        # manter as formas já acentuadas também não faz mal
        'Matemática': 'Matemática',
        'Física': 'Física',
        'Química': 'Química',
        'Estatística': 'Estatística',
    }

    # Detectar anos automaticamente se não informados
    if anos is None:
        anos = []
        if os.path.isdir(pasta_dados):
            for fname in os.listdir(pasta_dados):
                m = re.match(r'MICRODADOS_CADASTRO_CURSOS_(\d{4})\.(csv|txt|zip)$', fname, flags=re.IGNORECASE)
                if m:
                    anos.append(int(m.group(1)))
        anos = sorted(anos)
        if not anos:
            print("Nenhum arquivo MICRODADOS_CADASTRO_CURSOS_YYYY encontrado em", pasta_dados)
            return

    dfs = []
    for ano in anos:
        caminho = os.path.join(pasta_dados, f'MICRODADOS_CADASTRO_CURSOS_{ano}.CSV')
        if not os.path.exists(caminho):
            print(f"Arquivo não encontrado: {caminho}  (pulando)")
            continue

        print(f"Lendo: {caminho}")
        df = pd.read_csv(caminho, sep=';', encoding='latin1', low_memory=False)

        # checar se as colunas que vamos usar existem
        col_necessarias = {'SG_UF', 'CO_IES', 'CO_CURSO', 'NO_CURSO'}
        if not col_necessarias.issubset(set(df.columns)):
            print(f"Colunas necessárias ausentes em {caminho}. Colunas disponíveis: {list(df.columns)}")
            continue

        # filtro correto e não redundante
        mask = (
            (df['SG_UF'] == uf) &
            (df['CO_IES'] == co_ies) &
            (df['CO_CURSO'].isin(codigos_cursos_desejados))
        )
        df_filtrado = df.loc[mask].copy()

        if df_filtrado.empty:
            print(f"Nenhum registro filtrado para o ano {ano}.")
            continue

        # padroniza NO_CURSO: aplicar substituições + title()
        df_filtrado['NO_CURSO'] = (
            df_filtrado['NO_CURSO']
            .astype(str)
            .str.strip()
            .replace(substituicoes, regex=False)
            .str.title()
        )

        df_filtrado['ANO'] = ano
        dfs.append(df_filtrado)

    if dfs:
        df_total = pd.concat(dfs, ignore_index=True)
        os.makedirs(os.path.dirname(output_file), exist_ok=True)
        df_total.to_csv(output_file, sep=';', encoding='latin1', index=False)
        print(f"\nDados filtrados salvos em: {output_file}")
    else:
        print("Nenhum dado filtrado foi encontrado.")
        

def limpar_arquivos_originais(pasta_microdados='microdados', pasta_dados='dados', arquivo_filtrado='cursos_filtrados.csv'):
    """
    Remove a pasta de microdados e mantém apenas o arquivo filtrado na pasta 'dados'.
    """

    if os.path.exists(pasta_microdados):
        try:
            shutil.rmtree(pasta_microdados)
            print(f"Pasta '{pasta_microdados}' removida com sucesso.")
        except Exception as e:
            print(f"Erro ao remover '{pasta_microdados}': {e}")

    if os.path.exists(pasta_dados):
        for fname in os.listdir(pasta_dados):
            caminho = os.path.join(pasta_dados, fname)
            # manter apenas o arquivo cursos_filtrados
            if os.path.isfile(caminho) and fname != arquivo_filtrado:
                try:
                    os.remove(caminho)
                    print(f"Removido: {fname}")
                except Exception as e:
                    print(f"Erro ao remover {fname}: {e}")

if __name__ == "__main__":
    ANO_INICIAL = 2009
    ANO_FINAL = 2024

    print("\n==== INÍCIO DO PROCESSAMENTO ====\n")
    baixar_e_extrair_microdados(ANO_INICIAL, ANO_FINAL)
    renomear_pastas_para_ano()
    padronizar_pastas_e_arquivos()
    copiar_arquivos_ano_para_pasta_dados()
    filtrar_e_salvar_cursos()
    limpar_arquivos_originais()
    print("\n==== PROCESSAMENTO CONCLUÍDO ====\n")
