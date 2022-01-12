rule all:
    input:
        "spreadsheets/metrics.xlsx",
        "figures/discrepancies_genome_strat.pdf",
        "figures/table.pdf"

rule stratmet_plot:
    input:
        metrics = "data/benchmark.csv"
    conda:
        "environment.yaml"
    output:
        genome_strat = "figures/discrepancies_genome_strat.pdf"
    script:
        "scripts/stratmet_plot.R"

rule stratmet_table:
    input:
        metrics = "data/benchmark.csv"
    conda:
        "environment.yaml"
    output:
        table = "figures/table.pdf"
    script:
        "scripts/stratmet_table.R"        
        
rule stratmet_sheet:
    input:
        metrics = "data/benchmark.csv"
    conda:
        "environment.yaml"
    output:
        sheet = "spreadsheets/metrics.xlsx"
    script:
        "scripts/stratmet_sheet.R"