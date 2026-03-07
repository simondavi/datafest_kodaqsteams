# DataFest 2026 - KODAQS

## Project Structure

The naming scheme of the project folders is as follows:

```
repository/
├── kodaqs_team_quarto.qmd            # this file (main analysis script)
├── data/                             # raw data provided by the DataFest (not shared)
│   ├── arwiki.json                   # information about all edits to arabic articles
│   ├── dewiki.json                   # information about all edits to german articles
│   ├── enwiki.json                   # information about all edits to english articles
│   ├── eswiki.json                   # information about all edits to spanish articles
│   ├── frwiki.json                   # information about all edits to french articles
│   ├── itwiki.json                   # information about all edits to italian articles
│   ├── nlwiki.json                   # information about all edits to dutch articles
│   ├── page_info.json                # page-level information (154k observations)
│   ├── page_views.json               # page view information (36M observations)
│   ├── plwiki.json                   # information about all edits to polish articles
│   ├── ruwiki.json                   # information about all edits to russian articles
│   └── svwiki.json                   # information about all edits to swedish articles
├── extra/                            # additional resources provided by the DataFest
│   ├── access_json.R                 # 
│   ├── datafest_2026.ipynb           # 
│   └── resources_information.txt     # general information on the provided data
├── output/                           # generated tables and plots
├── src/                              # individual code chunks
│   ├── 01_cross-lang.R               # 
│   ├── 02_cross-lang-prep.R          # 
│   ├── 03_cross-lang-reg.py          # 
│   ├── 04_bot-analysis.R             # 
│   ├── 05_bot-pred-prep.R            # 
│   ├── 06_bot-prep-analysis.py       # 
│   └── 07_mal-bot.R                  # 
├── .gitignore                        # note: data/ folder is ignored
├── LICENSE                           # CC-BY-SA license documentation
└── README.md                         # project overview and documentation
```
