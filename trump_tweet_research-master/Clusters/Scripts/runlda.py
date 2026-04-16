import sys

fn = sys.argv[1]
fn_noext = ".".join(fn.split(".")[:-1])

import os

if not os.path.exists(fn_noext):
    os.mkdir(fn_noext)

basecmd = "mallet train-topics"
params = [
    # input param
    f"--input {fn_noext}.mallet",
    # hyperparams
    "--num-topics 5",
    "--num-iterations 2000",
    "--optimize-interval 10",
    "--optimize-burn-in 200",
    # output params
    f"--output-state {fn_noext}/state.gz",
    f"--output-topic-keys {fn_noext}/keywords.tsv",
    f"--output-doc-topics {fn_noext}/composition.tsv"
]

os.system(f"{basecmd} {' '.join(params)}")