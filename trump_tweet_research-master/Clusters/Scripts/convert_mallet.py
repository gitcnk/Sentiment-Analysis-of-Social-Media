import sys

fn = sys.argv[1]
fn_noext = ".".join(fn.split(".")[:-1])

cmd = f"mallet import-file --input {fn} --output {fn_noext}.mallet --keep-sequence --remove-stopwords"

import os
os.system(cmd)