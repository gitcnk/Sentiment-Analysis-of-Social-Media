fn = "HC_7_text.csv"

with open(fn, "r", encoding="utf-8") as f:
    fstr = f.read()

fstr = fstr.split("\n")

for i in range(len(fstr)):
    fstr[i] = f"{i + 1}\t\t{fstr[i]}"

with open(fn, "w", encoding="utf-8") as f:
    f.write("\n".join(fstr))