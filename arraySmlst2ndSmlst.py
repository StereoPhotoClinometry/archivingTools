#!/usr/bin/env python3
# 07 Jan 2026 - John R. Weirich
# Takes multiple outputs from map_coverage_p_low and determines the highest
# image resolution and the second highest image resolution. Had AI generate it.
# I checked the results using ISIS 


import sys, math
from itertools import zip_longest

def usage():
    print(f"Usage: {sys.argv[0]} out_min.txt out_second_min.txt file1.txt [file2.txt ...]")
    sys.exit(1)

if len(sys.argv) < 4:
    usage()

out_min, out_second, *inputs = sys.argv[1:]
fps = [open(p, 'r') for p in inputs]

def parse_row(s):
    # Parse a line into floats; None for missing/invalid
    cols = s.strip().split()
    vals = []
    for c in cols:
        try:
            v = float(c)
            vals.append(v if not math.isnan(v) else None)
        except ValueError:
            vals.append(None)
    return vals

def min2(values):
    # Compute smallest and second smallest (second may equal smallest if duplicates).
    m1 = math.inf
    m2 = math.inf
    for v in values:
        if v < m1:
            m2 = m1; m1 = v
        elif v < m2:
            m2 = v
    return m1, m2

with open(out_min, 'w') as fmin, open(out_second, 'w') as f2:
    for rows in zip_longest(*fps, fillvalue=""):
        parsed = [parse_row(r) if r is not None else [] for r in rows]
        max_cols = max((len(p) for p in parsed), default=0)

        out_min_row = []
        out_second_row = []

        for c in range(max_cols):
            candidates = []
            for p in parsed:
                if c < len(p) and p[c] is not None:
                    candidates.append(p[c])

            if len(candidates) == 0:
                out_min_row.append("nan"); out_second_row.append("nan")
            elif len(candidates) == 1:
                out_min_row.append(f"{candidates[0]}"); out_second_row.append("nan")
            else:
                m1, m2 = min2(candidates)
                out_min_row.append(f"{m1}")
                out_second_row.append(f"{m2}")

        fmin.write(" ".join(out_min_row) + "\n")
        f2.write(" ".join(out_second_row) + "\n")

for fp in fps:
    fp.close()

