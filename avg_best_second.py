
#!/usr/bin/env python3
"""
Average per-cell values from 'best' and 'second best' grid files.

Usage:
    python3 avg_best_second.py best.txt second_best.txt [-o out.txt]

Behavior:
- Lines may have different lengths; averages are computed column-wise up to the longest line.
- Values are parsed as floats. 'nan' or non-numeric tokens are treated as missing.
- Averaging rules:
    * both numeric -> (best + second_best) / 2
    * one numeric, one missing -> that numeric value
    * both missing -> nan

Author: M365 Copilot
"""

# Update 12 Jan 2026 - Instead of checking for None, checks for "9999.".

import sys
import math
import argparse
from itertools import zip_longest

def parse_args():
    ap = argparse.ArgumentParser(description="Average best and second-best grid files.")
    ap.add_argument("best", help="Path to best (min) grid file")
    ap.add_argument("second", help="Path to second-best grid file")
    ap.add_argument("-o", "--out", help="Output file (defaults to stdout)")
    ap.add_argument("--delimiter", choices=["space", "tab"], default="space",
                    help="Delimiter for output (default: space). Input accepts any whitespace.")
    return ap.parse_args()

def to_float(token):
    """Return float value or None for missing/non-numeric/nan."""
    s = token.strip()
    if not s:
        return None
    try:
        v = float(s)
        return v if not math.isnan(v) else None
    except ValueError:
        return None

def average_pair(a, b):
    """
    Average two optional floats:
    - both <9999 -> arithmetic mean
    - one <9999 -> returns 9999
    """

    a_is = isinstance(a, (int, float)) and math.isfinite(a) and a < 9999
    b_is = isinstance(b, (int, float)) and math.isfinite(b) and b < 9999

    if a_is and b_is:
        return (float(a) + float(b)) / 2.0
    elif (not a_is) and (not b_is):
        return 9999.0
    else:
        return 9999.0 

def main():
    args = parse_args()
    out_delim = "\t" if args.delimiter == "tab" else " "

    with open(args.best, "r") as f_best, open(args.second, "r") as f_second:
        lines_best = f_best.readlines()
        lines_second = f_second.readlines()

    # Stream over the longest file line count
    out_lines = []
    for line_best, line_second in zip_longest(lines_best, lines_second, fillvalue=""):
        # Split on whitespace for input
        cols_best = line_best.strip().split() if line_best is not None else []
        cols_second = line_second.strip().split() if line_second is not None else []

        max_cols = max(len(cols_best), len(cols_second))
        row_out = []

        for c in range(max_cols):
            tb = cols_best[c] if c < len(cols_best) else ""
            ts = cols_second[c] if c < len(cols_second) else ""

            vb = to_float(tb)
            vs = to_float(ts)
            vavg = average_pair(vb, vs)

            if vavg is None:
                row_out.append("nan")
            else:
                # Format: keep integers tidy, floats concise
                # Choose a representation: up to 6 significant digits
                if float(vavg).is_integer():
                    row_out.append(str(int(vavg)))
                else:
                    row_out.append(f"{vavg:.6g}")

        out_lines.append(out_delim.join(row_out))

    if args.out:
        with open(args.out, "w") as fout:
            for line in out_lines:
                fout.write(line + "\n")
    else:
        for line in out_lines:
            print(line)

if __name__ == "__main__":
    main()

