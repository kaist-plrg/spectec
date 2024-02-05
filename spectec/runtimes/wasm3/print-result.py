#!/usr/bin/env python3

import argparse
import glob

#
# Args handling
#

parser = argparse.ArgumentParser()
parser.add_argument("file")
parser.add_argument("verbose")

args = parser.parse_args()
verbose = args.verbose == "true"
exitcode = 0

with open(args.file) as f:
    lines = f.readlines()
    for line in lines:
        if "load file" in line:
            if verbose: print(line.strip())
            exitcode = 1
    summarylines = lines[-11:]
    for i in range(11):
        if "{" in summarylines[i]:
            break
    summary = eval(''.join(summarylines[i:i+7]))
    success, total = summary['success'], summary['total_run']
    if success == total:
        pass
    # if total == 0:
        # print("- 0/0 (100.00%)")
    else:
        print("- %s/%s (%.2f%%)"%(success, total, success/total*100))
        exitcode = 1

    exit(exitcode)
# - 13/13 (100.00%)
#     {'crashed': 0,
#  'failed': 149,
#  'missing': 0,
#  'skipped': 0,
#  'success': 0,
#  'timeout': 0,
#  'total_run': 149}