#!/usr/bin/env python3
"""Read-only JVM ABI probe against published Scala standard-library artifacts.

This is an opportunity scan, not a claim of a Scala regression.
"""
import json
import re
import subprocess
import sys
import zipfile
from pathlib import Path

VERSIONS = ["3.9.0", "3.10.1", "3.10.2-RC1"]
ROOTS = (
    "scala.collection.immutable.Range",
    "scala.collection.immutable.NumericRange",
    "scala.util.Sorting",
    "scala.util.hashing.MurmurHash3",
    "scala.collection.ArrayOps",
)

def javap_signatures(jar, cls):
    p = subprocess.run(["javap", "-classpath", str(jar), "-public", "-s", cls],
                       capture_output=True, text=True)
    if p.returncode:
        raise RuntimeError(f"javap failed {cls} {jar}: {p.stderr}")
    found = set()
    previous = ""
    for raw in p.stdout.splitlines():
        line = raw.strip()
        if line.startswith("descriptor:"):
            desc = line.split("descriptor:", 1)[1].strip()
            if "(" not in previous:
                # Field descriptor or another non-callable member
                found.add(("field", previous.split()[-1].removesuffix(";"), desc))
                continue
            name = previous[:previous.index("(")].split()[-1]
            found.add(("method", name, desc))
        elif line and not line.startswith(("Compiled from", "public class", "public final class",
                                         "public abstract class", "public interface", "protected",
                                         "}", "{", "Signature:")):
            previous = line
        elif line.startswith("public ") or line.startswith("protected "):
            previous = line
    return found

def class_names(jar):
    with zipfile.ZipFile(jar) as z:
        items = [p[:-6].replace("/", ".") for p in z.namelist()
                 if p.endswith(".class") and "$$Lambda" not in p]
    return set(x for x in items if any(x == root or x.startswith(root + "$") for root in ROOTS))

out = {"versions": VERSIONS, "comparisons": []}
classes = {v: class_names(Path("artifacts") / f"scala-library-{v}.jar") for v in VERSIONS}
print("CLASS_COUNTS", {v: len(classes[v]) for v in VERSIONS}, flush=True)
for a,b in zip(VERSIONS, VERSIONS[1:]):
    old = Path("artifacts") / f"scala-library-{a}.jar"
    new = Path("artifacts") / f"scala-library-{b}.jar"
    removed_classes = sorted(classes[a] - classes[b])
    newly_added_classes = sorted(classes[b] - classes[a])
    removed = []
    for cls in sorted(classes[a] & classes[b]):
        old_s = javap_signatures(old, cls)
        new_s = javap_signatures(new, cls)
        for kind,name,desc in sorted(old_s-new_s):
            if name == "static {}":
                continue
            removed.append({"class":cls,"kind":kind,"symbol":name,"descriptor":desc})
    result = {"from":a,"to":b,"removed_classes":removed_classes,
              "new_classes":newly_added_classes,"removed_symbols":removed}
    out["comparisons"].append(result)
    print("COMPARE", a, b, "removed_classes", len(removed_classes),
          "removed_symbols",len(removed),flush=True)
    for row in removed[:65]:
        print("MISSING",json.dumps(row),flush=True)
Path("results").mkdir(exist_ok=True)
Path("results/abi-diff.json").write_text(json.dumps(out,indent=2)+"\n")
