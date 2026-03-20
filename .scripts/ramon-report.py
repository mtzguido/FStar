#!/usr/bin/env python3
"""
ramon-report.py — Generate a visual performance comparison report from
two directories of .ramon files (produced by the `ramon` tool).

Usage:
    python3 ramon-report.py <baseline_dir> <patched_dir> [--output report.html]

Produces an HTML report with:
  - Summary statistics (median/mean/min/max changes)
  - Memory and time scatter plots
  - Top regressions and improvements tables
  - Full comparison table
"""

import os
import sys
import argparse
import json
import re
from collections import defaultdict
from statistics import median, mean

# ── Parsing ──────────────────────────────────────────────────────────────────

def parse_time(s):
    m = re.search(r'([\d.]+)s', s)
    return float(m.group(1)) if m else 0.0

def parse_mem(s):
    m = re.search(r'(\d+)\s*(KiB|MiB|GiB|B)', s)
    if not m:
        return 0
    val = int(m.group(1))
    unit = m.group(2)
    if unit == "KiB":  val *= 1024
    elif unit == "MiB": val *= 1024 * 1024
    elif unit == "GiB": val *= 1024 * 1024 * 1024
    return val

def humanize(n):
    for suf in ['B', 'KiB', 'MiB', 'GiB', 'TiB']:
        if abs(n) < 1024:
            return f"{n:.1f} {suf}"
        n /= 1024
    return f"{n:.1f} TiB"

def load_ramon_file(fn):
    ret = {"fn": fn}
    try:
        with open(fn) as f:
            for line in f:
                parts = line.split(None, 1)
                if len(parts) < 2: continue
                key, val = parts[0], parts[1].strip()
                if key == "group.total":     ret["time"] = parse_time(val)
                elif key == "group.utime":   ret["utime"] = parse_time(val)
                elif key == "group.stime":   ret["stime"] = parse_time(val)
                elif key == "group.mempeak": ret["mem"] = parse_mem(val)
                elif key == "exitcode":      ret["rc"] = int(val)
                elif key == "walltime":      ret["wall"] = parse_time(val)
    except Exception as e:
        return None
    if "rc" not in ret or "time" not in ret or "mem" not in ret:
        return None
    return ret

def find_ramon_files(root):
    result = []
    for dirpath, _, filenames in os.walk(root):
        for f in filenames:
            if f.endswith(".ramon"):
                result.append(os.path.join(dirpath, f))
    return sorted(result)

def make_matching(root1, root2, files1, files2):
    d1 = {}
    d2 = {}
    for f in files1:
        r = load_ramon_file(f)
        if r and r.get("rc", 1) == 0:
            key = os.path.relpath(f, root1).removesuffix(".ramon")
            d1[key] = r
    for f in files2:
        r = load_ramon_file(f)
        if r and r.get("rc", 1) == 0:
            key = os.path.relpath(f, root2).removesuffix(".ramon")
            d2[key] = r
    matches = []
    for key in sorted(set(d1.keys()) & set(d2.keys())):
        matches.append({
            "fn": key,
            "l": d1[key],
            "r": d2[key],
        })
    return matches, d1, d2

# ── Statistics ───────────────────────────────────────────────────────────────

def compute_stats(matches):
    if not matches: return {}
    mem_pcts = [(m["r"]["mem"] - m["l"]["mem"]) / m["l"]["mem"] * 100
                for m in matches if m["l"]["mem"] > 0]
    time_pcts = [(m["r"]["time"] - m["l"]["time"]) / m["l"]["time"] * 100
                 for m in matches if m["l"]["time"] > 0.1]  # skip very fast tests
    mem_diffs = [m["r"]["mem"] - m["l"]["mem"] for m in matches]
    time_diffs = [m["r"]["time"] - m["l"]["time"] for m in matches]
    # Heavy tests: baseline mem > 100MiB
    heavy = [m for m in matches if m["l"]["mem"] > 100*1024*1024]
    heavy_mem_pcts = [(m["r"]["mem"] - m["l"]["mem"]) / m["l"]["mem"] * 100
                      for m in heavy if m["l"]["mem"] > 0]
    heavy_time_pcts = [(m["r"]["time"] - m["l"]["time"]) / m["l"]["time"] * 100
                       for m in heavy if m["l"]["time"] > 0.1]
    def stats(xs):
        if not xs: return {"median": 0, "mean": 0, "min": 0, "max": 0, "count": 0}
        return {"median": median(xs), "mean": mean(xs),
                "min": min(xs), "max": max(xs), "count": len(xs)}
    return {
        "mem_pct": stats(mem_pcts),
        "time_pct": stats(time_pcts),
        "mem_abs": stats(mem_diffs),
        "time_abs": stats(time_diffs),
        "heavy_mem_pct": stats(heavy_mem_pcts),
        "heavy_time_pct": stats(heavy_time_pcts),
        "n_matches": len(matches),
        "n_heavy": len(heavy),
    }

# ── HTML Report ──────────────────────────────────────────────────────────────

def generate_html(matches, stats, lhs_label, rhs_label):
    # Prepare data for charts
    mem_data = []
    time_data = []
    for m in matches:
        short = m["fn"].split("/")[-1] if "/" in m["fn"] else m["fn"]
        mem_data.append({
            "name": short,
            "full": m["fn"],
            "base": m["l"]["mem"] / (1024*1024),
            "patch": m["r"]["mem"] / (1024*1024),
        })
        time_data.append({
            "name": short,
            "full": m["fn"],
            "base": m["l"]["time"],
            "patch": m["r"]["time"],
        })

    # Sort matches for tables
    by_mem_diff = sorted(matches, key=lambda m: m["r"]["mem"] - m["l"]["mem"])
    by_time_diff = sorted(matches, key=lambda m: m["r"]["time"] - m["l"]["time"])
    by_mem_pct = sorted(matches, key=lambda m: (m["r"]["mem"] - m["l"]["mem"]) / max(m["l"]["mem"], 1))

    s = stats
    mp = s["mem_pct"]
    tp = s["time_pct"]
    hm = s["heavy_mem_pct"]
    ht = s["heavy_time_pct"]

    # Build histogram data for memory % changes
    mem_pct_list = [(m["r"]["mem"] - m["l"]["mem"]) / max(m["l"]["mem"], 1) * 100
                    for m in matches]
    hist_bins = list(range(-35, 15, 1))
    hist_counts = [0] * (len(hist_bins) - 1)
    for p in mem_pct_list:
        for i in range(len(hist_bins) - 1):
            if hist_bins[i] <= p < hist_bins[i+1]:
                hist_counts[i] += 1
                break

    def pct(base, new):
        if base == 0: return "N/A"
        p = (new - base) / base * 100
        color = "green" if p < -1 else ("red" if p > 1 else "gray")
        sign = "+" if p > 0 else ""
        return f'<span style="color:{color};font-weight:bold">{sign}{p:.1f}%</span>'

    def row(m):
        fn = m["fn"]
        lm, rm = m["l"]["mem"], m["r"]["mem"]
        lt, rt = m["l"]["time"], m["r"]["time"]
        return f"""<tr>
            <td style="font-family:monospace;font-size:0.8em" title="{fn}">{fn[-80:]}</td>
            <td style="text-align:right">{humanize(lm)}</td>
            <td style="text-align:right">{humanize(rm)}</td>
            <td style="text-align:right">{pct(lm, rm)}</td>
            <td style="text-align:right">{lt:.2f}s</td>
            <td style="text-align:right">{rt:.2f}s</td>
            <td style="text-align:right">{pct(lt, rt)}</td>
        </tr>"""

    def table(items, n=20):
        hdr = """<table style="border-collapse:collapse;width:100%;font-size:0.85em">
        <tr style="background:#f0f0f0"><th>File</th>
            <th>Mem (base)</th><th>Mem (patch)</th><th>Mem Δ</th>
            <th>Time (base)</th><th>Time (patch)</th><th>Time Δ</th></tr>"""
        rows = "\n".join(row(m) for m in items[:n])
        return hdr + rows + "</table>"

    html = f"""<!DOCTYPE html>
<html><head>
<meta charset="utf-8">
<title>F* Performance Comparison Report</title>
<style>
body {{ font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; margin: 2em; background: #fafafa; }}
h1 {{ color: #333; border-bottom: 2px solid #0366d6; padding-bottom: 0.3em; }}
h2 {{ color: #444; margin-top: 2em; }}
.summary {{ display: grid; grid-template-columns: repeat(auto-fit, minmax(250px, 1fr)); gap: 1em; margin: 1em 0; }}
.card {{ background: white; border-radius: 8px; padding: 1.2em; box-shadow: 0 1px 3px rgba(0,0,0,0.1); }}
.card h3 {{ margin: 0 0 0.5em; color: #555; font-size: 0.9em; text-transform: uppercase; }}
.card .value {{ font-size: 1.8em; font-weight: bold; }}
.card .detail {{ color: #888; font-size: 0.85em; margin-top: 0.3em; }}
.good {{ color: #22863a; }}
.bad {{ color: #cb2431; }}
.neutral {{ color: #6a737d; }}
table {{ border-collapse: collapse; width: 100%; }}
th, td {{ padding: 4px 8px; border-bottom: 1px solid #eee; }}
tr:hover {{ background: #f8f8ff; }}
canvas {{ max-width: 100%; margin: 1em 0; }}
details {{ margin: 0.5em 0; }}
summary {{ cursor: pointer; font-weight: bold; padding: 0.5em; background: #f0f0f0; border-radius: 4px; }}
</style>
<script src="https://cdn.jsdelivr.net/npm/chart.js@4"></script>
</head><body>
<h1>🔬 F* Performance Comparison Report</h1>
<p><strong>Baseline:</strong> {lhs_label}<br>
<strong>Patched:</strong> {rhs_label}<br>
<strong>Matched tests:</strong> {s["n_matches"]}</p>

<div class="summary">
    <div class="card">
        <h3>Memory Change (median)</h3>
        <div class="value {'good' if mp['median'] < -1 else ('bad' if mp['median'] > 1 else 'neutral')}">{mp['median']:+.1f}%</div>
        <div class="detail">mean: {mp['mean']:+.1f}% · min: {mp['min']:+.1f}% · max: {mp['max']:+.1f}%</div>
    </div>
    <div class="card">
        <h3>Time Change (median)</h3>
        <div class="value {'good' if tp['median'] < -1 else ('bad' if tp['median'] > 1 else 'neutral')}">{tp['median']:+.1f}%</div>
        <div class="detail">mean: {tp['mean']:+.1f}% · min: {tp['min']:+.1f}% · max: {tp['max']:+.1f}%</div>
    </div>
    <div class="card">
        <h3>Total Memory Saved</h3>
        <div class="value {'good' if s['mem_abs']['mean'] < 0 else 'bad'}">{humanize(sum(m['r']['mem'] - m['l']['mem'] for m in matches))}</div>
        <div class="detail">sum across all {s['n_matches']} tests</div>
    </div>
    <div class="card">
        <h3>Total Time Change</h3>
        <div class="value {'good' if s['time_abs']['mean'] < 0 else ('bad' if s['time_abs']['mean'] > 0.5 else 'neutral')}">{sum(m['r']['time'] - m['l']['time'] for m in matches):+.1f}s</div>
        <div class="detail">sum across all {s['n_matches']} tests</div>
    </div>
</div>

<h2>🏋️ Heavy Tests Only (baseline &gt; 100 MiB, n={s['n_heavy']})</h2>
<div class="summary">
    <div class="card">
        <h3>Memory Change (median)</h3>
        <div class="value {'good' if hm['median'] < -1 else ('bad' if hm['median'] > 1 else 'neutral')}">{hm['median']:+.1f}%</div>
        <div class="detail">mean: {hm['mean']:+.1f}% · min: {hm['min']:+.1f}% · max: {hm['max']:+.1f}% · n={hm['count']}</div>
    </div>
    <div class="card">
        <h3>Time Change (median)</h3>
        <div class="value {'good' if ht['median'] < -1 else ('bad' if ht['median'] > 1 else 'neutral')}">{ht['median']:+.1f}%</div>
        <div class="detail">mean: {ht['mean']:+.1f}% · min: {ht['min']:+.1f}% · max: {ht['max']:+.1f}% · n={ht['count']}</div>
    </div>
</div>

<h2>📊 Distribution of Memory Changes (%)</h2>
<canvas id="histChart" height="250"></canvas>

<h2>📊 Memory: Baseline vs Patched</h2>
<canvas id="memChart" height="400"></canvas>

<h2>⏱️ Time: Baseline vs Patched</h2>
<canvas id="timeChart" height="400"></canvas>

<h2>📉 Top Memory Improvements</h2>
{table(by_mem_diff[:20])}

<h2>📈 Top Memory Regressions</h2>
{table(list(reversed(by_mem_diff[-20:])))}

<h2>⬇️ Top Time Improvements</h2>
{table(by_time_diff[:20])}

<h2>⬆️ Top Time Regressions</h2>
{table(list(reversed(by_time_diff[-20:])))}

<details><summary>Full Comparison ({s['n_matches']} tests)</summary>
{table(sorted(matches, key=lambda m: m['fn']), n=len(matches))}
</details>

<script>
const memData = {json.dumps(mem_data)};
const timeData = {json.dumps(time_data)};

// Filter to tests with significant memory (> 50 MiB)
const sigMem = memData.filter(d => d.base > 50 || d.patch > 50);
sigMem.sort((a,b) => b.base - a.base);

new Chart(document.getElementById('memChart'), {{
    type: 'scatter',
    data: {{
        datasets: [{{
            label: 'Tests (x=baseline, y=patched)',
            data: sigMem.map(d => ({{ x: d.base, y: d.patch }})),
            backgroundColor: sigMem.map(d => d.patch < d.base ? 'rgba(34,134,58,0.6)' : 'rgba(203,36,49,0.6)'),
            pointRadius: 5,
        }}, {{
            label: 'y = x (no change)',
            data: [{{x: 0, y: 0}}, {{x: Math.max(...sigMem.map(d=>d.base))*1.1, y: Math.max(...sigMem.map(d=>d.base))*1.1}}],
            type: 'line',
            borderColor: 'rgba(0,0,0,0.2)',
            borderDash: [5,5],
            pointRadius: 0,
            fill: false,
        }}]
    }},
    options: {{
        plugins: {{ tooltip: {{ callbacks: {{ label: (ctx) => sigMem[ctx.dataIndex]?.full || '' }} }} }},
        scales: {{
            x: {{ title: {{ display: true, text: 'Baseline Memory (MiB)' }} }},
            y: {{ title: {{ display: true, text: 'Patched Memory (MiB)' }} }}
        }}
    }}
}});

const sigTime = timeData.filter(d => d.base > 1 || d.patch > 1);
sigTime.sort((a,b) => b.base - a.base);

new Chart(document.getElementById('timeChart'), {{
    type: 'scatter',
    data: {{
        datasets: [{{
            label: 'Tests (x=baseline, y=patched)',
            data: sigTime.map(d => ({{ x: d.base, y: d.patch }})),
            backgroundColor: sigTime.map(d => d.patch < d.base ? 'rgba(34,134,58,0.6)' : 'rgba(203,36,49,0.6)'),
            pointRadius: 5,
        }}, {{
            label: 'y = x (no change)',
            data: [{{x: 0, y: 0}}, {{x: Math.max(...sigTime.map(d=>d.base))*1.1, y: Math.max(...sigTime.map(d=>d.base))*1.1}}],
            type: 'line',
            borderColor: 'rgba(0,0,0,0.2)',
            borderDash: [5,5],
            pointRadius: 0,
            fill: false,
        }}]
    }},
    options: {{
        plugins: {{ tooltip: {{ callbacks: {{ label: (ctx) => sigTime[ctx.dataIndex]?.full || '' }} }} }},
        scales: {{
            x: {{ title: {{ display: true, text: 'Baseline Time (s)' }} }},
            y: {{ title: {{ display: true, text: 'Patched Time (s)' }} }}
        }}
    }}
}});

// Memory change histogram
const histLabels = {json.dumps([f"{b}%" for b in hist_bins[:-1]])};
const histData = {json.dumps(hist_counts)};
new Chart(document.getElementById('histChart'), {{
    type: 'bar',
    data: {{
        labels: histLabels,
        datasets: [{{
            label: 'Number of tests',
            data: histData,
            backgroundColor: histData.map((_, i) => {{
                const b = {json.dumps(hist_bins)}[i];
                return b < 0 ? 'rgba(34,134,58,0.6)' : (b > 0 ? 'rgba(203,36,49,0.6)' : 'rgba(100,100,100,0.4)');
            }}),
        }}]
    }},
    options: {{
        plugins: {{ legend: {{ display: false }} }},
        scales: {{
            x: {{ title: {{ display: true, text: 'Memory Change (%)' }} }},
            y: {{ title: {{ display: true, text: 'Count' }} }}
        }}
    }}
}});
</script>
</body></html>"""
    return html

# ── Main ─────────────────────────────────────────────────────────────────────

def main():
    parser = argparse.ArgumentParser(description="Generate visual performance comparison from ramon files")
    parser.add_argument("lhs", help="Baseline directory with .ramon files")
    parser.add_argument("rhs", help="Patched directory with .ramon files")
    parser.add_argument("--output", "-o", default="report.html", help="Output HTML file")
    parser.add_argument("--lhs-label", default=None, help="Label for baseline")
    parser.add_argument("--rhs-label", default=None, help="Label for patched")
    args = parser.parse_args()

    lhs = args.lhs.rstrip("/")
    rhs = args.rhs.rstrip("/")

    print(f"Scanning {lhs} ...")
    f1 = find_ramon_files(lhs)
    print(f"  Found {len(f1)} .ramon files")

    print(f"Scanning {rhs} ...")
    f2 = find_ramon_files(rhs)
    print(f"  Found {len(f2)} .ramon files")

    matches, d1, d2 = make_matching(lhs, rhs, f1, f2)
    print(f"  Matched {len(matches)} tests")

    if not matches:
        print("No matching test results found!")
        sys.exit(1)

    stats = compute_stats(matches)

    lhs_label = args.lhs_label or lhs
    rhs_label = args.rhs_label or rhs

    html = generate_html(matches, stats, lhs_label, rhs_label)

    with open(args.output, "w") as f:
        f.write(html)

    print(f"\nReport written to {args.output}")
    print(f"\nSummary:")
    mp = stats["mem_pct"]
    tp = stats["time_pct"]
    hm = stats["heavy_mem_pct"]
    ht = stats["heavy_time_pct"]
    print(f"  All tests ({stats['n_matches']}):")
    print(f"    Memory: median {mp['median']:+.1f}%, mean {mp['mean']:+.1f}%, range [{mp['min']:+.1f}%, {mp['max']:+.1f}%]")
    print(f"    Time:   median {tp['median']:+.1f}%, mean {tp['mean']:+.1f}%, range [{tp['min']:+.1f}%, {tp['max']:+.1f}%]")
    print(f"  Heavy tests ({stats['n_heavy']}, baseline > 100 MiB):")
    print(f"    Memory: median {hm['median']:+.1f}%, mean {hm['mean']:+.1f}%, range [{hm['min']:+.1f}%, {hm['max']:+.1f}%]")
    print(f"    Time:   median {ht['median']:+.1f}%, mean {ht['mean']:+.1f}%, range [{ht['min']:+.1f}%, {ht['max']:+.1f}%]")

if __name__ == "__main__":
    main()
