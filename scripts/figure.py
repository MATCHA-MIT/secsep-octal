#!/usr/bin/env python3

import math
import click
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np
import re
from pathlib import Path


from eval import BENCHMARKS, BENCHMARK_PAPER_ORDER, TF


our_palette = sns.color_palette("pastel")
our_palette[0], our_palette[1], our_palette[2], our_palette[3] = our_palette[3], our_palette[0], our_palette[1], our_palette[2]

TF_NAME_MAP = {
    TF.Origin: "Original",
    TF.Secsep: "SecSep",
    TF.SecsepNoPushPop: "SecSep (no push/pop TF)",
    TF.SecsepNoCallPreserv: "SecSep (no $C_{callee}$)",
    TF.SecsepNoPushPopNoCallPreserv: "SecSep (no push/pop and call TF)",
    TF.ProspectPub: "ProSpeCT (public stack)$^{*}$",
    TF.ProspectSec: "ProSpeCT (secret stack)",
}

# Efficiency figure settings
EFF_IMG_SIZE = (5, 5)
EFF_XLABEL_ROTATION = 10
EFF_TICK_FONTSIZE = 8
EFF_LABEL_FONTSIZE = 14
EFF_LABEL_PADDING = 10
EFF_TITLE_FONTSIZE = 10
EFF_TITLE_PADDING = 10

# Overhead figure settings

IMG_SIZE = (17, 3.5)
TITLE_PADDING = 25
XLABEL_ROTATION = 10
X_PADDING = 0.20
LEGEND_NCOL = 4
LEGEND_LOC = (0.48, -0.02)
BAR_WIDTH = 0.85
OVERFLOW_MARK_SHIFT = 0.08

TITLE_FONTSIZE = 16
LABEL_FONTSIZE = 16
LEGEND_FONTSIZE = 14
TICK_FONTSIZE = 14
BAR_TEXT_FONTSIZE = 8
BAR_TEXT_SHIFT = {
    26.3: (0.02, 0.0),
}

OVERHEAD_TASKS = [
    {
        "title": "(a) Software overhead with hardware defense disabled",
        "metric": "overhead_gem5_cycles_def-off",
        "ylabel": "Overhead (%)",
        "benchmark_list": BENCHMARKS,
        "tf_list": [TF.ProspectPub, TF.ProspectSec, TF.SecsepNoCallPreserv, TF.Secsep],
        "y_ticks": np.concatenate([np.arange(-2, 6, 0.5)]),
        "y_low": -1,
        "y_break": 5.0,
        "y_tick_jmp": 2,
        "ylabel_yloc": 0.5,
        "draw_avg": True,
    },
    {
        "title": "(b) Comprehensive overhead with hardware defense enabled",
        "metric": "overhead_gem5_cycles_def-on",
        "ylabel": "Overhead (%)",
        "benchmark_list": BENCHMARKS,
        "tf_list": [TF.ProspectPub, TF.ProspectSec, TF.SecsepNoCallPreserv, TF.Secsep],
        "y_ticks": np.concatenate([np.arange(0, 0, 2), np.arange(0, 101, 10)]),
        "y_low": -5,
        "y_break": 100,
        "y_tick_jmp": 2,
        "ylabel_yloc": 0.5,
        "draw_avg": True,
    },
]


def load_dataset(path: Path, **extra_args) -> pd.DataFrame:
    if path.suffix == ".csv":
        return pd.read_csv(path, **extra_args)
    elif path.suffix == ".json":
        return pd.read_json(path, **extra_args)
    elif path.suffix == ".xlsx":
        return pd.read_excel(path, **extra_args)
    else:
        raise ValueError(f"Unsupported file format: {path.suffix}")


def draw(
    df: pd.DataFrame,
    ax: plt.Axes,
    title: str,
    metric: str,
    ylabel: str,
    benchmark_list: list[str],
    tf_list: list[TF],
    y_ticks = None,
    y_low: float = None,
    y_break: float = 100.0,
    y_tick_jmp: int = 1,
    y_tick_jmp_off: int = 0,
    ylabel_yloc: float = 0.5,
    draw_avg: bool = False,
):
    ax.set_title(title, fontsize=TITLE_FONTSIZE, pad=TITLE_PADDING)

    # Rename benchmarks to their display names
    benchmark_revmap = {v: k for k, v in BENCHMARK_PAPER_ORDER.items()}
    df.rename(index=benchmark_revmap, inplace=True)
    benchmark_list = [k for k, v in BENCHMARK_PAPER_ORDER.items() if v in benchmark_list]

    # Focus on selected metric
    df = df.loc[pd.IndexSlice[benchmark_list, [tf.name for tf in tf_list]], [metric]]
    # Convert to percentage
    df[metric] = df[metric] * 100

    # Rename TF index to their display names
    tf_map = {k.name: v for k, v in TF_NAME_MAP.items()}
    df.rename(index=tf_map, level="TF", inplace=True)

    if draw_avg:
        avg_df = df.groupby(level="TF").mean()
        avg_df[metric] = avg_df[metric].round(2)
        avg_df = avg_df.reset_index()
        avg_df["Benchmark"] = "Avg"
        df = pd.concat([df.reset_index(), avg_df], ignore_index=True).set_index(["Benchmark", "TF"])
        print(df.loc["Avg", metric])


    hue_order = df.index.get_level_values("TF").unique()
    hue_color_map = dict(zip(hue_order, our_palette))
    
    sns.barplot(
        data=df,
        x="Benchmark",
        y=metric,
        hue="TF",
        ax=ax,
        width=BAR_WIDTH,
        edgecolor="black",
        zorder=3,
    )

    num_groups = df.index.get_level_values('Benchmark').nunique()
    x_left, x_right = -0.5 * BAR_WIDTH - X_PADDING, num_groups - 1 + 0.5 * BAR_WIDTH + X_PADDING
    ax.set_xlim(left=x_left, right=x_right)

    def is_avg_bar(bar):
        return abs(bar.get_x() - (num_groups - 1)) <= 0.5
    
    def get_text_shift(value):
        for k, v in BAR_TEXT_SHIFT.items():
            if abs(value - k) < 0.1:
                return v
        return 0.0, 0.0

    if y_ticks is not None:
        ax.set_yticks(y_ticks)
    for bar in ax.patches:
        y = bar.get_height()
        draw_text = y_break if y > y_break else (y if is_avg_bar(bar) else None)
        if y > y_break:
            bar.set_height(y_break)
        if draw_text is not None:
            x = bar.get_x() + bar.get_width() / 2
            sx, sy = get_text_shift(y)
            ax.text(
                x + sx,
                draw_text + sy,
                f"{y:.1f}",
                ha="center",
                va="bottom",
                fontsize=BAR_TEXT_FONTSIZE,
            )
    ax.set_ylim(ymax=y_break)
    if y_low is not None:
        ax.set_ylim(ymin=y_low)
    yticklabels = [label.get_text() for label in ax.get_yticklabels()]
    new_yticklabels = [label if (i + y_tick_jmp_off) % y_tick_jmp == 0 else ""
                       for i, label in enumerate(yticklabels)]
    ax.set_yticklabels(new_yticklabels)

    ax.grid(axis="y", linestyle="--", color="gray", alpha=0.5)
    ax.axhline(y=0, color="black", linewidth=1, alpha=1, zorder=1)

    ax.tick_params(labelsize=TICK_FONTSIZE)
    ax.tick_params(axis="x", labelrotation=XLABEL_ROTATION, labelsize=TICK_FONTSIZE)

    ax.set_xlabel(None)
    ax.set_ylabel(ylabel=ylabel, fontsize=LABEL_FONTSIZE, x=0.05, y=ylabel_yloc)

    
def draw_overhead(eval_dir: Path):
    df = load_dataset(eval_dir / "eval.csv", index_col=["Benchmark", "TF"])

    sns.set_theme(style="ticks", palette=our_palette, font_scale=1.5)
    fig, axes = plt.subplots(1, len(OVERHEAD_TASKS), figsize=IMG_SIZE, sharey=False)

    for task, ax in zip(OVERHEAD_TASKS, axes):
        draw(df, ax=ax, **task)
        ax.get_legend().remove()

    handles, labels = axes[0].get_legend_handles_labels()
    fig.legend(handles, labels,
               loc="lower center",
               ncol=LEGEND_NCOL,
               fontsize=LEGEND_FONTSIZE,
               frameon=True,
               bbox_to_anchor=LEGEND_LOC)
    plt.tight_layout(rect=[0, 0.04, 0.99, 0.98])

    out_dir = eval_dir / "figures"
    out_dir.mkdir(exist_ok=True)
    filename = "secsep-overhead.pdf"
    plt.savefig(out_dir / filename, dpi=100, bbox_inches="tight", pad_inches=0)
    print(f"Overhead figure saved to {out_dir / filename}")

    
def draw_efficiency(eval_dir: Path):
    df = load_dataset(eval_dir / "secsep.csv", index_col=["Benchmark"])

    benchmark_order = BENCHMARK_PAPER_ORDER.keys()
    df = df.reset_index()
    df["Benchmark"] = df["Benchmark"].map({v: k for k, v in BENCHMARK_PAPER_ORDER.items()})
    df["Benchmark"] = pd.Categorical(
        df["Benchmark"],
        categories=benchmark_order,
        ordered=True
    )
    df = df.sort_values("Benchmark")

    metric_pat = re.compile(r"^(infer|check)_")
    metric_cols = [c for c in df.columns if re.match(metric_pat, c)]
    df = df.reset_index()
    long = df.melt(id_vars="Benchmark",
                   value_vars=metric_cols,
                   var_name="Phase_Metric",
                   value_name="Value")
    long[["Phase", "Metric"]] = long["Phase_Metric"].str.split("_", n=1, expand=True)
    tidy = (
        long
        .pivot(index=["Benchmark", "Phase"], columns="Metric", values="Value")
        .reset_index()
    )
    tidy.columns.name = None
    tidy = tidy.set_index(["Benchmark", "Phase"])
    tidy = tidy[["time", "smt_queries"]]

    fig, (ax1, ax2) = plt.subplots(2, 1, figsize=EFF_IMG_SIZE)

    sns.set_theme(style="ticks", palette="Set2")
    for ax, y, title in [(ax1, "time", "Time (s)"), (ax2, "smt_queries", "SMT Queries")]:
        sns.lineplot(
            data=tidy,
            x="Benchmark",
            y=y,
            hue="Phase",
            marker="o",
            ax=ax,
            legend=True,
        )
        ax.set_xlabel(None)
        ax.legend(loc="upper left")
        ax.tick_params(axis="x", labelrotation=XLABEL_ROTATION, labelsize=EFF_TICK_FONTSIZE)
        ax.tick_params(axis="y", labelsize=EFF_TICK_FONTSIZE)
        ax.grid(axis="y", linestyle="--", color="gray", alpha=0.5)
        ax.set_title(title, fontsize=EFF_TITLE_FONTSIZE, pad=EFF_TITLE_PADDING)
        ax.set_ylabel(None)

    fig.tight_layout()

    out_dir = eval_dir / "figures"
    out_dir.mkdir(exist_ok=True)
    filename = "secsep-efficiency.pdf"
    fig.savefig(out_dir / filename, dpi=100, bbox_inches="tight", pad_inches=0)
    print(f"Efficiency figure saved to {out_dir / filename}")


@click.command()
@click.argument("eval-dir", type=click.Path(exists=True, file_okay=False))
def main(eval_dir):
    eval_dir = Path(eval_dir)
    draw_efficiency(eval_dir)
    draw_overhead(eval_dir)


if __name__ == "__main__":
    main()
