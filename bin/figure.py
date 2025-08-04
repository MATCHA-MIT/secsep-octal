#!/usr/bin/env python3

import click
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np
from pathlib import Path


from eval import BENCHMARKS, BENCHMARK_PAPER_ORDER, TF


IMG_SIZE = (15, 4.5)
TITLE_PADDING = 25
PALETTE_NAME = "pastel"
OUTPUT_FILENAME = "secsep-overhead"
XLABEL_ROTATION = 10
X_PADDING = 0.20
LEGEND_NCOL = 4
LEGEND_LOC = (0.48, 0)
BAR_WIDTH = 0.9
OVERFLOW_MARK_SHIFT = 0.08

TITLE_FONTSIZE = 20
LABEL_FONTSIZE = 16
LEGEND_FONTSIZE = 14
TICK_FONTSIZE = 14
OVERFLOW_FONTSIZE = 8


TASKS = [
    {
        "title": "Software Overhead",
        "metrics": ["overhead_gem5_cycles_def-off"],
        "ylabel": "Overhead (%)",
        "benchmark_list": BENCHMARKS,
        "tf_list": [TF.ProspectPub, TF.ProspectSec, TF.Secsep, TF.SecsepNoCallPreserv],
        "avg_line": [TF.ProspectPub, TF.ProspectSec, TF.Secsep, TF.SecsepNoCallPreserv],
        "y_ticks": np.concatenate([np.arange(-2, 5, 0.5)]),
        "y_low": -1,
        "y_break": 3.0,
        "ylabel_yloc": 0.5,
    },
    {
        "title": "Comprehensive Overhead",
        "metrics": ["overhead_gem5_cycles_def-on"],
        "ylabel": "Overhead (%)",
        "benchmark_list": BENCHMARKS,
        "tf_list": [TF.ProspectPub, TF.ProspectSec, TF.Secsep, TF.SecsepNoCallPreserv],
        "avg_line": [TF.ProspectPub, TF.ProspectSec, TF.Secsep, TF.SecsepNoCallPreserv],
        "y_ticks": np.concatenate([np.arange(0, 0, 2), np.arange(0, 101, 5)]),
        "y_low": -2,
        "y_break": 30,
        "ylabel_yloc": 0.5,
    },
]


TF_NAME_MAP = {
    TF.Origin: "Original",
    TF.Secsep: "SecSep",
    TF.SecsepNoPushPop: "SecSep (no push/pop TF)",
    TF.SecsepNoCallPreserv: "SecSep (no $C_{callee}$)",
    TF.SecsepNoPushPopNoCallPreserv: "SecSep (no push/pop and call TF)",
    TF.ProspectPub: "ProSpeCT (public stack)",
    TF.ProspectSec: "ProSpeCT (secret stack)",
}

DEF_MODE_NAME_MAP = {
    "overhead_gem5_cycles_def-off": "DefOff",
    "overhead_gem5_cycles_def-on": "DefOn",
}


def load_dataset(path: Path) -> pd.DataFrame:
    extra_args = {"index_col": ["Benchmark", "TF"]}
    if path.suffix == ".csv":
        return pd.read_csv(path, **extra_args)
    elif path.suffix == ".json":
        return pd.read_json(path, **extra_args)
    elif path.suffix == ".xlsx":
        return pd.read_excel(path, **extra_args)
    else:
        raise ValueError(f"Unsupported file format: {path.suffix}")


def draw(
    data: pd.DataFrame,
    ax: plt.Axes,
    title: str,
    metrics: list[str],
    ylabel: str,
    benchmark_list: list[str],
    tf_list: list[TF],
    y_ticks = None,
    y_low: float = None,
    y_break: float = None,
    ylabel_yloc: float = 0.5,
    avg_line: list[TF] = [],
):
    ax.set_title(title, fontsize=TITLE_FONTSIZE, pad=TITLE_PADDING)
    x_left, x_right = -0.5 * BAR_WIDTH - X_PADDING, len(benchmark_list) - 1 + 0.5 * BAR_WIDTH + X_PADDING

    def get_tf_def_str(tf_str, def_str):
        return "{}, {}".format(tf_str, def_str)

    benchmark_revmap = {v: k for k, v in BENCHMARK_PAPER_ORDER.items()}
    data.rename(index=benchmark_revmap, inplace=True)
    benchmark_list = [k for k, v in BENCHMARK_PAPER_ORDER.items() if v in benchmark_list]

    data = data.loc[pd.IndexSlice[benchmark_list, [tf.name for tf in tf_list]], :][metrics]
    data = data[data[metrics].notna()]
    data[metrics] = data[metrics] * 100

    data_reset = data.stack().reset_index()
    data_reset.columns = ["Benchmark", "TF", "DefenseMode", "Value"]
    data_reset["DefenseMode"] = data_reset.apply(lambda row: DEF_MODE_NAME_MAP[row["DefenseMode"]], axis=1)
    data_reset["TF_DefenseMode"] = data_reset.apply(
        lambda row: get_tf_def_str(TF_NAME_MAP[TF[row["TF"]]], row["DefenseMode"]),
        axis=1
    )

    data = data_reset.set_index(["Benchmark", "TF_DefenseMode"])
    data = data[["Value", "TF", "DefenseMode"]]

    hue_order = data.index.get_level_values("TF_DefenseMode").unique()
    palette = sns.color_palette(PALETTE_NAME, n_colors=len(hue_order))
    hue_color_map = dict(zip(hue_order, palette))
    
    sns.barplot(
        data=data,
        x="Benchmark",
        y="Value",
        hue="TF_DefenseMode",
        ax=ax,
        width=BAR_WIDTH,
        edgecolor="black",
        zorder=3,
    )

    if y_ticks is not None:
        ax.set_yticks(y_ticks)

    if y_low is not None:
        ax.set_ylim(ymin=y_low)
    
    if y_break is not None:
        ax.set_ylim(ymax=y_break)
        for bar in ax.patches:
            y = bar.get_height()
            if y > y_break:
                bar.set_height(y_break)
                x = bar.get_x() + bar.get_width() / 2
                ax.text(
                    x,
                    y_break,
                    f"{y:.1f}",
                    ha="center",
                    va="bottom",
                    fontsize=OVERFLOW_FONTSIZE,
                )

    for defense_mode in data["DefenseMode"].unique():
        for tf in avg_line:
            mean_val = data[(data["TF"] == tf.name) & (data["DefenseMode"] == defense_mode)]["Value"].mean()
            color = hue_color_map[get_tf_def_str(TF_NAME_MAP[tf], defense_mode)]
            draw_y = mean_val if mean_val <= y_break else y_break + OVERFLOW_MARK_SHIFT * (ax.get_ylim()[1] - ax.get_ylim()[0])

            ax.axhline(
                y=draw_y,
                color=color,
                linestyle="--",
                linewidth=1.0,
                zorder=0,
            )
            ax.scatter(
                x_left - 0.04,
                draw_y,
                marker=">",
                color=color,
                s=30,
                edgecolors="black",
                linewidths=0.8,
                zorder=0,
                clip_on=False,
            )

            if mean_val > y_break:
                ax.text(
                    x_left + 0.04,
                    draw_y,
                    f"({mean_val:.1f})",
                    ha="left",
                    va="center",
                    fontsize=OVERFLOW_FONTSIZE,
                    color="black",
                    zorder=4,
                )

    ax.set_xlim(left=x_left, right=x_right)

    ax.grid(axis="y", linestyle="--", color="gray", alpha=0.5)
    ax.axhline(y=0, color="black", linewidth=1, alpha=1, zorder=1)
    ax.tick_params(axis="x", length=0, labelrotation=XLABEL_ROTATION, labelsize=TICK_FONTSIZE)
    ax.tick_params(labelsize=TICK_FONTSIZE)

    ax.set_xlabel(None)
    ax.set_ylabel(ylabel=ylabel, fontsize=LABEL_FONTSIZE, x=0.05, y=ylabel_yloc)


@click.command()
@click.argument("dataset-path", type=click.Path(exists=True, dir_okay=False))
def main(dataset_path):
    df = load_dataset(Path(dataset_path))
    out_dir = Path(dataset_path).parent / "figures"

    sns.set_theme(style="ticks", palette=PALETTE_NAME, font_scale=1.5)

    fig, axes = plt.subplots(1, len(TASKS), figsize=IMG_SIZE, sharey=False)

    for task, ax in zip(TASKS, axes):
        draw(df, ax=ax, **task)
        ax.get_legend().remove()

    handles, labels = axes[0].get_legend_handles_labels()
    fig.legend(handles, labels,
               loc="lower center",
               ncol=LEGEND_NCOL,
               fontsize=LEGEND_FONTSIZE,
               frameon=True,
               bbox_to_anchor=LEGEND_LOC)
    plt.tight_layout(rect=[0, 0.08, 0.95, 1])

    out_dir.mkdir(parents=True, exist_ok=True)
    filename = f"{OUTPUT_FILENAME}.pdf"
    plt.savefig(out_dir / filename, dpi=100, bbox_inches="tight", pad_inches=0)
    print(f"Figure saved to {out_dir / filename}")


if __name__ == "__main__":
    main()
