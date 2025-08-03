#!/usr/bin/env python3

import click
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np
from pathlib import Path


from eval import BENCHMARKS, BENCHMARK_PAPER_ORDER, TF


TASKS = [
    {
        "output_filename": "secsep-overhead",
        "img_size": (16, 5),
        "metrics": ["overhead_gem5_cycles_def-off", "overhead_gem5_cycles_def-on"],
        "ylabel": "Overhead (%)",
        "benchmark_list": BENCHMARKS,
        "tf_list": [TF.ProspectPub, TF.ProspectSec, TF.SecsepNoCallPreserv, TF.Secsep],
        "bar_width": 0.9,
        "legend_loc": (0.50, -0.10),
        "y_ticks": np.concatenate([np.arange(-2, 5, 0.5)]),
        "y_low": -1,
        "y_break": 3.5,
        "ylabel_yloc": 0.3,
        "legend_ncol": 4,
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
    output_dir: Path,
    output_filename: str,
    metrics: list[str],
    ylabel: str,
    benchmark_list: list[str],
    tf_list: list[TF],
    y_ticks = None,
    y_low: float = None,
    y_break: float = None,
    bar_width: float = 0.8,
    img_size: tuple[int, int] = (8, 4),
    fontsize_label: int = 16,
    fontsize_tick: int = 14,
    fontsize_legend: int = 14,
    fontsize_value: int = 12,
    legend_loc: tuple[float, float] = (0.5, -0.35),
    top_label_offset: float = 1.2,
    xlabel_rotation: float = 0,
    ylabel_yloc: float = 0.5,
    legend_ncol: int = 2,
):
    benchmark_revmap = {v: k for k, v in BENCHMARK_PAPER_ORDER.items()}
    data.rename(index=benchmark_revmap, inplace=True)
    benchmark_list = [k for k, v in BENCHMARK_PAPER_ORDER.items() if v in benchmark_list]

    data = data.loc[pd.IndexSlice[benchmark_list, [tf.name for tf in tf_list]], :][metrics]
    data = data[data[metrics].notna()]
    data[metrics] = data[metrics] * 100

    data_reset = data.stack().reset_index()
    data_reset.columns = ["Benchmark", "TF", "DefenseMode", "Value"]
    data_reset["TF_DefenseMode"] = data_reset.apply(
        lambda row: "{}, {}".format(TF_NAME_MAP[TF[row["TF"]]], DEF_MODE_NAME_MAP[row["DefenseMode"]]),
        axis=1
    )
    
    data = data_reset.set_index(["Benchmark", "TF_DefenseMode"])
    data = data[["Value", "TF"]]

    plt.figure(figsize=img_size)
    reversed_set2 = sns.color_palette("Set2")[::-1]
    sns.set_theme(style="ticks", palette=reversed_set2, font_scale=1.5)

    ax = sns.barplot(
        data=data,
        x="Benchmark",
        y="Value",
        hue="TF_DefenseMode",
        width=bar_width,
        edgecolor="black",
    )
    fig = ax.get_figure()

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
                    f"{y:.0f}",
                    ha="center",
                    va="bottom",
                    fontsize=fontsize_value,
                )

    ax.grid(axis="y", linestyle="--", color="gray", alpha=0.5)
    plt.xticks(rotation=xlabel_rotation, fontsize=fontsize_tick)
    ax.tick_params(axis="x", length=0)
    ax.tick_params(labelsize=fontsize_tick)

    handles, _ = ax.get_legend_handles_labels()
    ax.legend(
        handles=handles,
        title_fontsize=fontsize_legend,
        loc="upper center",
        bbox_to_anchor=legend_loc,
        fontsize=fontsize_legend,
        ncol=legend_ncol,
    )

    ax.set_xlabel(None)
    ax.set_ylabel(ylabel=ylabel, fontsize=fontsize_label, x=0.05, y=ylabel_yloc)

    fig.tight_layout()
    output_dir.mkdir(parents=True, exist_ok=True)
    filename = f"{output_filename}.pdf"
    plt.savefig(output_dir / filename, dpi=100, bbox_inches="tight", pad_inches=0)
    print(f"Saved figure to {output_dir / filename}")


@click.command()
@click.argument("dataset-path", type=click.Path(exists=True, dir_okay=False))
def main(dataset_path):
    df = load_dataset(Path(dataset_path))
    out_dir = Path(dataset_path).parent / "figures"
    for task in TASKS:
        draw(df, out_dir, **task)


if __name__ == "__main__":
    main()
