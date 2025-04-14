#!/usr/bin/env python3

import click
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np
from pathlib import Path


from eval import Benchmark, TF, EVAL_DIR


TASKS = [
    {
        "metric": "overhead_gem5_cycles_def-off",
        "ylabel": "Overhead (%)",
        "benchmark_list": list(Benchmark),
        "tf_list": [TF.ProspectPub, TF.ProspectSec, TF.Octal],
        "legend_loc": (0.50, -0.15),
        "break_y": 3,
        "ylabel_yloc": 0.4,
        "legend_ncol": 3,
    },
    {
        "metric": "overhead_gem5_cycles_def-on",
        "ylabel": "Overhead (%)",
        "benchmark_list": list(Benchmark),
        "tf_list": [
            TF.Origin,
            TF.ProspectPub,
            TF.ProspectSec,
            TF.Octal,
            TF.OctalNoCallPreserv,
        ],
        "legend_loc": (0.50, -0.15),
        "break_y": 15,
        "ylabel_yloc": 0.3,
        "legend_ncol": 2,
    },
]


TF_NAME_MAP = {
    TF.Origin: "Original",
    TF.Octal: "SecSep",
    TF.OctalNoPushPop: "SecSep (no push/pop TF)",
    TF.OctalNoCallPreserv: "SecSep*",
    TF.OctalNoPushPopNoCallPreserv: "SecSep (no push/pop and call TF)",
    TF.ProspectPub: "ProSpeCT (public stack)",
    TF.ProspectSec: "ProSpeCT (secret stack)",
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
    metric: str = None,
    ylabel: str = None,
    benchmark_list: list[Benchmark] = list(Benchmark),
    tf_list: list[TF] = list(TF),
    # break_y: tuple[float, float, float] = None,
    break_y: float = None,
    bar_width: float = 0.5,
    img_size: tuple[int, int] = (8, 3),
    fontsize_label: int = 16,
    fontsize_tick: int = 12,
    fontsize_legend: int = 12,
    fontsize_value: int = 12,
    legend_loc: tuple[float, float] = (0.5, -0.35),
    top_label_offset: float = 1.2,
    ylabel_yloc: float = 0.5,
    legend_ncol: int = 2,
):
    benchmark_list = [bench.name for bench in benchmark_list]
    tf_list = [tf.name for tf in tf_list]

    data = data.loc[pd.IndexSlice[benchmark_list, tf_list], :][[metric]]
    data = data[data[metric].notna()]
    data[metric] = data[metric] * 100
    data.index = data.index.set_levels(
        data.index.levels[data.index.names.index("Benchmark")].str.lower(),
        level="Benchmark"
    )

    plt.figure(figsize=img_size)
    sns.set_theme(style="ticks", palette="pastel", font_scale=1.5)

    ax = sns.barplot(
        data=data,
        x="Benchmark",
        y=metric,
        hue="TF",
        width=bar_width,
        edgecolor="black",
    )
    fig = ax.get_figure()
    
    if break_y is not None:
        ax.set_ylim(ymax=break_y)

        # offset_y = {}
        # for bar in ax.patches:
        #     y = bar.get_height()
        #     if y > break_y:
        #         offset_y[bar.get_x()] = top_label_offset
        # sorted_x = sorted(offset_y.keys(), reverse=True)
        # print(sorted_x)
        # print(offset_y)
        # for i in range(len(sorted_x) - 1):
        #     if abs(sorted_x[i + 1] - sorted_x[i]) <= bar_width:
        #         offset_y[sorted_x[i + 1]] += offset_y[sorted_x[i]]
            
        for bar in ax.patches:
            y = bar.get_height()
            if y > break_y:
                bar.set_height(break_y)
                x = bar.get_x() + bar.get_width() / 2
                ax.text(
                    x,
                    break_y,
                    f"{y:.0f}",
                    ha="center",
                    va="bottom",
                    fontsize=fontsize_value,
                )

    ax.grid(axis="y", linestyle="--", color="gray", alpha=0.5)
    plt.xticks(rotation=0, fontsize=fontsize_tick)
    ax.tick_params(axis="x", length=0)
    ax.tick_params(labelsize=fontsize_tick)

    handles, labels = ax.get_legend_handles_labels()
    new_labels = [TF_NAME_MAP[TF[label]] for label in labels]
    ax.legend(
        handles=handles,
        labels=new_labels,
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
    plt.savefig(output_dir / f"{metric}.pdf", dpi=100, bbox_inches="tight", pad_inches=0)


@click.command()
@click.argument("dataset-path", type=click.Path(exists=True, dir_okay=False))
def main(dataset_path):
    df = load_dataset(Path(dataset_path))
    out_dir = EVAL_DIR / "figures"
    for task in TASKS:
        draw(df, out_dir, **task)


if __name__ == "__main__":
    main()


    # ax_upper = None
    # if break_y:
    #     (ymin1, ymax1, yden1), (ymin2, ymax2, yden2), inchout = break_y
    #     fig, (ax_upper, ax) = plt.subplots(
    #         2, 1, sharex=True, figsize=img_size
    #     )
    #     for a in [ax_upper, ax]:
    #         sns.barplot(
    #             ax=a,
    #             data=data,
    #             x="Benchmark",
    #             y=metric,
    #             hue="TF",
    #             width=bar_width,
    #             edgecolor="black",
    #         )

    #     inchout_lower = ymax1 + int(yden1 * inchout)
    #     inchout_upper = ymin2 - int(yden2 * inchout)
    #     if inchout_lower > inchout_upper:
    #         mid = int((inchout_lower + inchout_upper) / 2)
    #         inchout_upper = inchout_lower = mid
    #     ax.set_ylim(ymin1, inchout_lower)
    #     ax_upper.set_ylim(inchout_upper, ymax2)

    #     ax_upper.spines.bottom.set_visible(False)
    #     ax.spines.top.set_visible(False)
    #     ax.xaxis.tick_bottom()
    #     ax_upper.tick_params(axis="x", length=0)
    #     ax_upper.set_xlabel(None)
    #     ax_upper.set_ylabel(None)
    #     ax_upper.get_legend().remove()
    #     ax_upper.grid(axis="y", linestyle="--", color="gray", alpha=0.5)
    #     ax_upper.set_yticks(np.arange(ymin2, ymax2 + 1, yden2))
    #     ax.set_yticks(np.arange(ymin1, ymax1 + 1, yden1))

    #     d = 0.015
    #     kwargs = dict(transform=ax_upper.transAxes, color="black", clip_on=False)
    #     ax_upper.plot((-d, +d), (-d, +d), **kwargs)
    #     ax_upper.plot((1 - d, 1 + d), (-d, +d), **kwargs)
    #     kwargs.update(transform=ax.transAxes)
    #     ax.plot((-d, +d), (1 - d, 1 + d), **kwargs)
    #     ax.plot((1 - d, 1 + d), (1 - d, 1 + d), **kwargs)
    # else:
    #     ax = sns.barplot(
    #         data=data,
    #         x="Benchmark",
    #         y=metric,
    #         hue="TF",
    #         width=bar_width,
    #         edgecolor="black",
    #     )
    #     fig = ax.get_figure()