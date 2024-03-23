import sys
import os
import re
import pathlib
from urllib.request import urlopen
from bs4 import BeautifulSoup


def get_parser_name() -> str:
    """Get parser name for bs4"""
    return "html.parser"


def int_to_alphabets(i: int) -> str:
    """Convert an integer to corresponding alphabet

    Example:
    >>> int_to_alphabets(5)
    'e'
    >>> int_to_alphabets(28)
    'ab'
    """
    m = 26
    l = 1
    while i >= m:
        i -= m
        m = m * m
        l += 1
    s = ""
    for k in range(l):
        s = str(chr(ord("a") + i % 26)) + s
        i //= 26
    return s


def download_html(url: str) -> str:
    """Download specified URL and returns as html string"""
    print("download", url)
    with urlopen(url) as res:
        body = res.read()
        charset = res.headers.get_content_charset()
    html = body.decode(charset)
    return html


def save_sample_files(prefix, samples, directory):
    for i, sample in enumerate(samples):
        filename = prefix + str(i + 1) + ".txt"
        path = directory / filename
        with open(path, "w") as f:
            pre = sample.find_next_sibling("pre")
            if pre is None:
                if section := sample.find_next_sibling("section"):
                    pre = section.find("pre")
            if pre is None:
                print("Warning: could not find any testcase")
            else:
                txt = pre.text.lstrip()
                f.write(txt)


def save_samples(contest_id: str, task_id: str, base_dir: pathlib.Path):
    full_id = re.sub("-", "_", contest_id + "_" + task_id)
    url_base = "https://atcoder.jp/contests/"
    url = url_base + contest_id + "/tasks/" + full_id
    html = download_html(url)

    parser = get_parser_name()
    soup = BeautifulSoup(html, parser)
    sample_inputs = soup.find_all("h3", string=re.compile(r"入力例 ?\d*"))
    sample_outputs = soup.find_all("h3", string=re.compile(r"出力例 ?\d*"))

    directory = base_dir / task_id
    os.makedirs(directory, exist_ok=True)
    save_sample_files("in-", sample_inputs, directory)
    save_sample_files("out-", sample_outputs, directory)


def make_task_id_list(contest_id):
    url_base = "https://atcoder.jp/contests/"
    url = url_base + contest_id + "/tasks/"
    html = download_html(url)

    parser = get_parser_name()
    soup = BeautifulSoup(html, parser)
    table = soup.find("h2").parent
    task_num = len(table.tbody.find_all("tr"))
    task_id_list = [int_to_alphabets(i) for i in range(task_num)]
    return task_id_list


def gen_testcases(contest_id: str, base_dir: pathlib.Path) -> None:
    task_id_list = make_task_id_list(contest_id)
    testcase_dir = base_dir / "testcases"
    for task_id in task_id_list:
        save_samples(contest_id, task_id, testcase_dir)


def new_contest(args):
    contest_id = args.contest_id
    pth = pathlib.Path(contest_id)
    pth.mkdir(exist_ok=True)
    gen_testcases(contest_id, pth)
