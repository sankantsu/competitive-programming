import sys
import os
import re
from urllib.request import urlopen
from bs4 import BeautifulSoup

def usage():
    print("usage:",os.path.basename(sys.argv[0]),"contest-id","task-id=all")
    exit(0)

def int_to_alphabets(i):
    m = 26
    l = 1
    while (i >= m):
        i -= m
        m = m*m
        l += 1
    s = ""
    for k in range(l):
        s = str(chr(ord("a")+i%26)) + s
        i //= 26
    return s

def download_html(url):
    with urlopen(url) as res:
        body = res.read()
        charset = res.headers.get_content_charset()
    html = body.decode(charset)
    return html

def save_sample_files(prefix,samples,directory):
    for i,sample in enumerate(samples):
        file = directory + prefix + str(i+1) + ".txt"
        with open(file,"w") as f:
            pre = sample.find_next_sibling("pre")
            if pre == None:
                pre = sample.find_next_sibling("section").find("pre")
            txt = pre.text.lstrip()
            f.write(txt);

def save_samples(contest_id,task_id):
    full_id = re.sub('-','_',contest_id + "_" + task_id)
    url_base = "https://atcoder.jp/contests/"
    url = url_base + contest_id + "/tasks/" + full_id
    html = download_html(url)

    parser = "lxml"
    soup = BeautifulSoup(html,parser)
    sample_inputs = soup.find_all("h3",string=re.compile(r"入力例 ?\d*"))
    sample_outputs = soup.find_all("h3",string=re.compile(r"出力例 ?\d*"))

    directory = "testcases/" + task_id + "/"
    os.makedirs(directory,exist_ok=True)
    save_sample_files("in-",sample_inputs,directory)
    save_sample_files("out-",sample_outputs,directory)

def make_task_id_list(contest_id):
    url_base = "https://atcoder.jp/contests/"
    url = url_base + contest_id + "/tasks/"
    html = download_html(url)

    parser = "lxml"
    soup = BeautifulSoup(html,parser)
    table = soup.find("h2").parent
    task_num = len(table.tbody.find_all("tr"))
    task_id_list = [int_to_alphabets(i) for i in range(task_num)]
    return task_id_list

if __name__ == "__main__":
    argc = len(sys.argv)
    if not (argc == 2 or argc == 3):
        usage()

    contest_id = sys.argv[1]
    if (argc == 3):
        task_id = sys.argv[2]
        save_samples(contest_id,task_id)
    else:
        task_id_list = make_task_id_list(contest_id)
        for task_id in task_id_list:
            save_samples(contest_id,task_id)
