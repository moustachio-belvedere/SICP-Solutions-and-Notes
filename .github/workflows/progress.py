import re
import os

n_in_chapters = [46, 97, 82, 79, 52]
n_done_clist  = [0] * len(n_in_chapters)
cpercentages  = [0] * len(n_in_chapters)
n_total = sum(n_in_chapters)

n_done_total = 0

for chapnum in range(1, 5):
    files = os.listdir("chapter0{}".format(chapnum))
    exs_in_chapter = filter(lambda x: re.search("ex_\d-\d+", x), files)
    if exs_in_chapter:
        exs_unique = set(map(lambda x: int(re.search("ex_\d-(\d+)", x)
                         .group(1)), exs_in_chapter))
        n_done_chapter = len(exs_unique)
        n_done_clist[chapnum - 1] = n_done_chapter
        cpercentages[chapnum - 1] = n_done_chapter/n_in_chapters[chapnum - 1]
        n_done_total += n_done_chapter

tpercent = "{:.2f}".format(n_done_total/n_total)
cpercentages = ["{:.2f}".format(x) for x in cpercentages]

print(cpercentages)
print(tpercent)
