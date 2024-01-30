from aocd import get_data
from aocd.models import Puzzle
from aocd.utils import AOC_TZ
from aocd.utils import blocker

cookie = "53616c7465645f5f4c27a799f2ae4dc0f0ce3152ee2bd5b1caf4260ac7018b0b8412032dcf6dcd2b7e356bcceebf6a990f68e9ce120fedd2d10ff0e4fb980834"
day = 10
for day in range(1,26):
    file = open("./inputs/day{:02d}.txt".format(day),"w")
    input = get_data(cookie, day, 2015)
    file.write(input)