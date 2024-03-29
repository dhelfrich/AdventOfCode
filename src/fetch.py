from aocd import get_data
from aocd.models import Puzzle
from aocd.utils import AOC_TZ
from aocd.utils import blocker

cookie = "53616c7465645f5fcefe56ba9d0c2c582f54fa0d8aaa0fe6786fd21de405b78ee85fbe9b63c85296778577bda20c34f6cfe3e6e59716fbeeef5266f266b68456"
day = 10
year = 2016
for day in range(1,26):
    file = open("./inputs/{}/day{:02d}.txt".format(year, day),"w")
    input = get_data(cookie, day, year)
    file.write(input)