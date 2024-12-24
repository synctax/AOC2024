from pathlib import Path


def basen(n, b):
    if n == 0:
        return '0'
    nums = []
    while n:
        n, r = divmod(n, b)
        nums.append(str(r))
    return ''.join(reversed(nums))


def doTheThing(input: [str], ops: [str]):
    count = 0
    for i in input:
        words = i.split(' ')
        target = words[0][:-1]
        row = words[1:]
        keys = [basen(x, len(ops)).rjust(len(row)-1, '0')
                for x in range(pow(len(ops), len(row)-1))]
        for k in keys:
            value = int(row[0])
            for i, v in enumerate(k):
                combine = str(value)+ops[int(v)]+row[i+1]
                value = eval(combine)
            if value == int(target):
                count += value
                break
    return count


def part1(input):
    return doTheThing(input, ['+', '*'])


def part2(input):
    return doTheThing(input, ['+', '*', ''])


input = Path('input.txt').read_text().splitlines()
print(part1(input))
print(part2(input))
