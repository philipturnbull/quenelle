def reverse_upper(s):
    return s.upper()[::-1]

numbers = 'one two three'
SREBMUN = map(lambda x: reverse_upper(x), numbers.split())

print SREBMUN
