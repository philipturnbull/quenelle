def reverse_upper(s):
    return s.upper()[::-1]

def reverse_lower(s):
    return s.lower(s)[::-1]

numbers = 'one two three'
SREBMUN = map(lambda x: reverse_upper(x), numbers.split())
srebmun = map(lambda y: reverse_lower(y), reversed(numbers.split()))

print SREBMUN
print srebmun
