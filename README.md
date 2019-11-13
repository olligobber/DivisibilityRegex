# DivisibilityRegex
Generates Regular Expressions to match any multiple of a number (in any base)

## Usage
When `main.hs` is compiled, the resulting program takes 1-3 arguments; 
* the first is the natural number to match multiples of, 
* the second is the number of attempts to make to achieve a regex with minimal length, default is 1, type 0 to try all orders,
* the third is the base of the numbering system, from 2 to 36, default is 10

## Example usage

```
$ ghc -dynamic main.hs
$ ./main 2          # try one attempt at multiples of 2 in base 10
([02468]|[13579][13579]*[02468])*
$
$ ./main 2          # try another attempt at multiples of 2 in base 10
([02468]*[13579]([13579]|[02468][02468]*[13579])*[02468][02468]*|[02468]*)
$
$ ./main 2 1 2      # one attempt at multiples of 2 in base 2
(0|11*0)*
$
$ ./main 2 1 2      # another attempt at multiples of 2 in base 2
(0*1(1|00*1)*00*|0*)
$
$ ./main 7 | wc     # one attempt at multiples of 7 in base 10, just output the length of the regex
      1       1   60939
$
$ ./main 7 10 | wc  # try ten attempts at multiples of 7 in base 10
      1       1   12605
$
$ ./main 7 0 | wc   # try all ways to find multiples of 7 in base 10
      1       1   11621
```
