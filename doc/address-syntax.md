# Address syntax

Broadly speaking the syntax for specifying addresses in ad is inspired by the syntax
from Sam and Acme. Simple addresses can be composed together into compound addresses
for specifying related or larger areas of a buffer.

### Simple addresses
- '.'      the current value of dot in the buffer
- '0'      BOF
- '$'      EOF
- 'n'      select line n
- '+n'     move forward n lines
- '-n'     move backward n lines

- '#n'     select character n
- '+#n'    move forward n characters
- '-#n'    move backward n characters

- '/re/'   search forward for a regular expression relative to the current dot
- '+/re/'  same as above
- '-/re/'  search backward for a regular expression relative to the current dot


### Suffixes
- '..+'    extend to the end of the line containing the target address
- '..-'    extend to the start of the line containing the target address

- '..-+'   select the line containing the current address
- '..+-'   select the line containing the current address
  - In both cases, this is simply the composition of the '-' and '+' suffixes
    acting together to expand out to full line containing the address.

- '..+$ad' extend forward to the end of $ad
- '..-$ad' extend backward to the start of $ad


### Compound addresses
- '$a1,'     select the region from the start of $a1 to EOF
- ',$a1'     select the region from the BOF to the end of $a2
- '$a1,$a2'  select the region from the start of $a1 to the end of $a2
