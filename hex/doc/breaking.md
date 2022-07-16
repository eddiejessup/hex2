# Breaking (paragraphs and pages)

From tex-by-topic (p 66):

> Glue shrinking can lead to ‘overfull’ boxes: a box is called overfull if the available shrink is less than the shrink necessary to meet the box specification.

## Encoding page-cost formula into code

Source:

```python
if b < ∞ and p ≤ −10000 and q < 10000:
  p
if b < 10000 and −10000 < p < 10000 and q < 10000:
  b+p+q
if b = 10000 and −10000 < p < 10000 and q < 10000:
  100000
 if (b = ∞ or q ≥ 10000) and p < 10000:
  ∞
```

first, check whether b is infinite:

```python
if b = inf:
  c = inf
```

left with:

```python
if p ≤ −10000 and q < 10000:
  p
if b < 10000 and −10000 < p < 10000 and q < 10000:
  b+p+q
if b = 10000 and −10000 < p < 10000 and q < 10000:
  100000
 if q ≥ 10000 and p < 10000:
  ∞
```

i am going to assume p < 10k, because otherwise the equations aren't complete (what happens if q >= 10k and p = 10k?)

so now:

```python
if p ≤ −10000 and q < 10000:
  p
if b < 10000 and p > −10000 and q < 10000:
  b+p+q
if b = 10000 and p > −10000 and q < 10000:
  100000
if q ≥ 10000:
  ∞
```

now i will add a check:

```python
if q >= 10000:
  infty
```

actually can fold that into the previous one:

```python
if (b = infty or q >= 10k):
  infty
```

now we have:

```python
if p ≤ −10000:
  p
if b < 10000 and p > −10000:
  b+p+q
if b = 10000 and p > −10000:
  100000
```

add a check:

```python
if p <= -10k:
  p
```

now we have:

```python
if b < 10000:
  b+p+q
if b = 10000:
  100000
```

add a check:

```python
if b = 10000:
  100000
else:
  b+p+q
```

all together:

```python
if (b = infty or q >= 10k):
  infty
if p <= -10k:
  p
if b = 10000:
  100000
else:
  b+p+q
```
