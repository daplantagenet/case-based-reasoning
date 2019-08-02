## 0.22

### Bugfixes

- Weighted distance calculation was wrongly calculated, namely: $abs(sum(x-y))$ to $sum(abs(x-y))$. This has an effect on all linear models.
- 