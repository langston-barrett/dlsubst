# dlsubst

A naïve, bottom-up Datalog interpreter based on explicit substitutions, rather
than relational joins. Just an experiment, not an efficient engine!

## Test

```haskell
cabal run test:dlsubst-test -- --hedgehog-discards 1024
```

Transitive closure:

```sh
echo "a,b\nb,c" | cabal run exe:dlsubst
```
