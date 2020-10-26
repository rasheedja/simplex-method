# Usage of Simplex.hs

## Types

```Haskell
simplex :: [Constraint] -> Sum [Nat] (Mapping Nat Rat);

data Constraint = LT Linear_poly Rat | GT Linear_poly Rat | LEQ Linear_poly Rat
  | GEQ Linear_poly Rat | EQ Linear_poly Rat | LTPP Linear_poly Linear_poly
  | GTPP Linear_poly Linear_poly | LEQPP Linear_poly Linear_poly
  | GEQPP Linear_poly Linear_poly | EQPP Linear_poly Linear_poly;

newtype Linear_poly = LinearPoly (Fmap Nat Rat);

newtype Fmap a b = Fmap_of_list [(a, b)];

newtype Nat = Nat Integer;

newtype Rat = Frct (Int, Int);

newtype Int = Int_of_integer Integer; -- Integer is from Prelude
```

## Examples

```
simplex [LT (LinearPoly (Fmap_of_list [(Nat 1, Frct (Int_of_integer 1, Int_of_integer 2))])) (Frct (Int_of_integer 1, Int_of_integer 1))]
```