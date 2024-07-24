# Change log for the `synthesizer-llvm` package

## 1.0

* Move from `llvm-dsl` `Parameter` to `Exp` for parameters.
  Remove clumsy distinction between simple and parameterized
  `Signal`s and `Process`es.

## 0.9

* Clean separation between Haskell's `Storable` memory format
  as used in `StorableVector`
  and LLVM's memory format, used for parameters.

* Use of new `llvm-dsl` package.

## 0.8.3

* `Noise`: caused a crash with LLVM-9
  because it called the X86 intrinsic `pmuludq`.
  Now use generic multiplication.

## 0.8.0

* Compiled code is now freed by the garbage collector if it is no longer needed.

* `reverbSimple`: No longer add the original signal.
  Every partial comb filter maintains it anyway.

* In `CausalParameterized.Process`:
  `reverb` -> `reverbSimple`
  `reverbEfficient` -> `reverb`

* added many export lists

* For GHC-7.10 we had to separate `ProcessOf` type functions
  from `synthesizer-core:CausalClass`.
  We adapt to this change here.
