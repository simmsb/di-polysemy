# Changelog

`di-polysemy` uses [PVP Versioning][1].
The changelog is available [on GitHub][2].

## 0.2.0.0

* `runDiToStderrIO`, `runDiToIOFinal`, `runDiToStderrIOFinal` and the `Reset`
  action have been removed.
* The `Fetch` action has been added.
* `runDiToIOReader`, and `runDiNoop` has been added.
* `runDiToIO` now needs a `Di` value to be passed to it, the library now doesn't
  create the Di value.

## 0.1.4.0

* `runDiToIOFinal` and `runDiToStderrIOFinal` have been added

## 0.1.3.0

* The 'Reset' action has been added

## 0.1.2.0

* 'runDiToIO' now reinterprets in terms of reader
* The effect has been generalised and now has: Log, Flush, and Local

## 0.0.0.0

* Initially created.

[1]: https://pvp.haskell.org
[2]: https://github.com/nitros12/di-polysemy/releases
