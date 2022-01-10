# Rescript Async Iterators

```rescript
type t<'a> = { next: unit => Promise.t<option<'a>> }
```

## Example

```rescript
open AsyncIterator

interval(1000)
->map(i => i + 1)
->flatMap(i => interval(100)->take(i))
->take(100)
->forEach(i => console.log(i)->Promise.resolve )
```
