@val external setTimeout: (unit => unit, int) => 'a = "setTimeout"

type iterator<'item> = {next: unit => Promise.t<option<'item>>}

let wait = ms => Promise.make((resolve, _) => setTimeout(() => resolve(. true), ms)->ignore)

let interval = ms => {
  let i = ref(0)
  let waitPromise = ref(wait(0))
  {
    next: () => {
      waitPromise.contents->Promise.thenResolve(_ => {
        let result = i.contents
        i.contents = i.contents + 1
        waitPromise.contents = wait(ms)
        Some(result)
      })
    },
  }
}

let sequence = () => interval(0)

let fromArray = a => {
  let i = ref(0)
  {
    next: () => {
      let result = a->Belt.Array.get(i.contents)
      i.contents = i.contents + 1
      Js.Promise.resolve(result)
    },
  }
}

let empty = () => fromArray([])

let mapPromise = (iter, fn) =>
  iter.next()->Promise.then(opt =>
    opt->Belt.Option.mapWithDefault(Promise.resolve(None), val => fn(val))
  )

let mapOpt = (iter, fn) => {
  next: () => iter.next()->Promise.thenResolve(Belt.Option.flatMap(_, val => fn(val))),
}
let mapValue = (iter, fn) => iter->mapOpt(val => Some(fn(val)))
let map = mapValue

let take = (iter, n) => {
  let i = ref(0)
  mapOpt(iter, val =>
    if i.contents < n {
      i.contents = i.contents + 1
      Some(val)
    } else {
      None
    }
  )
}

let filter = (iter, predicate) => {
  next: () => {
    Promise.make((resolve, _) => {
      let rec loop = () => {
        iter.next()
        ->Promise.thenResolve(opt => {
          switch opt {
          | Some(val) if predicate(val) => resolve(. Some(val))
          | Some(_) => loop()
          | None => resolve(. None)
          }
          ()
        })
        ->ignore
      }
      loop()
    })
  },
}

let delay = (iter, ms) => {
  next: () => iter.next()->Promise.then(val => wait(ms)->Promise.thenResolve(_ => val)),
}

let reduce = (iter, fn, initial) =>
  Promise.make((resolve, _) => {
    let current = ref(initial)
    let rec loop = () => {
      iter.next()->Promise.thenResolve(opt =>
        switch opt {
        | Some(val) => {
            current.contents = fn(current.contents, val)
            loop()->ignore
          }
        | None => resolve(. current.contents)
        }
      )
    }
    loop()->ignore
  })

let find = (iter, predicate) => {
  Promise.make((resolve, _) => {
    let rec findAux = () =>
      iter.next()
      ->Promise.thenResolve(
        Belt.Option.map(_, val => {
          if predicate(val) {
            resolve(. Some(val))
          } else {
            findAux()
          }
        }),
      )
      ->ignore
    findAux()
  })
}

let enumerate = iter => {
  let i = ref(0)
  iter->map(v => {
    let result = (i.contents, v)
    i.contents = i.contents + 1
    result
  })
}

let collect = iter => iter->reduce((arr, val) => {
    arr->Js.Array2.push(val)->ignore
    arr
  }, [])

let scan = (iter, fn, initial) => {
  let state = ref(initial)
  {
    next: () =>
      iter.next()->Promise.then(opt =>
        switch opt {
        | None => Promise.resolve(None)
        | Some(val) => fn(state, val)->Promise.thenResolve(res => Some(res))
        }
      ),
  }
}

let flatten = iter => {
  let current = ref(iter.next())
  {
    next: () => {
      let rec loop = sIterOptProm => {
        sIterOptProm->Promise.then(sIterOpt =>
          switch sIterOpt {
          | None => Promise.resolve(None)
          | Some(sIter) =>
            sIter.next()->Promise.then(opt =>
              switch opt {
              | Some(val) => Promise.resolve(Some(val))
              | None => {
                  current.contents = iter.next()
                  loop(current.contents)
                }
              }
            )
          }
        )
      }
      loop(current.contents)
    },
  }
}

let forEach = (iter, fn) => {
  let rec loop = () => {
    iter.next()->Promise.then(
      Belt.Option.mapWithDefault(_, Promise.resolve(), val => fn(val)->Promise.then(() => loop())),
    )
  }
  loop()
}

let mergeArr = (iterators, ~maxConcurrency=iterators->Js.Array2.length, ()) => {
  let promises =
    iterators->Js.Array2.mapi((iter, idx) => Some(
      iter.next()->Promise.thenResolve(val => (idx, val)),
    ))
  {
    next: () => {
      let rec loop = () => {
        Promise.race(
          promises->Belt.Array.keepMap(v => v)->Belt.Array.slice(~offset=0, ~len=maxConcurrency),
        )->Promise.then(((idx, opt)) =>
          switch opt {
          | Some(val) => {
              promises
              ->Belt.Array.set(
                idx,
                iterators[idx].next()->Promise.thenResolve(val => (idx, val))->Some,
              )
              ->ignore
              Promise.resolve(Some(val))
            }
          | None => {
              promises->Belt.Array.set(idx, None)->ignore
              loop()
            }
          }
        )
      }
      loop()
    },
  }
}

let flatMap = (iter, fn) => iter->map(fn)->flatten

let merge = (iterA, iterB) => mergeArr([iterA, iterB])
