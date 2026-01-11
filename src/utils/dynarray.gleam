import gleam/int
import gleam/list

pub type DynArray(t) {
  DynArray(count: Int, capacity: Int, items: List(t))
}

pub fn empty() -> DynArray(t) {
  DynArray(0, 0, [])
}

pub fn from(list: List(t)) -> DynArray(t) {
  let length = list.length(list)
  DynArray(length, next_pow_of_2(length), list)
}

pub fn write(dyn: DynArray(t), item: t) -> DynArray(t) {
  case dyn.capacity < dyn.count + 1 {
    True ->
      DynArray(
        count: dyn.count + 1,
        capacity: grow_capacity(dyn.capacity),
        items: list.append(dyn.items, [item]),
      )
    False ->
      DynArray(
        count: dyn.count + 1,
        capacity: dyn.capacity,
        items: list.append(dyn.items, [item]),
      )
  }
}

pub fn write_head(dyn: DynArray(t), item: t) -> DynArray(t) {
  case dyn.capacity < dyn.count + 1 {
    True ->
      DynArray(
        count: dyn.count + 1,
        capacity: grow_capacity(dyn.capacity),
        items: [item, ..dyn.items],
      )
    False ->
      DynArray(count: dyn.count + 1, capacity: dyn.capacity, items: [
        item,
        ..dyn.items
      ])
  }
}

pub fn get(dyn: DynArray(t), index: Int) -> Result(t, Nil) {
  case index >= 0 && index < dyn.count {
    True ->
      case list.drop(dyn.items, index) {
        [item, ..] -> Ok(item)
        [] -> Error(Nil)
      }
    False -> Error(Nil)
  }
}

pub fn get_head(dyn: DynArray(t)) -> Result(t, Nil) {
  get(dyn, 0)
}

pub fn get_tail(dyn: DynArray(t)) -> Result(t, Nil) {
  get(dyn, dyn.count - 1)
}

fn grow_capacity(n: Int) -> Int {
  case n < 8 {
    True -> 8
    False -> n * 2
  }
}

fn or_and_shift(n: Int, by: Int) -> Int {
  int.bitwise_or(n, int.bitwise_shift_right(n, by))
}

fn next_pow_of_2(n: Int) -> Int {
  let n = n - 1
  let n = or_and_shift(n, 1)
  let n = or_and_shift(n, 2)
  let n = or_and_shift(n, 4)
  let n = or_and_shift(n, 8)
  let n = or_and_shift(n, 16)
  let n = n + 1
  n
}
