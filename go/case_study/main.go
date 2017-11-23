package main

import (
  "fmt"
)

type Person struct {
  GetName func() string
  GetSurname func() string
  SetName func(string)
  SetSurname func(string)
  GetFullname func() string
}

type Point struct {
  x int
  y int
}

func main() {

  get1, set1 := make_cnt(2)
  fmt.Println("get1:", get1())
  fmt.Println("set1", set1(5))
  fmt.Println("get1 again:", get1())

  get2, set2 := make_cnt(0)
  fmt.Println("get2:", get2())
  fmt.Println("set2", set2(3))
  fmt.Println("get2 again:", get2())

  fmt.Println("get1:", get1())
  fmt.Println("get2:", get2())

  items := []int{7, 6, 8, 1, 4, 2}
  fmt.Println("items:", items, "len/cap", len(items), cap(items))
  slice := items[:0]
  fmt.Println("slice:", slice, "len/cap", len(slice), cap(slice))
  slice = items[:4]
  fmt.Println("slice:", slice, "len/cap", len(slice), cap(slice))
  slice = items[1:3]
  fmt.Println("slice:", slice, "len/cap", len(slice), cap(slice))

  fmt.Println("items:", items)
  fmt.Println("items sorted:", bubble_sort(items))
  fmt.Println("items after sorting:", items)

  person1 := make_person("chris", "bartolo")
  fmt.Println("person1:", person1.GetFullname())

  person2 := make_person("chris", "bartolo2")
  fmt.Println("person2:", person2.GetFullname())

  point := Point{x:2, y:2}
  fmt.Println("point", point)

  point.x = 8
  fmt.Println("point", point.mult())
}

func (p Point) mult() int {
  return p.x * p.y
}

func make_person(name, surname string) Person {
  _name, _surname := name, surname

  return Person{
    GetName: func() string {
      return _name
    },
    GetSurname: func() string {
      return _surname
    },
    SetName: func(name string) {
      _name = name
    },
    SetSurname: func(surname string) {
      _surname = surname
    },
    GetFullname: func() string {
      return _name + " " + _surname
    }}
}

func make_cnt(init int) (func() int, func(int) int) {
  cnt := init // Lives in the closure.

  return func() int { // Gets the value of cnt.
    return cnt
  },
  func(val int) int { // Sets the value of cnt.
    tmp := cnt
    cnt = val
    return tmp
  }
}

func bubble_sort(items []int) []int {
  len := len(items)
  swapped := true

  for swapped {
    swapped = false
    for i := 1; i < len; i++ {
      if items[i - 1] > items[i] {

        // Using tmp.
        // tmp := items[i - 1]
        // items[i - 1] = items[i]
        // items[i] = tmp

        // Direct way
        items[i - 1], items[i] = items[i], items[i - 1]
        swapped = true
      }
    }
  }

  return items
}
