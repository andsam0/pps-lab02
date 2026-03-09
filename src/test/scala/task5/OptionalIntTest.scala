package task5

import org.junit.*
import org.junit.Assert.*
import Optionals.*

class OptionalIntTest:
  @Test def emptyOptionalShouldBeEmpty(): Unit =
    val empty = OptionalInt.Empty()
    assertTrue(OptionalInt.isEmpty(empty))

  @Test def nonEmptyOptionalShouldNotBeEmpty(): Unit =
    val nonEmpty = OptionalInt.Just(0)
    assertFalse(OptionalInt.isEmpty(nonEmpty))

  @Test def orElseShouldReturnDefaultWhenEmpty(): Unit =
    val nonEmpty = OptionalInt.Just(0)
    assertEquals(0, OptionalInt.orElse(nonEmpty, 1))

  @Test def orElseShouldReturnValueWhenNonEmpty(): Unit =
    val empty = OptionalInt.Empty()
    assertEquals(1, OptionalInt.orElse(empty, 1))

  /** Task 5: do test for map **/
  @Test def mapShouldReturnValue(): Unit =
    val nonEmpty = OptionalInt.Just(5)
    assertEquals(OptionalInt.Just(6), OptionalInt.mapInt(nonEmpty)(_ + 1))

  @Test def mapShouldReturnEmpty(): Unit =
    val empty = OptionalInt.Empty()
    assertEquals(OptionalInt.Empty(), OptionalInt.mapInt(empty)(_ + 1))

  @Test def filterShouldPass(): Unit =
    val okOpt = OptionalInt.Just(5)
    assertEquals(OptionalInt.Just(5), OptionalInt.filter(okOpt)(_ > 2))

  @Test def filterShouldNotPassWhenNonEmpty(): Unit =
    val notOkOpt = OptionalInt.Just(5)
    assertEquals(OptionalInt.Empty(), OptionalInt.filter(notOkOpt)(_ > 8))

  @Test def filterShouldNotPassWhenEmpty(): Unit =
    val empty = OptionalInt.Empty()
    assertEquals(OptionalInt.Empty(), OptionalInt.filter(empty)(_ > 2))