package task5

import org.junit.*
import org.junit.Assert.*
import Optionals.*

class OptionalIntTest:
  final val empty = OptionalInt.Empty()
  final val nonEmpty = OptionalInt.Just(0)

  @Test def emptyOptionalShouldBeEmpty(): Unit =
    assertTrue(OptionalInt.isEmpty(empty))

  @Test def nonEmptyOptionalShouldNotBeEmpty(): Unit =
    assertFalse(OptionalInt.isEmpty(nonEmpty))

  @Test def orElseShouldReturnDefaultWhenEmpty(): Unit =
    assertEquals(0, OptionalInt.orElse(nonEmpty, 1))

  @Test def orElseShouldReturnValueWhenNonEmpty(): Unit =
    assertEquals(1, OptionalInt.orElse(empty, 1))

  /** Task 5: do test for map **/
  @Test def mapShouldReturnValue(): Unit =
    assertEquals(OptionalInt.Just(1), OptionalInt.mapInt(nonEmpty)(_ + 1))

  @Test def mapShouldReturnEmpty(): Unit =
    assertEquals(OptionalInt.Empty(), OptionalInt.mapInt(empty)(_ + 1))

  @Test def filterShouldPass(): Unit =
    val okOpt = OptionalInt.Just(5)
    assertEquals(OptionalInt.Just(5), OptionalInt.filter(okOpt)(_ > 2))

  @Test def filterShouldNotPassWhenNonEmpty(): Unit =
    val notOkOpt = OptionalInt.Just(5)
    assertEquals(OptionalInt.Empty(), OptionalInt.filter(notOkOpt)(_ > 8))

  @Test def filterShouldNotPassWhenEmpty(): Unit =
    assertEquals(OptionalInt.Empty(), OptionalInt.filter(empty)(_ > 2))