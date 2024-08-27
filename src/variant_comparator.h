#pragma once
#include "type_sequences.h"

namespace details {
template <size_t I, is_variant_concept Variant>
constexpr bool equal(const Variant& lhs, const Variant& rhs);

template <size_t I, is_variant_concept Variant>
  requires(I < Variant::types_count)
constexpr bool equal(const Variant& lhs, const Variant& rhs) {
  if (I == lhs.index()) {
    return get<I>(lhs) == get<I>(rhs);
  } else {
    return equal<I + 1>(lhs, rhs);
  }
}

template <size_t I, is_variant_concept Variant>
constexpr bool equal(const Variant&, const Variant&) {
  return false;
}

template <size_t I, is_variant_concept Variant>
constexpr bool not_equal(const Variant&, const Variant&);

template <size_t I, is_variant_concept Variant>
  requires(I < Variant::types_count)
constexpr bool not_equal(const Variant& lhs, const Variant& rhs) {
  if (I == lhs.index()) {
    return get<I>(lhs) != get<I>(rhs);
  } else {
    return not_equal<I + 1>(lhs, rhs);
  }
}

template <size_t I, is_variant_concept Variant>
constexpr bool not_equal(const Variant&, const Variant&) {
  return false;
}

template <size_t I, is_variant_concept Variant>
constexpr bool less(const Variant&, const Variant&);

template <size_t I, is_variant_concept Variant>
  requires(I < Variant::types_count)
constexpr bool less(const Variant& lhs, const Variant& rhs) {
  if (I == lhs.index()) {
    return get<I>(lhs) < get<I>(rhs);
  } else {
    return less<I + 1>(lhs, rhs);
  }
}

template <size_t I, is_variant_concept Variant>
constexpr bool less(const Variant&, const Variant&) {
  return false;
}

template <size_t I, is_variant_concept Variant>
constexpr bool greater(const Variant&, const Variant&);

template <size_t I, is_variant_concept Variant>
  requires(I < Variant::types_count)
constexpr bool greater(const Variant& lhs, const Variant& rhs) {
  if (I == lhs.index()) {
    return get<I>(lhs) > get<I>(rhs);
  } else {
    return greater<I + 1>(lhs, rhs);
  }
}

template <size_t I, is_variant_concept Variant>
constexpr bool greater(const Variant&, const Variant&) {
  return false;
}

template <size_t I, is_variant_concept Variant>
constexpr bool not_less(const Variant& lhs, const Variant& rhs);

template <size_t I, is_variant_concept Variant>
  requires(I < Variant::types_count)
constexpr bool not_less(const Variant& lhs, const Variant& rhs) {
  if (I == lhs.index()) {
    return get<I>(lhs) >= get<I>(rhs);
  } else {
    return not_less<I + 1>(lhs, rhs);
  }
}

template <size_t I, is_variant_concept Variant>
constexpr bool not_less(const Variant&, const Variant&) {
  return false;
}

template <size_t I, is_variant_concept Variant>
constexpr bool not_greater(const Variant& lhs, const Variant& rhs);

template <size_t I, is_variant_concept Variant>
  requires(I < Variant::types_count)
constexpr bool not_greater(const Variant& lhs, const Variant& rhs) {
  if (I == lhs.index()) {
    return get<I>(lhs) <= get<I>(rhs);
  } else {
    return not_greater<I + 1>(lhs, rhs);
  }
}

template <size_t I, is_variant_concept Variant>
constexpr bool not_greater(const Variant&, const Variant&) {
  return false;
}

template <typename R, size_t I, is_variant_concept Variant>
constexpr R spaceship(const Variant& lhs, const Variant& rhs);

template <typename R, size_t I, is_variant_concept Variant>
  requires(I < Variant::types_count)
constexpr R spaceship(const Variant& lhs, const Variant& rhs) {
  if (I == lhs.index()) {
    return get<I>(lhs) <=> get<I>(rhs);
  } else {
    return spaceship<R, I + 1>(lhs, rhs);
  }
}

template <typename R, size_t I, is_variant_concept Variant>
constexpr R spaceship(const Variant&, const Variant&) {
  return std::strong_ordering::equivalent;
}
} // namespace details
