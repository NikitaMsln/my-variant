#pragma once

#include "container.h"

#include <string>

namespace details {
// Get signature of type like variant
template <typename T, typename U>
struct copy_reference_type {
  using type = T;
};

template <typename T, typename U>
struct copy_reference_type<T, U&> {
  using type = T&;
};

template <typename T, typename U>
struct copy_reference_type<T, U&&> {
  using type = T&&;
};

template <typename T, typename U>
struct copy_const_declartion {
  using type = T;
};

template <typename T, typename U>
struct copy_const_declartion<T, const U> {
  using type = const T;
};

template <typename T, typename U>
using copy_type_t =
    copy_const_declartion<typename copy_reference_type<typename copy_const_declartion<T, U>::type, U>::type, U>::type;
} // namespace details

template <typename First, typename... Types>
class variant;

class bad_variant_access : public std::exception {
public:
  bad_variant_access();
  explicit bad_variant_access(std::string message);
  explicit bad_variant_access(const char* message);
  const char* what() const noexcept override;

private:
  std::string msg;
};

template <typename T>
struct in_place_type_t {
  constexpr in_place_type_t() noexcept = default;
};

template <typename T>
inline constexpr in_place_type_t<T> in_place_type;

template <size_t I>
struct in_place_index_t {
  constexpr in_place_index_t() noexcept = default;
};

template <size_t I>
inline constexpr in_place_index_t<I> in_place_index;

template <details::is_variant_concept T>
struct variant_size {
  static constexpr size_t value = T::types_count;

  constexpr variant_size() noexcept = default;

  constexpr operator size_t() const noexcept {
    return value;
  }

  constexpr size_t operator()() const noexcept {
    return value;
  }
};

template <typename T>
inline constexpr size_t variant_size_v = variant_size<std::decay_t<T>>::value;

template <size_t I, details::is_variant_concept V>
struct variant_alternative;

template <size_t I, typename... Types>
struct variant_alternative<I, variant<Types...>> {
  using type = details::get_type_t<I, Types...>;
};

template <size_t I, typename... Types>
struct variant_alternative<I, const variant<Types...>> {
  using type = const details::get_type_t<I, Types...>;
};

template <size_t I, typename V>
using variant_alternative_t = variant_alternative<I, std::remove_reference_t<V>>::type;

inline constexpr size_t variant_npos = std::numeric_limits<size_t>::max();
