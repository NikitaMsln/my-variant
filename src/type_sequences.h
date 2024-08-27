#pragma once

#include <cstddef>
#include <limits>
#include <type_traits>
#include <utility>

template <typename First, typename... Types>
class variant;

namespace details {

template <typename T>
struct is_variant {
  static constexpr bool value = false;
};

template <typename First, typename... Types>
struct is_variant<variant<First, Types...>> {
  static constexpr bool value = true;
};

template <typename T>
inline constexpr bool is_variant_v = is_variant<std::decay_t<T>>::value;

template <typename T>
concept is_variant_concept = is_variant_v<T>;

// None type
struct nontype {};

// Container for types (for visit)
template <typename... Types>
struct types_container;

template <typename First, typename... Types>
struct types_container<First, Types...> {
  using curr_type = First;
  using tail = types_container<Types...>;
};

template <typename Last>
struct types_container<Last> {
  using curr_type = Last;
};

template <typename T>
struct is_type_container {
  static constexpr bool value = false;
};

template <typename... Types>
struct is_type_container<types_container<Types...>> {
  static constexpr bool value = true;
};

template <typename T>
inline constexpr bool is_type_container_v = is_type_container<std::decay_t<T>>::value;

template <typename T>
concept is_type_container_concept = is_type_container_v<T>;

template <typename T, is_type_container_concept Container>
struct push_front_type;

template <typename T, typename... Types>
struct push_front_type<T, types_container<Types...>> {
  using type = types_container<T, Types...>;
};

template <typename T, is_type_container_concept Container>
using push_front_type_t = push_front_type<T, Container>::type;

template <typename T, is_type_container_concept Container>
struct push_back_type;

template <typename T, typename... Types>
struct push_back_type<T, types_container<Types...>> {
  using type = types_container<Types..., T>;
};

template <typename T, is_type_container_concept Container>
using push_back_type_t = push_back_type<T, Container>::type;

template <is_type_container_concept Container>
struct pop_front_type;

template <typename First, typename... Types>
struct pop_front_type<types_container<First, Types...>> {
  using type = types_container<Types...>;
};

template <is_type_container_concept Container>
using pop_front_type_t = pop_front_type<Container>::type;

template <is_type_container_concept Container>
constexpr auto pop_back_type(Container);

template <typename... Types, typename Last>
constexpr auto pop_back_type(types_container<Types..., Last>) {
  return types_container<Types...>();
}

template <is_type_container_concept Container>
using pop_back_type_t = decltype(pop_back_type<Container>);

template <is_type_container_concept Container, typename... Types>
struct is_same_sequence {
  static constexpr bool value = false;
};

template <typename FirstContainer, typename... ContainerTypes, typename First, typename... Types>
struct is_same_sequence<types_container<FirstContainer, ContainerTypes...>, First, Types...> {
  static constexpr bool value =
      std::is_same_v<FirstContainer, First> && is_same_sequence<types_container<ContainerTypes...>, Types...>::value;
};

template <>
struct is_same_sequence<types_container<>> {
  static constexpr bool value = true;
};

template <is_type_container_concept Container, typename... Types>
inline constexpr bool is_same_sequence_v = is_same_sequence<Container, Types...>::value;

// Count of types
template <typename... Types>
struct count_types;

template <typename First, typename... Types>
struct count_types<First, Types...> {
  static constexpr size_t value = 1 + count_types<Types...>::value;
};

template <>
struct count_types<> {
  static constexpr size_t value = 0;
};

template <typename... Types>
inline constexpr size_t count_types_v = count_types<Types...>::value;

template <typename... Types>
inline constexpr size_t count_types_v<types_container<Types...>> = count_types_v<Types...>;

// Check T in Types
template <typename T, typename... Types>
struct contains_type;

template <typename T, typename U, typename... Types>
struct contains_type<T, U, Types...> {
  static constexpr bool value = std::is_same_v<T, U> || contains_type<T, Types...>::value;
  static constexpr bool is_multy =
      contains_type<T, Types...>::is_multy || (std::is_same_v<T, U> && contains_type<T, Types...>::value);
};

template <typename T>
struct contains_type<T> {
  static constexpr bool value = false;
  static constexpr bool is_multy = false;
};

template <typename T, typename... Types>
inline constexpr bool contains_type_v = contains_type<T, Types...>::value;

template <typename T, typename... Types>
inline constexpr bool contains_type_v<T, types_container<Types...>> = contains_type_v<T, Types...>;

template <typename T, typename... Types>
inline constexpr bool contains_multy_type_t = contains_type<T, Types...>::is_multy;

template <typename T, typename... Types>
inline constexpr bool contains_multy_type_t<T, types_container<Types...>> = contains_multy_type_t<T, Types...>;

// Find index of T in Types
template <size_t I, typename T, typename... Types>
struct find_type_impl;

template <size_t I, typename T, typename U, typename... Types>
struct find_type_impl<I, T, U, Types...> {
  static constexpr size_t value = (std::is_same_v<T, U>) ? I : find_type_impl<I + 1, T, Types...>::value;
};

template <size_t I, typename T>
struct find_type_impl<I, T> {
  static constexpr size_t value = std::numeric_limits<size_t>::max();
};

template <typename T, typename... Types>
inline constexpr size_t find_type = find_type_impl<0, T, Types...>::value;

template <typename T, typename... Types>
inline constexpr size_t find_type<T, types_container<Types...>> = find_type_impl<0, T, Types...>::value;

// Get type by index
template <size_t I, typename... Types>
struct get_type;

template <size_t I, typename T, typename... Types>
struct get_type<I, T, Types...> {
  using type = std::conditional_t<I == 0, T, typename get_type<I - 1, Types...>::type>;
};

template <size_t I, typename... Types>
struct get_type<I, types_container<Types...>> {
  using type = get_type<I, Types...>::type;
};

template <size_t I>
struct get_type<I> {
  using type = nontype;
};

template <size_t I, typename... Types>
using get_type_t = get_type<I, Types...>::type;

// Get single type constructable by T
template <size_t I, typename T, typename... Types>
struct has_single_constructible_by_impl {
  using type = nontype;
  static constexpr bool value = false;
};

template <typename To>
struct is_array_initializable_impl {
  To value[1];
};

template <typename From, typename To>
concept is_array_initializable = requires() {
  { is_array_initializable_impl<To>({std::declval<From>()}) } -> std::same_as<is_array_initializable_impl<To>>;
};

template <size_t I>
struct size_t_container {
  static constexpr size_t value = I;
};

template <size_t I, typename T, typename First, typename... Types>
struct has_single_constructible_by_impl<I, T, First, Types...>
    : public has_single_constructible_by_impl<I + 1, T, Types...> {
public:
  using has_single_constructible_by_impl<I + 1, T, Types...>::convert;

  static constexpr size_t_container<I> convert(std::remove_volatile_t<First>) noexcept
    requires(std::is_convertible_v<T, First> && is_array_initializable<T, First>)
  {
    return size_t_container<I>();
  }

  static constexpr bool has_constructible = (std::is_convertible_v<T, First> && is_array_initializable<T, First>) ||
                                            has_single_constructible_by_impl<I + 1, T, Types...>::has_constructible;

  template <bool Has, typename Non>
  struct type_impl {
    using type = Non;
  };

  template <typename Non>
  struct type_impl<true, Non> {
    using type = std::conditional_t<
        contains_type_v<T, First, Types...>, T,
        get_type_t<decltype(convert(std::forward<T>(std::declval<T>())))::value - I, First, Types...>>;
  };

public:
  using type = has_single_constructible_by_impl<I, T, First, Types...>::type_impl<has_constructible, nontype>::type;
  static constexpr bool value = contains_type_v<type, First, Types...> && !contains_multy_type_t<type, First, Types...>;
};

template <size_t I, typename T>
struct has_single_constructible_by_impl<I, T> {
public:
  constexpr void convert() noexcept {}

  static constexpr bool has_constructible = false;

public:
  using type = nontype;
  static constexpr bool value = false;
};

template <typename T, typename... Types>
inline constexpr bool has_single_constructible_by_v = has_single_constructible_by_impl<0, T, Types...>::value;

template <typename T, typename... Types>
using has_single_constructible_by_t = has_single_constructible_by_impl<0, T, Types...>::type;

} // namespace details
