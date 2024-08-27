#pragma once

#include "additional.h"
#include "container.h"

// visit realization
namespace details {

// Get returned type
template <typename R, typename Visitor, is_type_container_concept Container>
struct get_returned_type;

template <typename T>
struct propogate_type {
  using type = T;
};

template <typename R, typename Visitor, typename... Types>
struct get_returned_type<R, Visitor, types_container<Types...>> {
  using type = std::conditional_t<
      std::is_convertible_v<typename propogate_type<decltype(std::declval<Visitor>()(std::declval<Types>()...))>::type,
                            R>,
      R, typename propogate_type<decltype(std::declval<Visitor>()(std::declval<Types>()...))>::type>;
};

template <typename Visitor, typename... Types>
struct get_returned_type<nontype, Visitor, types_container<Types...>> {
  using type = propogate_type<decltype(std::declval<Visitor>()(std::declval<Types>()...))>::type;
};

template <typename R, typename R1, typename... Args, typename... Types>
struct get_returned_type<R, R1(Args...), types_container<Types...>> {
  using type = std::conditional_t<std::is_convertible_v<R1, R>, R, R1>;
};

template <typename R1, typename... Args, typename... Types>
struct get_returned_type<nontype, R1(Args...), types_container<Types...>> {
  using type = R1;
};

template <typename R, typename Visitor, is_type_container_concept Container>
using get_returned_type_t = get_returned_type<R, Visitor, Container>::type;

// Check one returned type
template <typename R, typename Visitor, typename... Types>
concept visitor_has_function_impl = requires(Visitor vis, Types... t) {
  { vis(std::forward<Types>(t)...) } -> std::convertible_to<R>;
};

template <typename R, typename Visitor, is_type_container_concept Container>
struct visitor_has_function;

template <typename R, typename Visitor, typename... Types>
struct visitor_has_function<R, Visitor, types_container<Types...>> {
  static constexpr bool value = visitor_has_function_impl<R, Visitor, Types...>;
};

template <typename R, typename Visitor, is_type_container_concept Container>
inline constexpr bool visitor_has_function_v =
    visitor_has_function<get_returned_type_t<R, Visitor, Container>, Visitor, Container>::value;

// Splitter of variadic templates
struct splitter_t {};

inline constexpr splitter_t splitter;

template <typename T>
concept not_splitter = !std::same_as<T, splitter_t>;

// Function table
template <is_type_container_concept Types, is_variant_concept... Variants>
struct function_table;

template <bool IsEnd, is_type_container_concept Types, is_variant_concept CurrVariant, size_t I,
          is_variant_concept... Variants>
  requires(I < std::decay_t<CurrVariant>::types_count)
struct function_row_impl;

template <is_type_container_concept Types, is_variant_concept CurrVariant, size_t I, is_variant_concept... Variants>
using function_row =
    function_row_impl<I + 1 == std::decay_t<CurrVariant>::types_count, Types, CurrVariant, I, Variants...>;

template <is_type_container_concept Types, is_variant_concept CurrVariant, size_t I, is_variant_concept... Variants>
  requires(I < std::decay_t<CurrVariant>::types_count - 1)
struct function_row_impl<false, Types, CurrVariant, I, Variants...> {
  static constexpr function_table<
      push_back_type_t<copy_type_t<variant_alternative_t<I, CurrVariant>, CurrVariant>, Types>, Variants...>
      next_row{};
  static constexpr function_row<Types, CurrVariant, I + 1, Variants...> next_col{};

  template <typename R, typename Visitor>
  using returned_type = decltype(next_row)::template returned_type<R, Visitor>;

  template <typename R, typename Visitor>
  static constexpr bool has_one_returned_type =
      decltype(next_row)::template has_one_returned_type<R, Visitor> &&
      decltype(next_col)::template has_one_returned_type<R, Visitor> &&
      std::is_same_v<typename decltype(next_row)::template returned_type<R, Visitor>,
                     typename decltype(next_col)::template returned_type<R, Visitor>>;

  template <typename R, typename Visitor, not_splitter... T>
    requires(is_same_sequence_v<Types, T...>)
  constexpr returned_type<R, Visitor> evaluate(Visitor&& visitor, T&&... t, splitter_t, CurrVariant&& curr_variant,
                                               Variants&&... variants) const {
    if (curr_variant.index() == I) {
      if constexpr (std::is_same_v<returned_type<R, Visitor>, void>) {
        next_row.template evaluate<R, Visitor, T..., copy_type_t<variant_alternative_t<I, CurrVariant>, CurrVariant>>(
            std::forward<Visitor>(visitor), std::forward<T>(t)..., get<I>(std::forward<CurrVariant>(curr_variant)),
            splitter, std::forward<Variants>(variants)...);
      } else {
        return next_row
            .template evaluate<R, Visitor, T..., copy_type_t<variant_alternative_t<I, CurrVariant>, CurrVariant>>(
                std::forward<Visitor>(visitor), std::forward<T>(t)..., get<I>(std::forward<CurrVariant>(curr_variant)),
                splitter, std::forward<Variants>(variants)...);
      }
    } else {
      if constexpr (std::is_same_v<returned_type<R, Visitor>, void>) {
        next_col.template evaluate<R, Visitor, T...>(std::forward<Visitor>(visitor), std::forward<T>(t)..., splitter,
                                                     std::forward<CurrVariant>(curr_variant),
                                                     std::forward<Variants>(variants)...);
      } else {
        return next_col.template evaluate<R, Visitor, T...>(std::forward<Visitor>(visitor), std::forward<T>(t)...,
                                                            splitter, std::forward<CurrVariant>(curr_variant),
                                                            std::forward<Variants>(variants)...);
      }
    }
  }
};

template <is_type_container_concept Types, is_variant_concept CurrVariant, size_t I, is_variant_concept... Variants>
  requires(I == std::decay_t<CurrVariant>::types_count - 1)
struct function_row_impl<true, Types, CurrVariant, I, Variants...> {
  static constexpr function_table<
      push_back_type_t<copy_type_t<variant_alternative_t<I, CurrVariant>, CurrVariant>, Types>, Variants...>
      next_row{};
  static constexpr nontype next_col{};

  template <typename R, typename Visitor>
  using returned_type = decltype(next_row)::template returned_type<R, Visitor>;

  template <typename R, typename Visitor>
  static constexpr bool has_one_returned_type = decltype(next_row)::template has_one_returned_type<R, Visitor>;

  template <typename R, typename Visitor, not_splitter... T>
    requires(is_same_sequence_v<Types, T...>)
  constexpr returned_type<R, Visitor> evaluate(Visitor&& visitor, T&&... t, splitter_t, CurrVariant&& curr_variant,
                                               Variants&&... variants) const {
    if (curr_variant.index() == I) {
      if constexpr (std::is_same_v<returned_type<R, Visitor>, void>) {
        next_row.template evaluate<R, Visitor, T..., copy_type_t<variant_alternative_t<I, CurrVariant>, CurrVariant>>(
            std::forward<Visitor>(visitor), std::forward<T>(t)..., get<I>(std::forward<CurrVariant>(curr_variant)),
            splitter, std::forward<Variants>(variants)...);
      } else {
        return next_row
            .template evaluate<R, Visitor, T..., copy_type_t<variant_alternative_t<I, CurrVariant>, CurrVariant>>(
                std::forward<Visitor>(visitor), std::forward<T>(t)..., get<I>(std::forward<CurrVariant>(curr_variant)),
                splitter, std::forward<Variants>(variants)...);
      }
    } else {
      throw bad_variant_access(std::string("Invalid variant (") + typeid(curr_variant).name() +
                               ") and index = " + std::to_string(curr_variant.index()) + "in visit");
    }
  }
};

template <is_type_container_concept Types, is_variant_concept First, is_variant_concept... Variants>
struct function_table<Types, First, Variants...> {
private:

public:
  static constexpr function_row<Types, First, 0, Variants...> curr_row{};

public:
  template <typename R, typename Visitor>
  using returned_type = decltype(curr_row)::template returned_type<R, Visitor>;

  template <typename R, typename Visitor>
  static constexpr bool has_one_returned_type = decltype(curr_row)::template has_one_returned_type<R, Visitor>;

  template <typename R, typename Visitor, not_splitter... T>
    requires(is_same_sequence_v<Types, T...>)
  constexpr returned_type<R, Visitor> evaluate(Visitor&& visitor, T&&... t, splitter_t, First&& first,
                                               Variants&&... variants) const {
    if constexpr (std::is_same_v<returned_type<R, Visitor>, void>) {
      curr_row.template evaluate<R, Visitor, T...>(std::forward<Visitor>(visitor), std::forward<T>(t)..., splitter,
                                                   std::forward<First>(first), std::forward<Variants>(variants)...);
    } else {
      return curr_row.template evaluate<R, Visitor, T...>(std::forward<Visitor>(visitor), std::forward<T>(t)...,
                                                          splitter, std::forward<First>(first),
                                                          std::forward<Variants>(variants)...);
    }
  }
};

template <is_type_container_concept Types>
struct function_table<Types> {
  template <typename R, typename Visitor>
  using returned_type = get_returned_type_t<R, Visitor, Types>;

  template <typename R, typename Visitor>
  static constexpr bool has_one_returned_type = visitor_has_function_v<returned_type<R, Visitor>, Visitor, Types>;

  template <typename R, typename Visitor, not_splitter... T>
    requires(is_same_sequence_v<Types, T...>)
  constexpr returned_type<R, Visitor> evaluate(Visitor&& visitor, T&&... t, splitter_t) const {
    if constexpr (std::is_same_v<returned_type<R, Visitor>, void>) {
      std::forward<Visitor>(visitor)(std::forward<T>(t)...);
    } else {
      return std::forward<Visitor>(visitor)(std::forward<T>(t)...);
    }
  }
};

template <is_variant_concept... Variants>
static constexpr function_table<types_container<>, Variants...> function_table_obj;

} // namespace details
