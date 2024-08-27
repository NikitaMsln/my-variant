#pragma once

#include "additional.h"
#include "container.h"
#include "function_table.h"
#include "variant_comparator.h"

#include <limits>
#include <memory>
#include <string>
#include <type_traits>
#include <utility>

template <typename First, typename... Types>
class variant {
public:
  using types_list = details::types_container<First, Types...>;
  using container = details::container<First, Types...>;
  static constexpr size_t types_count = details::count_types_v<types_list>;

  constexpr variant() noexcept(std::is_nothrow_default_constructible_v<First>)
    requires(std::is_default_constructible_v<First>)
  = default;

  constexpr ~variant() noexcept
    requires(container::is_trivially_destructible)
  = default;

  constexpr ~variant() noexcept
    requires(!container::is_trivially_destructible)
  {
    clear();
  }

  constexpr variant(const variant& other)
    requires(container::is_copy_constructable && container::is_trivially_copy_constructible)
  = default;

  constexpr variant(const variant& other)
    requires(container::is_copy_constructable && !container::is_trivially_copy_constructible)
      : index_(other.index_),
        data_(0) {
    data_.copy_construct(index_, other.data_);
  }

  constexpr variant(variant&& other) noexcept(container::is_nothrow_move_constructible)
    requires(container::is_move_constructable && container::is_trivially_move_constructible)
  = default;

  constexpr variant(variant&& other) noexcept(container::is_nothrow_move_constructible)
    requires(container::is_move_constructable && !container::is_trivially_move_constructible)
      : index_(other.index_),
        data_(0) {
    data_.move_construct(index_, std::move(other.data_));
  }

  template <typename T>
    requires(details::has_single_constructible_by_v<T, First, Types...>)
  constexpr variant(T&& t) noexcept(
      std::is_nothrow_constructible_v<details::has_single_constructible_by_t<T, First, Types...>, T>)
      : variant(in_place_type<details::has_single_constructible_by_t<T, First, Types...>>, std::forward<T>(t)) {}

  template <typename T>
    requires(details::has_single_constructible_by_v<T, First, Types...>)
  constexpr variant& operator=(T&& t) noexcept(
      std::is_nothrow_constructible_v<details::has_single_constructible_by_t<T, First, Types...>, T>) {
    if constexpr (std::is_assignable_v<details::has_single_constructible_by_t<T, First, Types...>, T>) {
      if (index() == details::find_type<details::has_single_constructible_by_t<T, First, Types...>, First, Types...>) {
        get<details::has_single_constructible_by_t<T, First, Types...>>(*this) = std::forward<T>(t);
      } else {
        emplace<details::has_single_constructible_by_t<T, First, Types...>>(std::forward<T>(t));
      }
    } else {
      emplace<details::has_single_constructible_by_t<T, First, Types...>>(std::forward<T>(t));
    }
    return *this;
  }

  template <typename T, typename... Args>
    requires(details::contains_type_v<T, First, Types...> && !details::contains_multy_type_t<T, First, Types...> &&
             std::is_constructible_v<T, Args...>)
  constexpr variant(in_place_type_t<T>, Args&&... args)
      : variant(in_place_index<details::find_type<T, First, Types...>>, std::forward<Args>(args)...) {}

  constexpr variant& operator=(const variant& other)
    requires(container::is_trivially_copy_assignable && container::is_trivially_copy_constructible &&
             container::is_trivially_destructible && container::is_copy_constructable && container::is_copy_assignable)
  = default;

  constexpr variant& operator=(const variant& other)
    requires(!(container::is_trivially_copy_assignable && container::is_trivially_copy_constructible &&
               container::is_trivially_destructible) &&
             container::is_copy_constructable && container::is_copy_assignable)
  {
    if constexpr (container::is_nothrow_move_constructible && container::is_nothrow_move_assignable) {
      container tmp(0);
      tmp.move_construct(index_, std::move(data_));
      try {
        data_.copy(index_, other.index_, other.data_);
      } catch (...) {
        data_.move_construct(index_, std::move(tmp));
        tmp.clear(index_);
        throw;
      }
      tmp.clear(index_);
    } else {
      try {
        data_.copy(index_, other.index_, other.data_);
      } catch (...) {
        index_ = variant_npos;
        throw;
      }
    }
    index_ = other.index_;
    return *this;
  }

  constexpr variant& operator=(variant&& other) noexcept(container::is_nothrow_move_constructible &&
                                                         container::is_nothrow_move_assignable)
    requires(container::is_trivially_move_assignable && container::is_trivially_move_constructible &&
             container::is_trivially_destructible && container::is_move_constructable && container::is_move_assignable)
  = default;

  constexpr variant& operator=(variant&& other) noexcept(container::is_nothrow_move_constructible &&
                                                         container::is_nothrow_move_assignable)
    requires(!(container::is_trivially_move_assignable && container::is_trivially_move_constructible &&
               container::is_trivially_destructible) &&
             container::is_move_constructable && container::is_move_assignable)
  {
    try {
      data_.move(index_, other.index_, std::move(other.data_));
    } catch (...) {
      index_ = variant_npos;
      if constexpr (!(container::is_nothrow_move_constructible && container::is_nothrow_move_assignable)) {
        throw;
      }
    }
    index_ = other.index_;
    return *this;
  }

  template <size_t I, typename... Args>
    requires(std::is_constructible_v<variant_alternative_t<I, variant>, Args...> && I < variant_size_v<variant>)
  constexpr variant(in_place_index_t<I>, Args&&... args) : index_(I),
                                                           data_(0) {
    variant_alternative_t<I, variant>* pos = container::template get<I>(data_);
    if (pos) {
      std::construct_at(container::template get<I>(data_), std::forward<Args>(args)...);
    }
  }

  constexpr size_t index() const noexcept {
    return index_;
  }

  template <size_t I, typename... Args>
    requires(std::is_constructible_v<variant_alternative_t<I, variant>, Args...> && I < variant_size_v<variant>)
  constexpr variant_alternative_t<I, variant>& emplace(Args&&... args) {
    variant_alternative_t<I, variant>* result;
    if constexpr (std::is_nothrow_move_constructible_v<variant> && std::is_nothrow_move_assignable_v<variant>) {
      variant tmp(std::move(*this));
      clear();
      try {
        result = std::construct_at(container::template get<I>(data_), std::forward<Args>(args)...);
      } catch (...) {
        *this = std::move(tmp);
        throw;
      }
    } else {
      clear();
      result = std::construct_at(container::template get<I>(data_), std::forward<Args>(args)...);
    }
    index_ = I;
    return *result;
  }

  template <typename T, typename... Args>
    requires(details::contains_type_v<T, First, Types...> && !details::contains_multy_type_t<T, First, Types...> &&
             std::is_constructible_v<T, Args...>)
  constexpr T& emplace(Args&&... args) {
    return emplace<details::find_type<T, First, Types...>>(std::forward<Args>(args)...);
  }

  constexpr bool valueless_by_exception() const noexcept {
    return index_ == variant_npos;
  }

  template <typename T>
  friend constexpr bool holds_alternative(const variant&) {
    return details::contains_type_v<T, First, Types...>;
  }

  template <size_t I>
    requires(I < variant_size_v<variant>)
  friend constexpr variant_alternative_t<I, variant>& get(variant& other) {
    if (other.valueless_by_exception()) {
      throw bad_variant_access("variant from get(variant&) is valueless");
    }
    if (other.index() != I) {
      throw bad_variant_access("actual variant index is " + std::to_string(other.index()) + " not " +
                               std::to_string(I));
    }
    return *container::template get<I>(other.data_);
  }

  template <size_t I>
    requires(I < variant_size_v<variant>)
  friend constexpr variant_alternative_t<I, variant>&& get(variant&& other) {
    if (other.valueless_by_exception()) {
      throw bad_variant_access("variant from get(variant&&) is valueless");
    }
    if (other.index() != I) {
      throw bad_variant_access("actual variant index is " + std::to_string(other.index()) + " not " +
                               std::to_string(I));
    }
    return std::move(*container::template get<I>(other.data_));
  }

  template <size_t I>
    requires(I < variant_size_v<variant>)
  friend constexpr const variant_alternative_t<I, variant>& get(const variant& other) {
    if (other.valueless_by_exception()) {
      throw bad_variant_access("variant from get(const variant&) is valueless");
    }
    if (other.index() != I) {
      throw bad_variant_access("actual variant index is " + std::to_string(other.index()) + " not " +
                               std::to_string(I));
    }
    return *container::template get<I>(other.data_);
  }

  template <size_t I>
    requires(I < variant_size_v<variant>)
  friend constexpr const variant_alternative_t<I, variant>&& get(const variant&& other) {
    if (other.valueless_by_exception()) {
      throw bad_variant_access("variant from get(const variant&&) is valueless");
    }
    if (other.index() != I) {
      throw bad_variant_access("actual variant index is " + std::to_string(other.index()) + " not " +
                               std::to_string(I));
    }
    return std::move(*container::template get<I>(other.data_));
  }

  template <typename T>
    requires(details::contains_type_v<T, First, Types...> && !details::contains_multy_type_t<T, First, Types...>)
  friend constexpr T& get(variant& other) {
    return get<details::find_type<T, First, Types...>>(other);
  }

  template <typename T>
    requires(details::contains_type_v<T, First, Types...> && !details::contains_multy_type_t<T, First, Types...>)
  friend constexpr T&& get(variant&& other) {
    return std::move(get<details::find_type<T, First, Types...>>(std::move(other)));
  }

  template <typename T>
    requires(details::contains_type_v<T, First, Types...> && !details::contains_multy_type_t<T, First, Types...>)
  friend constexpr const T& get(const variant& other) {
    return get<details::find_type<T, First, Types...>>(other);
  }

  template <typename T>
    requires(details::contains_type_v<T, First, Types...> && !details::contains_multy_type_t<T, First, Types...>)
  friend constexpr const T&& get(const variant&& other) {
    return std::move(get<details::find_type<T, First, Types...>>(std::move(other)));
  }

  template <size_t I>
    requires(I < variant_size_v<variant>)
  friend constexpr std::add_pointer_t<variant_alternative_t<I, variant>> get_if(variant* other) noexcept {
    if (other->index_ != I) {
      return nullptr;
    } else {
      return container::template get<I>(other->data_);
    }
  }

  template <size_t I>
    requires(I < variant_size_v<variant>)
  friend constexpr std::add_pointer_t<const variant_alternative_t<I, variant>> get_if(const variant* other) noexcept {
    if (other->index_ != I) {
      return nullptr;
    } else {
      return container::template get<I>(other->data_);
    }
  }

  template <typename T>
    requires(details::contains_type_v<T, First, Types...> && !details::contains_multy_type_t<T, First, Types...>)
  friend constexpr std::add_pointer_t<T> get_if(variant* other) noexcept {
    return get_if<details::find_type<T, First, Types...>>(other);
  }

  template <typename T>
    requires(details::contains_type_v<T, First, Types...> && !details::contains_multy_type_t<T, First, Types...>)
  friend constexpr std::add_pointer_t<const T> get_if(const variant* other) noexcept {
    return get_if<details::find_type<T, First, Types...>>(other);
  }

  constexpr void swap(variant& other) {
    if (index() == other.index()) {
      data_.swap_c(index(), other.data_);
    } else {
      variant tmp(std::move(other));
      other = std::move(*this);
      *this = std::move(tmp);
    }
  }

  /*template <typename R = details::nontype, typename Visitor, details::is_variant_concept... Variants>
    requires(decltype(details::function_table_obj<Variants...>)::template has_one_returned_type<R, Visitor> &&
             !std::is_same_v<typename decltype(details::function_table_obj<Variants...>)::template returned_type<R,
  Visitor>, void>) constexpr typename decltype(details::function_table_obj<Variants...>)::template returned_type<R,
  Visitor> visit( Visitor&& vis, Variants&&... vars);

  template <typename R, typename Visitor, details::is_variant_concept... Variants>
    requires(decltype(details::function_table_obj<Variants...>)::template has_one_returned_type<void, Visitor> &&
             std::is_same_v<typename decltype(details::function_table_obj<Variants...>)::template returned_type<void,
  Visitor>, void> && std::is_same_v<R, void>) friend constexpr void visit(Visitor&& vis, Variants&&... vars);*/

private:
  size_t index_ = 0;
  container data_;

  constexpr void clear() noexcept {
    if (index_ != variant_npos) {
      data_.clear(index_);
      index_ = variant_npos;
    }
  }
};

template <typename R = details::nontype, typename Visitor, details::is_variant_concept... Variants>
  requires(
      decltype(details::function_table_obj<Variants...>)::template has_one_returned_type<R, Visitor> &&
      !std::is_same_v<typename decltype(details::function_table_obj<Variants...>)::template returned_type<R, Visitor>,
                      void>)
constexpr typename decltype(details::function_table_obj<Variants...>)::template returned_type<R, Visitor> visit(
    Visitor&& vis, Variants&&... vars) {
  return details::function_table_obj<Variants...>.template evaluate<R>(std::forward<Visitor>(vis), details::splitter,
                                                                       std::forward<Variants>(vars)...);
}

template <typename R = void, typename Visitor, details::is_variant_concept... Variants>
  requires(
      decltype(details::function_table_obj<Variants...>)::template has_one_returned_type<void, Visitor> &&
      std::is_same_v<typename decltype(details::function_table_obj<Variants...>)::template returned_type<void, Visitor>,
                     void> &&
      std::is_same_v<R, void>)
constexpr void visit(Visitor&& vis, Variants&&... vars) {
  details::function_table_obj<Variants...>.template evaluate<void>(std::forward<Visitor>(vis), details::splitter,
                                                                   std::forward<Variants>(vars)...);
}

template <typename... Types>
constexpr bool operator==(const variant<Types...>& lhs, const variant<Types...>& rhs) {
  if (lhs.index() != rhs.index()) {
    return false;
  } else if (lhs.valueless_by_exception()) {
    return true;
  } else {
    return details::equal<0>(lhs, rhs);
  }
}

template <typename... Types>
constexpr bool operator!=(const variant<Types...>& lhs, const variant<Types...>& rhs) {
  if (lhs.index() != rhs.index()) {
    return true;
  } else if (lhs.valueless_by_exception()) {
    return false;
  } else {
    return details::not_equal<0>(lhs, rhs);
  }
}

template <typename... Types>
constexpr bool operator<(const variant<Types...>& lhs, const variant<Types...>& rhs) {
  if (rhs.valueless_by_exception()) {
    return false;
  } else if (lhs.valueless_by_exception()) {
    return true;
  } else if (lhs.index() < rhs.index()) {
    return true;
  } else if (lhs.index() > rhs.index()) {
    return false;
  } else {
    return details::less<0>(lhs, rhs);
  }
}

template <typename... Types>
constexpr bool operator>(const variant<Types...>& lhs, const variant<Types...>& rhs) {
  if (lhs.valueless_by_exception()) {
    return false;
  } else if (rhs.valueless_by_exception()) {
    return true;
  } else if (lhs.index() > rhs.index()) {
    return true;
  } else if (lhs.index() < rhs.index()) {
    return false;
  } else {
    return details::greater<0>(lhs, rhs);
  }
}

template <typename... Types>
constexpr bool operator<=(const variant<Types...>& lhs, const variant<Types...>& rhs) {
  if (lhs.valueless_by_exception()) {
    return true;
  } else if (rhs.valueless_by_exception()) {
    return false;
  } else if (lhs.index() > rhs.index()) {
    return false;
  } else if (lhs.index() < rhs.index()) {
    return true;
  } else {
    return details::not_greater<0>(lhs, rhs);
  }
}

template <typename... Types>
constexpr bool operator>=(const variant<Types...>& lhs, const variant<Types...>& rhs) {
  if (rhs.valueless_by_exception()) {
    return true;
  } else if (lhs.valueless_by_exception()) {
    return false;
  } else if (lhs.index() > rhs.index()) {
    return true;
  } else if (lhs.index() < rhs.index()) {
    return false;
  } else {
    return details::not_less<0>(lhs, rhs);
  }
}

template <typename... Types>
constexpr std::common_comparison_category_t<std::compare_three_way_result_t<Types>...> operator<=>(
    const variant<Types...>& lhs, const variant<Types...>& rhs) {
  if (lhs.valueless_by_exception() && rhs.valueless_by_exception()) {
    return std::strong_ordering::equal;
  } else if (lhs.valueless_by_exception()) {
    return std::strong_ordering::less;
  } else if (rhs.valueless_by_exception()) {
    return std::strong_ordering::greater;
  } else if (lhs.index() != rhs.index()) {
    return lhs.index() <=> rhs.index();
  } else {
    return details::spaceship<std::common_comparison_category_t<std::compare_three_way_result_t<Types>...>, 0>(lhs,
                                                                                                               rhs);
  }
}
