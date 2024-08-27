#pragma once

#include "type_sequences.h"

#include <cstddef>
#include <optional>
#include <type_traits>
#include <utility>

namespace details {

template <typename T>
concept has_swap = requires(T t1, T t2) {
  { swap(t1, t2) };
};

// Container
template <typename... Types>
struct container;

template <typename First, typename... Tail>
struct container<First, Tail...> {
  static constexpr bool is_uniq = !contains_type_v<First, Tail...> && container<Tail...>::is_uniq;
  static constexpr bool is_copy_constructable =
      std::is_copy_constructible_v<First> && container<Tail...>::is_copy_constructable;
  static constexpr bool is_trivially_copy_constructible =
      std::is_trivially_copy_constructible_v<First> && container<Tail...>::is_trivially_copy_constructible;
  static constexpr bool is_move_constructable =
      std::is_move_constructible_v<First> && container<Tail...>::is_move_constructable;
  static constexpr bool is_trivially_move_constructible =
      std::is_trivially_move_constructible_v<First> && container<Tail...>::is_trivially_move_constructible;
  static constexpr bool is_nothrow_move_constructible =
      std::is_nothrow_move_constructible_v<First> && container<Tail...>::is_nothrow_move_constructible;
  static constexpr bool is_copy_assignable = std::is_copy_assignable_v<First> && container<Tail...>::is_copy_assignable;
  static constexpr bool is_trivially_copy_assignable =
      std::is_trivially_copy_assignable_v<First> && container<Tail...>::is_trivially_copy_assignable;
  static constexpr bool is_move_assignable = std::is_move_assignable_v<First> && container<Tail...>::is_move_assignable;
  static constexpr bool is_trivially_move_assignable =
      std::is_trivially_move_assignable_v<First> && container<Tail...>::is_trivially_move_assignable;
  static constexpr bool is_nothrow_move_assignable =
      std::is_nothrow_move_assignable_v<First> && container<Tail...>::is_nothrow_move_assignable;
  static constexpr bool is_trivially_destructible =
      std::is_trivially_destructible_v<First> && container<Tail...>::is_trivially_destructible;

  union storage {
    constexpr storage() noexcept(std::is_nothrow_default_constructible_v<First>)
      requires(std::is_default_constructible_v<First> && std::is_trivially_destructible_v<First>)
        : curr() {}

    constexpr storage() noexcept(std::is_nothrow_default_constructible_v<First>)
      requires(!std::is_default_constructible_v<First> || !std::is_trivially_destructible_v<First>)
    {}

    constexpr storage(int) noexcept : tail(0) {}

    constexpr ~storage() noexcept
      requires(container<First, Tail...>::is_trivially_destructible)
    = default;

    constexpr ~storage() noexcept
      requires(!container<First, Tail...>::is_trivially_destructible)
    {}

    container<Tail...> tail;
    First curr;
  } data;

  constexpr container() noexcept = default;

  constexpr container(int) noexcept : data(0) {}

  constexpr ~container() noexcept = default;

  constexpr void copy(size_t cur_index, size_t index, const container& other) {
    if (cur_index == index) {
      copy_assign(index, other);
    } else {
      clear(cur_index);
      copy_construct(index, other);
    }
  }

  constexpr void copy_construct(size_t index, const container& other) {
    if (index == 0) {
      std::construct_at(std::addressof(data.curr), other.data.curr);
    } else {
      data.tail.copy_construct(index - 1, other.data.tail);
    }
  }

  constexpr void copy_assign(size_t index, const container& other) {
    if (index == 0) {
      if constexpr (std::is_copy_assignable_v<First>) {
        data.curr = other.data.curr;
      } else {
        clear(index);
        std::construct_at(std::addressof(data.curr), other.data.curr);
      }
    } else {
      data.tail.copy_assign(index - 1, other.data.tail);
    }
  }

  constexpr void move(size_t cur_index, size_t index,
                      container&& other) noexcept(container::is_nothrow_move_constructible) {
    if (cur_index == index) {
      move_assign(index, std::move(other));
    } else {
      clear(cur_index);
      move_construct(index, std::move(other));
    }
  }

  constexpr void move_construct(size_t index, container&& other) noexcept(container::is_nothrow_move_constructible) {
    if (index == 0) {
      std::construct_at(std::addressof(data.curr), std::move(other.data.curr));
    } else {
      data.tail.move_construct(index - 1, std::move(other.data.tail));
    }
  }

  constexpr void move_assign(size_t index, container&& other) noexcept(container::is_nothrow_move_constructible) {
    if (index == 0) {
      if constexpr (std::is_move_assignable_v<First>) {
        data.curr = std::move(other.data.curr);
      } else {
        clear(index);
        std::construct_at(std::addressof(data.curr), std::move(other.data.curr));
      }
    } else {
      data.tail.move_assign(index - 1, std::move(other.data.tail));
    }
  }

  constexpr void clear(size_t index) noexcept {
    if (index == 0) {
      data.curr.~First();
      std::construct_at(&data.tail, 0);
    } else {
      data.tail.clear(index - 1);
    }
  }

  template <size_t I>
    requires(I != 0)
  static constexpr get_type_t<I, First, Tail...>* get(container& other) {
    return container<Tail...>::template get<I - 1>(other.data.tail);
  }

  template <size_t I>
    requires(I == 0)
  static constexpr get_type_t<0, First, Tail...>* get(container& other) {
    return std::addressof(other.data.curr);
  }

  template <size_t I>
    requires(I != 0)
  static constexpr const get_type_t<I, First, Tail...>* get(const container& other) {
    return container<Tail...>::template get<I - 1>(other.data.tail);
  }

  template <size_t I>
    requires(I == 0)
  static constexpr const get_type_t<0, First, Tail...>* get(const container& other) {
    return std::addressof(other.data.curr);
  }

  constexpr void swap_c(size_t index, container& other) {
    if (index == 0) {
      if constexpr (has_swap<First>) {
        swap(data.curr, other.data.curr);
      } else {
        std::swap(data.curr, other.data.curr);
      }
    } else {
      data.tail.swap_c(index - 1, other.data.tail);
    }
  }
};

template <>
struct container<> {
  static constexpr bool is_uniq = true;
  static constexpr bool is_copy_constructable = true;
  static constexpr bool is_trivially_copy_constructible = true;
  static constexpr bool is_move_constructable = true;
  static constexpr bool is_trivially_move_constructible = true;
  static constexpr bool is_nothrow_move_constructible = true;
  static constexpr bool is_copy_assignable = true;
  static constexpr bool is_trivially_copy_assignable = true;
  static constexpr bool is_move_assignable = true;
  static constexpr bool is_trivially_move_assignable = true;
  static constexpr bool is_nothrow_move_assignable = true;
  static constexpr bool is_trivially_destructible = true;

  constexpr container(int) noexcept {}

  constexpr void copy(size_t, size_t, const container&) noexcept {}

  constexpr void copy_construct(size_t, const container&) noexcept {}

  constexpr void copy_assign(size_t, const container&) noexcept {}

  constexpr void move(size_t, size_t, container&&) noexcept {}

  constexpr void move_construct(size_t, container&&) noexcept {}

  constexpr void move_assign(size_t, container&&) noexcept {}

  constexpr void clear(size_t) noexcept {}

  template <size_t I>
  static constexpr get_type_t<I>* get(container&) {
    return nullptr;
  }

  template <size_t I>
  static constexpr const get_type_t<I>* get(const container&) {
    return nullptr;
  }

  constexpr void swap_c(size_t, container&) {}
};

} // namespace details
