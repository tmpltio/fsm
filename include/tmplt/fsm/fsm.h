#ifndef TMPLT_FSM_FSM_H
#define TMPLT_FSM_FSM_H

#include <concepts>
#include <type_traits>
#include <utility>

namespace tmplt::fsm
{

namespace detail
{

template<typename T>
struct add_const_lvalue_reference
{
    using type = std::add_lvalue_reference_t<std::add_const_t<std::remove_cvref_t<T>>>;
};

template<typename T>
using add_const_lvalue_reference_t = typename add_const_lvalue_reference<T>::type;

template<typename T>
using event_t = typename std::remove_cvref_t<T>::event_t;

template<typename T>
using destination_t = typename std::remove_cvref_t<T>::destination_t;

template<typename T>
using guard_t = decltype(std::remove_cvref_t<T>::guard);

template<typename T>
using action_t = decltype(std::remove_cvref_t<T>::action);

template<typename T, typename ... Args>
concept nothrow_constructible_from = requires
{
    requires std::destructible<T>;
    requires std::is_nothrow_constructible_v<T, Args...>;
};

template<typename From, typename To>
concept nothrow_convertible_to = requires
{
    requires std::is_nothrow_convertible_v<From, To>;
    { static_cast<To>(std::declval<From>()) } noexcept;
};

template<typename T>
concept nothrow_move_constructible = requires
{
    requires nothrow_constructible_from<T, T>;
    requires nothrow_convertible_to<T, T>;
};

template<typename Guard, typename Event>
concept transition_guard = requires
{
    requires nothrow_move_constructible<std::remove_cvref_t<Guard>>;
    requires std::predicate<add_const_lvalue_reference_t<Guard>, add_const_lvalue_reference_t<Event>>;
};

template<typename Action, typename Event>
concept transition_action = requires
{
    requires nothrow_move_constructible<std::remove_cvref_t<Action>>;
    requires std::regular_invocable<add_const_lvalue_reference_t<Action>, add_const_lvalue_reference_t<Event>>;
    requires std::same_as<std::invoke_result_t<add_const_lvalue_reference_t<Action>, add_const_lvalue_reference_t<Event>>, void>;
};

}

template<typename Transition>
concept transition = requires
{
    requires detail::nothrow_move_constructible<std::remove_cvref_t<Transition>>;
    typename detail::event_t<Transition>;
    typename detail::destination_t<Transition>;
};

template<typename Transition>
concept transition_with_guard = requires
{
    requires transition<Transition>;
    requires detail::transition_guard<detail::guard_t<Transition>, detail::event_t<Transition>>;
};

template<typename Transition>
concept transition_with_action = requires
{
    requires transition<Transition>;
    requires detail::transition_action<detail::action_t<Transition>, detail::event_t<Transition>>;
};

class transition_factory
{
    template<typename Event, typename Destination>
    struct internal_transition
    {
        using event_t = Event;
        using destination_t = Destination;
    };

    template<typename Transition, typename Guard>
    struct internal_transition_with_guard : Transition
    {
        Guard guard;
    };

    template<typename Transition, typename Guard>
    internal_transition_with_guard(Transition&&, Guard&&) -> internal_transition_with_guard<std::remove_cvref_t<Transition>, std::remove_cvref_t<Guard>>;

    template<typename Transition, typename Action>
    struct internal_transition_with_action : Transition
    {
        Action action;
    };

    template<typename Transition, typename Action>
    internal_transition_with_action(Transition&&, Action&&) -> internal_transition_with_action<std::remove_cvref_t<Transition>, std::remove_cvref_t<Action>>;

    struct default_event
    {};

public:
    using default_event_t = default_event;

    template<typename Event, typename Destination>
    [[nodiscard]] static constexpr transition auto create_transition() noexcept
    {
        return internal_transition<std::remove_cvref_t<Event>, std::remove_cvref_t<Destination>>{};
    }

    template<typename Destination>
    [[nodiscard]] static constexpr transition auto create_default_transition() noexcept
    {
        return create_transition<default_event_t, Destination>();
    }

    template<transition Transition, detail::transition_guard<detail::event_t<Transition>> Guard>
    requires (!requires{ typename detail::guard_t<Transition>; })
    [[nodiscard]] friend constexpr transition_with_guard auto operator|(Transition&& t, Guard&& g) noexcept
    {
        return internal_transition_with_guard{std::forward<Transition>(t), std::forward<Guard>(g)};
    }

    template<transition Transition, detail::transition_action<detail::event_t<Transition>> Action>
    requires (!requires{ typename detail::action_t<Transition>; })
    [[nodiscard]] friend constexpr transition_with_action auto operator|(Transition&& t, Action&& a) noexcept
    {
        return internal_transition_with_action{std::forward<Transition>(t), std::forward<Action>(a)};
    }
};

}

#endif /* TMPLT_FSM_FSM_H */
