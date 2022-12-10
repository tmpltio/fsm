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
using event_t = typename std::remove_cvref_t<T>::event_t;

template<typename T>
using destination_t = typename std::remove_cvref_t<T>::destination_t;

template<typename T>
using guard_t = decltype(std::remove_cvref_t<T>::guard);

template<typename T>
using action_t = decltype(std::remove_cvref_t<T>::action);

template<typename Guard, typename Event>
concept transition_guard = requires
{
    requires std::copy_constructible<Guard>;
    requires std::predicate<Guard const&, Event const&>;
};

template<typename Action, typename Event>
concept transition_action = requires
{
    requires std::copy_constructible<Action>;
    requires std::regular_invocable<Action const&, Event const&>;
    requires std::same_as<std::invoke_result_t<Action const&, Event const&>, void>;
};

}

template<typename Transition>
concept transition = requires
{
    requires std::copy_constructible<Transition>;
    typename detail::event_t<Transition>;
    typename detail::destination_t<Transition>;
};

template<typename Transition>
concept transition_with_guard = requires
{
    requires transition<Transition>;
    typename detail::guard_t<Transition>;
    requires detail::transition_guard<detail::guard_t<Transition>, detail::event_t<Transition>>;
};

template<typename Transition>
concept transition_with_action = requires
{
    requires transition<Transition>;
    typename detail::action_t<Transition>;
    requires detail::transition_action<detail::action_t<Transition>, detail::event_t<Transition>>;
};

}

#endif /* TMPLT_FSM_FSM_H */
