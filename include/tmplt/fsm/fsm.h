#ifndef TMPLT_FSM_FSM_H
#define TMPLT_FSM_FSM_H

#include <concepts>
#include <type_traits>
#include <utility>
#include <variant>
#include <optional>

namespace tmplt::fsm
{

struct default_event_t
{
    explicit constexpr default_event_t() = default;
};

template<typename ... T>
struct type_tag_t
{
    explicit constexpr type_tag_t() = default;
};

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

template<typename T>
using state_t = typename std::remove_cvref_t<T>::state_t;

template<typename T>
using transitions_t = typename std::remove_cvref_t<T>::transitions_t;

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

template<typename State, std::size_t ... I>
[[nodiscard]] consteval bool state_invocable(std::index_sequence<I...>) noexcept
{
    return (std::regular_invocable<State const&, event_t<std::tuple_element_t<I, transitions_t<State>>> const&> && ...);
}

template<typename State, std::size_t ... I>
[[nodiscard]] consteval bool state_result(std::index_sequence<I...>) noexcept
{
    return (std::same_as<std::invoke_result_t<State const&, event_t<std::tuple_element_t<I, transitions_t<State>>> const&>, std::optional<std::variant<type_tag_t<destination_t<std::tuple_element_t<I, transitions_t<State>>>>...>>> && ...);
}

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

public:
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
    [[nodiscard]] friend constexpr transition_with_guard auto operator|(Transition&& t, Guard&& g) noexcept(noexcept(internal_transition_with_guard{std::declval<Transition>(), std::declval<Guard>()}))
    {
        return internal_transition_with_guard{std::forward<Transition>(t), std::forward<Guard>(g)};
    }

    template<transition Transition, detail::transition_action<detail::event_t<Transition>> Action>
    requires (!requires{ typename detail::action_t<Transition>; })
    [[nodiscard]] friend constexpr transition_with_action auto operator|(Transition&& t, Action&& a) noexcept(noexcept(internal_transition_with_action{std::declval<Transition>(), std::declval<Action>()}))
    {
        return internal_transition_with_action{std::forward<Transition>(t), std::forward<Action>(a)};
    }
};

template<typename State>
concept state = requires
{
    requires std::copy_constructible<State>;
    typename detail::state_t<State>;
    typename detail::transitions_t<State>;
    requires detail::state_invocable<State>(std::make_index_sequence<std::tuple_size_v<detail::transitions_t<State>>>{});
    requires detail::state_result<State>(std::make_index_sequence<std::tuple_size_v<detail::transitions_t<State>>>{});
};

}

#endif /* TMPLT_FSM_FSM_H */
