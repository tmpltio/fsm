#ifndef TMPLT_FSM_FSM_H
#define TMPLT_FSM_FSM_H

#include <concepts>
#include <type_traits>
#include <utility>
#include <functional>
#include <variant>
#include <optional>
#include <tuple>
#include <cstddef>

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

template<typename T>
using states_t = typename std::remove_cvref_t<T>::states_t;

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

template<typename StateMachine, typename Event>
concept state_machine_state_event_processable = requires(StateMachine&& machine, Event const& event)
{
    { std::forward<StateMachine>(machine).process_event(event) } -> std::convertible_to<bool>;
};

template<typename StateMachine, typename State, std::size_t ... I>
[[nodiscard]] consteval bool state_machine_state_processable(std::index_sequence<I...>) noexcept
{
    return (state_machine_state_event_processable<StateMachine, event_t<std::tuple_element_t<I, transitions_t<State>>>> && ...);
}

template<typename StateMachine, std::size_t ... I>
[[nodiscard]] consteval bool state_machine_processable(std::index_sequence<I...>) noexcept
{
    return (state_machine_state_processable<StateMachine, std::tuple_element_t<I, states_t<StateMachine>>>(std::make_index_sequence<std::tuple_size_v<transitions_t<std::tuple_element_t<I, states_t<StateMachine>>>>>{}) && ...);
}

template<typename StateMachine>
concept state_machine_visitable = requires(StateMachine&& machine)
{
    std::forward<StateMachine>(machine).visit_state([](auto&&) {});
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

class state_factory
{
    template<typename State, typename ... Transitions>
    class internal_state
    {
        class indexes
        {
            template<typename Transition, std::size_t Index>
            struct indexed_transition
            {
                using type = Transition;

                static constexpr auto index = Index;
            };

            template<std::size_t ... Indexes>
            [[nodiscard]] static consteval auto get_indexed_transitions(std::index_sequence<Indexes...>) noexcept
            {
                return type_tag_t<indexed_transition<Transitions, Indexes>...>{};
            }

            template<typename Event>
            [[nodiscard]] static consteval auto get_ordered_indexes(auto indexed_transitions) noexcept
            {
                return get_indexes<Event, false>(indexed_transitions, get_indexes<Event, true>(indexed_transitions, std::index_sequence<>{}));
            }

            template<typename Event, bool Guarded, typename Current, typename ... Rest, std::size_t ... Indexes>
            [[nodiscard]] static consteval auto get_indexes(type_tag_t<Current, Rest...>, std::index_sequence<Indexes...>) noexcept
            {
                if constexpr(std::is_same_v<detail::event_t<typename Current::type>, Event> && Guarded == transition_with_guard<typename Current::type>)
                {
                    return get_indexes<Event, Guarded>(type_tag_t<Rest...>{}, std::index_sequence<Indexes..., Current::index>{});
                }
                else
                {
                    return get_indexes<Event, Guarded>(type_tag_t<Rest...>{}, std::index_sequence<Indexes...>{});
                }
            }

            template<typename, bool, std::size_t ... Indexes>
            [[nodiscard]] static consteval auto get_indexes(type_tag_t<>, std::index_sequence<Indexes...> indexes) noexcept
            {
                return indexes;
            }

        public:
            template<typename Event>
            [[nodiscard]] static consteval auto get_for() noexcept
            {
                constexpr auto indexed_transitions = get_indexed_transitions(std::index_sequence_for<Transitions...>{});
                if constexpr(constexpr auto indexes = get_ordered_indexes<Event>(indexed_transitions); indexes.size())
                {
                    return indexes;
                }
                else
                {
                    return get_ordered_indexes<default_event_t>(indexed_transitions);
                }
            }
        };

        using dispatch_result_t = std::optional<std::variant<type_tag_t<detail::destination_t<Transitions>>...>>;

        template<typename Event, std::size_t Current, std::size_t ... Rest>
        [[nodiscard]] constexpr dispatch_result_t dispatch_event(Event const& event, std::index_sequence<Current, Rest...>) const noexcept(is_dispatch_noexcept<Event, Current, Rest...>())
        {
            if(auto const& transition = get<Current>(transitions); is_transition_allowed(transition, event))
            {
                call_action(transition, event);

                return dispatch_result_t{std::in_place, std::in_place_index<Current>};
            }
            else
            {
                return dispatch_event(event, std::index_sequence<Rest...>{});
            }
        }

        [[nodiscard]] constexpr dispatch_result_t dispatch_event(auto const&, std::index_sequence<>) const noexcept
        {
            return std::nullopt;
        }

        [[nodiscard]] static constexpr bool is_transition_allowed(auto const&, auto const&) noexcept
        {
            return true;
        }

        [[nodiscard]] static constexpr bool is_transition_allowed(transition_with_guard auto const& transition, auto const& event) noexcept(noexcept(transition.guard(event)))
        {
            return std::invoke(transition.guard, event);
        }

        static constexpr void call_action(auto const&, auto const&) noexcept
        {}

        static constexpr void call_action(transition_with_action auto const& transition, auto const& event) noexcept(noexcept(transition.action(event)))
        {
            std::invoke(transition.action, event);
        }

        template<typename Event, std::size_t Current, std::size_t ... Rest>
        [[nodiscard]] static consteval bool is_dispatch_noexcept() noexcept
        {
            using transition_t = std::tuple_element_t<Current, transitions_t>;

            return noexcept(is_transition_allowed(std::declval<transition_t const&>(), std::declval<Event const&>())) && noexcept(call_action(std::declval<transition_t const&>(), std::declval<Event const&>())) && std::is_nothrow_constructible_v<dispatch_result_t, std::in_place_t, std::in_place_index_t<Current>> && noexcept(std::declval<internal_state const&>().dispatch_event(std::declval<Event const&>(), std::declval<std::index_sequence<Rest...>>()));
        }

    public:
        using state_t = State;
        using transitions_t = std::tuple<Transitions...>;

        template<typename ... T>
        requires (std::constructible_from<Transitions, T> && ...)
        explicit constexpr internal_state(type_tag_t<State>, T&&... transitions) noexcept(std::is_nothrow_constructible_v<transitions_t, T...>) : transitions{std::forward<T>(transitions)...}
        {}

        template<typename Event>
        requires ((std::same_as<Event, detail::event_t<Transitions>> || ...) || (std::same_as<default_event_t, detail::event_t<Transitions>> || ...))
        [[nodiscard]] constexpr auto operator()(Event const& event) const noexcept(noexcept(dispatch_event(std::declval<Event const&>(), indexes::template get_for<Event>())))
        {
            return dispatch_event(event, indexes::template get_for<Event>());
        }

    private:
        transitions_t transitions;
    };

    template<typename State, typename ... Transitions>
    internal_state(type_tag_t<State>, Transitions&&...) -> internal_state<State, std::remove_cvref_t<Transitions>...>;

    class transitions_comparator
    {
        template<bool ... B>
        [[nodiscard]] static consteval bool all_or_none() noexcept
        {
            return (B && ...) || (!B && ...);
        }

    public:
        template<typename LHS, typename RHS>
        [[nodiscard]] consteval bool operator()(type_tag_t<LHS, RHS>) const noexcept
        {
            return false;
        }

        template<typename LHS, typename RHS>
        requires (all_or_none<transition_with_guard<LHS>, transition_with_guard<RHS>>())
        [[nodiscard]] consteval bool operator()(type_tag_t<LHS, RHS>) const noexcept
        {
            return std::is_same_v<detail::event_t<LHS>, detail::event_t<RHS>>;
        }
    };

    template<typename Current, typename ... Rest>
    [[nodiscard]] static consteval bool are_all_transitions_unique(type_tag_t<Current, Rest...>) noexcept
    {
        return is_transition_unique(transitions_comparator{}, type_tag_t<Current, Rest...>{}) && are_all_transitions_unique(type_tag_t<Rest...>{});
    }

    [[nodiscard]] static consteval bool are_all_transitions_unique(type_tag_t<>) noexcept
    {
        return true;
    }

    template<typename Current, typename ... Rest>
    [[nodiscard]] static consteval bool is_transition_unique(auto&& comparator, type_tag_t<Current, Rest...>) noexcept
    {
        return !(comparator(type_tag_t<Current, Rest>{}) || ...);
    }

    [[nodiscard]] static consteval bool is_transition_unique(auto&&, type_tag_t<>) noexcept
    {
        return true;
    }

public:
    template<typename State, transition ... Transitions>
    requires (are_all_transitions_unique(type_tag_t<Transitions...>{}))
    [[nodiscard]] static constexpr state auto create_state(Transitions&&... transitions) noexcept(noexcept(internal_state{std::declval<type_tag_t<State>>(), std::declval<Transitions>()...}))
    {
        return internal_state{type_tag_t<std::remove_cvref_t<State>>{}, std::forward<Transitions>(transitions)...};
    }
};

template<typename StateMachine>
concept state_machine = requires
{
    requires std::copy_constructible<StateMachine>;
    typename detail::states_t<StateMachine>;
    requires detail::state_machine_processable<StateMachine>(std::make_index_sequence<std::tuple_size_v<detail::states_t<StateMachine>>>{});
    requires detail::state_machine_visitable<StateMachine>;
};

}

#endif /* TMPLT_FSM_FSM_H */
