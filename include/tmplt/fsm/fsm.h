#ifndef TMPLT_FSM_FSM_H
#define TMPLT_FSM_FSM_H

#include <concepts>
#include <type_traits>
#include <utility>
#include <variant>
#include <optional>
#include <tuple>
#include <cstddef>
#include <functional>

namespace tmplt::fsm
{

template<typename ... T>
struct type_tag_t
{
    explicit constexpr type_tag_t() = default;
};

template<typename ... T>
inline constexpr type_tag_t<T...> type_tag{};

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

template<typename T>
using state_t = typename std::remove_cvref_t<T>::state_t;

template<typename T>
using transitions_t = typename std::remove_cvref_t<T>::transitions_t;

template<typename T>
using states_t = typename std::remove_cvref_t<T>::states_t;

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

template<typename State, std::size_t ... I>
[[nodiscard]] consteval bool state_invocable(std::index_sequence<I...>) noexcept
{
    return (std::regular_invocable<add_const_lvalue_reference_t<State>, add_const_lvalue_reference_t<event_t<std::tuple_element_t<I, transitions_t<State>>>>> && ...);
}

template<typename State, std::size_t ... I>
[[nodiscard]] consteval bool state_result(std::index_sequence<I...>) noexcept
{
    return (std::same_as<std::invoke_result_t<add_const_lvalue_reference_t<State>, add_const_lvalue_reference_t<event_t<std::tuple_element_t<I, transitions_t<State>>>>>, std::optional<std::variant<type_tag_t<destination_t<std::tuple_element_t<I, transitions_t<State>>>>...>>> && ...);
}

template<typename StateMachine, typename Event>
concept state_machine_state_event_processable = requires(StateMachine&& machine, add_const_lvalue_reference_t<Event> event)
{
    { machine.process_event(event) } -> std::convertible_to<bool>;
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

template<typename Visitor, typename StateMachine, std::size_t ... I>
[[nodiscard]] consteval bool state_visitor_invocable(std::index_sequence<I...>) noexcept
{
    return (std::invocable<Visitor, type_tag_t<std::tuple_element_t<I, states_t<StateMachine>>>> && ...);
}

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

template<typename State>
concept state = requires
{
    requires detail::nothrow_move_constructible<std::remove_cvref_t<State>>;
    typename detail::state_t<State>;
    requires detail::state_invocable<State>(std::make_index_sequence<std::tuple_size_v<detail::transitions_t<State>>>{});
    requires detail::state_result<State>(std::make_index_sequence<std::tuple_size_v<detail::transitions_t<State>>>{});
};

class state_factory
{
    template<typename State, typename ... Transitions>
    class internal_state
    {
        class transitions_indexes
        {
            template<typename Transition, std::size_t Index>
            struct indexed_transition
            {
                using type = Transition;

                static constexpr auto index = Index;
                static constexpr auto guarded = transition_with_guard<Transition>;
            };

            template<typename T>
            using indexed_transition_t = typename T::type;

            template<std::size_t ... Indexes>
            [[nodiscard]] static consteval auto get_indexed_transitions(std::index_sequence<Indexes...>) noexcept
            {
                return type_tag<indexed_transition<Transitions, Indexes>...>;
            }

            template<typename Event, typename ... Indexed>
            [[nodiscard]] static consteval auto get_ordered_indexes_for_event(type_tag_t<Indexed...> indexed_transitions) noexcept
            {
                return get_indexes_for_event<Event, false>(indexed_transitions, get_indexes_for_event<Event, true>(indexed_transitions, std::index_sequence<>{}));
            }

            template<typename Event, bool Guarded, typename Current, typename ... Rest, std::size_t ... Indexes>
            [[nodiscard]] static consteval auto get_indexes_for_event(type_tag_t<Current, Rest...>, std::index_sequence<Indexes...>) noexcept
            {
                if constexpr(std::is_same_v<detail::event_t<indexed_transition_t<Current>>, Event> && Guarded == Current::guarded)
                {
                    return get_indexes_for_event<Event, Guarded>(type_tag<Rest...>, std::index_sequence<Indexes..., Current::index>{});
                }
                else
                {
                    return get_indexes_for_event<Event, Guarded>(type_tag<Rest...>, std::index_sequence<Indexes...>{});
                }
            }

            template<typename, bool, std::size_t ... Indexes>
            [[nodiscard]] static consteval auto get_indexes_for_event(type_tag_t<>, std::index_sequence<Indexes...> indexes) noexcept
            {
                return indexes;
            }

        public:
            template<typename Event>
            [[nodiscard]] static consteval auto get_for_event() noexcept
            {
                constexpr auto indexed_transitions = get_indexed_transitions(std::index_sequence_for<Transitions...>{});
                if constexpr(constexpr auto indexes = get_ordered_indexes_for_event<Event>(indexed_transitions); indexes.size())
                {
                    return indexes;
                }
                else
                {
                    return get_ordered_indexes_for_event<transition_factory::default_event_t>(indexed_transitions);
                }
            }
        };

        using dispatch_result_t = std::optional<std::variant<type_tag_t<detail::destination_t<Transitions>>...>>;

        template<std::size_t Current, std::size_t ... Rest>
        [[nodiscard]] constexpr dispatch_result_t dispatch_event(auto const& event, std::index_sequence<Current, Rest...>) const noexcept(noexcept(is_dispatch_noexcept<Current, Rest...>(this, event)))
        {
            if(auto const& transition = std::get<Current>(transitions); is_transition_allowed(transition, event))
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
            return transition.guard(event);
        }

        static constexpr void call_action(auto const&, auto const&) noexcept
        {}

        static constexpr void call_action(transition_with_action auto const& transition, auto const& event) noexcept(noexcept(transition.action(event)))
        {
            transition.action(event);
        }

        template<std::size_t Current, std::size_t ... Rest>
        [[nodiscard]] static consteval bool is_dispatch_noexcept(auto const& state, auto const& event) noexcept
        {
            auto const& transition = std::get<Current>(state.transitions);

            return noexcept(is_transition_allowed(transition, event)) && noexcept(call_action(transition, event)) && noexcept(state.dispatch_event(event, std::index_sequence<Rest...>{}));
        }

    public:
        using state_t = State;
        using transitions_t = std::tuple<Transitions...>;

        template<typename ... T>
        requires (detail::nothrow_constructible_from<Transitions, T> && ...)
        explicit constexpr internal_state(type_tag_t<State>, T&&... transitions) noexcept : transitions{std::forward<T>(transitions)...}
        {}

        template<typename Event>
        requires ((std::same_as<Event, detail::event_t<Transitions>> || ...) || (std::same_as<transition_factory::default_event_t, detail::event_t<Transitions>> || ...))
        [[nodiscard]] constexpr auto operator()(Event const& event) const noexcept(noexcept(dispatch_event(event, transitions_indexes::template get_for_event<Event>())))
        {
            return dispatch_event(event, transitions_indexes::template get_for_event<Event>());
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
        return is_transition_unique(transitions_comparator{}, type_tag<Current, Rest...>) && are_all_transitions_unique(type_tag<Rest...>);
    }

    [[nodiscard]] static consteval bool are_all_transitions_unique(type_tag_t<>) noexcept
    {
        return true;
    }

    template<typename Current, typename ... Rest>
    [[nodiscard]] static consteval bool is_transition_unique(auto&& comparator, type_tag_t<Current, Rest...>) noexcept
    {
        return !(comparator(type_tag<Current, Rest>) || ...);
    }

    [[nodiscard]] static consteval bool is_transition_unique(auto&&, type_tag_t<>) noexcept
    {
        return true;
    }

public:
    template<typename State, transition ... Transitions>
    requires (are_all_transitions_unique(type_tag<Transitions...>))
    [[nodiscard]] static constexpr state auto create_state(Transitions&&... transitions) noexcept
    {
        return internal_state{type_tag<State>, std::forward<Transitions>(transitions)...};
    }
};

template<typename StateMachine>
concept state_machine = requires
{
    requires detail::nothrow_move_constructible<std::remove_cvref_t<StateMachine>>;
    typename std::remove_cvref_t<StateMachine>::invalid_state_t;
    requires detail::state_machine_processable<StateMachine>(std::make_index_sequence<std::tuple_size_v<detail::states_t<StateMachine>>>{});
};

template<typename Visitor, typename StateMachine>
concept state_visitor = requires
{
    requires detail::state_visitor_invocable<Visitor, StateMachine>(std::make_index_sequence<std::tuple_size_v<detail::states_t<StateMachine>>>{});
};

class state_machine_factory
{
    struct invalid_state
    {};

    template<typename ... States>
    class internal_state_machine
    {
        template<typename Event>
        class internal_event_processor
        {
        public:
            explicit constexpr internal_event_processor(internal_state_machine& machine, Event const& event) noexcept : machine{machine}, event{event}
            {}

            constexpr bool operator()(auto) const noexcept
            {
                machine.make_transition(type_tag<invalid_state_t>);

                return false;
            }

            template<typename State>
            requires std::regular_invocable<State, detail::add_const_lvalue_reference_t<Event>>
            constexpr bool operator()(type_tag_t<State>) const noexcept(std::is_nothrow_invocable_v<detail::add_const_lvalue_reference_t<State>, detail::add_const_lvalue_reference_t<Event>>)
            {
                if(auto const handling_result = std::invoke(std::get<State>(machine.states), event); handling_result.has_value())
                {
                    std::visit([this](auto destination)
                    {
                        machine.make_transition(destination);
                    }, *handling_result);
                }

                return true;
            }

        private:
            internal_state_machine& machine;
            Event const& event;
        };

        template<typename Event>
        internal_event_processor(internal_state_machine&, Event const&) -> internal_event_processor<Event>;

        template<typename Visitor>
        class internal_state_visitor
        {
        public:
            explicit constexpr internal_state_visitor(Visitor& visitor) noexcept : visitor{visitor}
            {}

            template<typename State>
            constexpr decltype(auto) operator()(type_tag_t<State>) const noexcept(std::is_nothrow_invocable_v<Visitor, type_tag_t<detail::state_t<State>>>)
            {
                return visitor(type_tag<detail::state_t<State>>);
            }

        private:
            Visitor& visitor;
        };

        template<typename Visitor>
        internal_state_visitor(Visitor&) -> internal_state_visitor<Visitor>;

        template<typename Destination>
        constexpr void make_transition(type_tag_t<Destination>) noexcept
        {
            current_state.template emplace<get_destination_index<Destination, detail::state_t<States>...>({})>();
        }

        template<typename Destination, typename Current, typename ... Rest>
        [[nodiscard]] static consteval auto get_destination_index(std::size_t index) noexcept
        {
            if constexpr(std::is_same_v<Destination, Current>)
            {
                return index;
            }
            else
            {
                return get_destination_index<Destination, Rest...>(index + 1);
            }
        }

        template<typename>
        [[nodiscard]] static consteval auto get_destination_index(std::size_t index) noexcept
        {
            return index - 1;
        }

    public:
        using invalid_state_t = invalid_state;
        using states_t = std::tuple<States...>;

        template<typename ... T>
        requires (detail::nothrow_constructible_from<States, T> && ...)
        explicit constexpr internal_state_machine(T&&... states) noexcept : states{std::forward<T>(states)...}, current_state{std::in_place_index<0>}
        {}

        constexpr bool process_event(auto const& event) noexcept(noexcept(std::visit(internal_event_processor{*this, event}, current_state)))
        {
            return std::visit(internal_event_processor{*this, event}, current_state);
        }

        constexpr decltype(auto) visit_state(state_visitor<internal_state_machine> auto&& visitor) const noexcept(noexcept(std::visit(internal_state_visitor{visitor}, current_state)))
        {
            return std::visit(internal_state_visitor{visitor}, current_state);
        }

    private:
        states_t states;
        std::variant<type_tag_t<States>...> current_state;
    };

    template<typename ... T>
    internal_state_machine(T&&...) -> internal_state_machine<std::remove_cvref_t<T>...>;

    template<typename Current, typename ... Rest>
    [[nodiscard]] static consteval bool are_all_states_unique(type_tag_t<Current, Rest...>) noexcept
    {
        return std::conjunction_v<std::negation<std::is_same<detail::state_t<Current>, detail::state_t<Rest>>>...> && are_all_states_unique(type_tag<Rest...>);
    }

    [[nodiscard]] static consteval bool are_all_states_unique(type_tag_t<>) noexcept
    {
        return true;
    }

public:
    template<state ... States>
    requires (are_all_states_unique(type_tag<States...>))
    [[nodiscard]] static constexpr state_machine auto create_state_machine(States&&... states) noexcept
    {
        return internal_state_machine{std::forward<States>(states)..., state_factory::create_state<invalid_state>()};
    }
};

}

#endif /* TMPLT_FSM_FSM_H */
