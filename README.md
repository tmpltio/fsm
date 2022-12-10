# C++20 finite state machine library

Library **tmplt/fsm** is header only implementation of *finite state machine*[^1] written in C++20. It supports:
* transitions,
* guarded transitions,
* transitions with actions,
* guarded transitions with actions.

Creation of state machine implementation is very easy thanks to built-in factories:
* ```transition_factory```,
* ```state_factory```,
* ```state_machine_factory```.

For more details see [examples](#Examples).

## Table of contents
1. [API](#api):
  * [Types](#types)
  * [Concepts](#concepts)
  * [Factories](#factories)
2. [Glossary](#glossary):
  * [Transition guard](#transition-guard)
  * [Transition action](#transition-action)
3. [Examples](#examples):
  * [Coin-operated turnstile](#coin-operated-turnstile6)
  * [Substring matcher](#substring-matcher)
  * [Traffic light](#traffic-light)

## API

### Types

#### ```default_event_t```

This type is declared as follows:
```c++
struct default_event_t
{
    explicit constexpr default_event_t() = default;
};
```

It is used as event type in [transition factory](#transition_factory) when creating default transition.

#### ```type_tag_t```

This type is declared as follows:
```c++
template<typename ... T>
struct type_tag_t
{
    explicit constexpr type_tag_t() = default;
}
```

It is used to deduce types internally in state machine and in state visitors, e.g.:
```c++
[]<typename State>(type_tag_t<State>)
{
    /* visitor body */
};
```

#### ```invalid_state_t```

This type is declared as follows:
```c++
struct invalid_state_t
{
    explicit constexpr invalid_state_t() = default;
};
```

It is used as additional state in state machine into which transition occurs when current event is not handled by current state or destination state from current transition was not added to state machine.

### Concepts

#### ```transition```

Specifies requirements for object that is state machine transition:
* is copy constructible[^2],
* has public member type ```event_t``` which is type of event for which transition should be considered as candidate,
* has public member type ```destination_t``` which is type of state that should be destination state for state machine if transition is successful.

#### ```transition_with_guard```

Specifies requirements for object that is transition with guard:
* is ```transition```,
* its public member ```guard``` is [transition guard](#transition-guard).

#### ```transition_with_action```

Specifies requirements for object that is transition with action:
* is ```transition```,
* its public member ```action``` is [transition action](#transition-action).

#### ```state```

Specifies requirements for object that is state machine internal state implementation:
* is copy constructible[^2],
* has public member type ```state_t``` which is type of user provided state for which this internal state implementation was created,
* has public member type ```transitions_t``` which is tuple-like type holding all transitions from this state,
* is regular invocable[^4] with const reference to event (```transition::event_t```) from each transition from this state,
* its ```operator()``` returns ```std::optional``` with ```std::variant``` holding all possible destination states (```transition::destination_t```) from each transition from this state as template parameter for ```type_tag_t``` (```std::optional<std::variant<type_tag_t<transition::destination_t>...>>```).

#### ```state_machine```

Specifies requirements for object that is state machine implementation:
* is copy constructible[^2],
* has public member type ```states_t``` which is tuple-like type holding all states from this state machine,
* has public member function ```process_event``` invocable[^4] with const reference to event (```transition::event_t```) from each transition (```state::transitions_t```) from each state (```state_machine::states_t```) from this state machine and returns value convertible to[^5] ```bool``` for all those invocations,
* has public member function ```visit_state``` invocable[^4] with instance of type invocable with all state types (```state::state_t```) from all states (```states_t```) packed into ```type_tag_t```.

### Factories

#### ```transition_factory```

This class has public static template member function ```create_transition``` with two user provided template parameters:
* ```Event``` - type of event for which created transition will be considered as candidate,
* ```Destination``` - type of state to which state machine should be transitioned on successful transition.

Additional public static template member function ```create_default_transition``` is available with only one user provided template parameter ```Destination```. It uses ```default_event_t``` as event for created transition. Such transition will be considered as candidate if there will be no transition for given event in current state.

This factory has also public friend implementations of ```operator|``` which can be used to add guard and action to created transition (each transition can have one guard and one action). The ```transition``` object must be lhs argument and guard/action must be rhs argument, e.g.:
```c++
auto guard = /* predicate for transition::event_t const& */;
auto action = /* invocable with transition::event_t const& */;
transition auto t0 = transition_factory::create_transition</* Event, Destination */>();
transition auto t1 = transition_factory::create_transition</* Event, Destination */>() | guard;
transition auto t2 = transition_factory::create_transition</* Event, Destination */>() | action;
transition auto t3 = transition_factory::create_transition</* Event, Destination */>() | guard | action;
```

#### ```state_factory```

This class has public static template member function ```create_state``` with one user provided template parameter:
* ```State``` - type of state for which internal state representation will be created.

This method accepts any number of ```transition``` objects. Only requirement is that all transitions are unique - there is only one transition for each ```transition::event_t``` type. There could be second transition (with possible different ```transition::destination_t```) for same event if one of them is guarded and second is not - in such situation the first candidate will be guarded transition and if it will be not allowed, second (unguarded) transition will be made.

#### ```state_machine_factory```

This class has public static member function ```create_state_machine``` which accepts any number of ```state``` objects. Only requirement is that all states are unique - there is only one state object for each ```state::state_t``` type. Additional state ```state_machine_factory::invalid_state_t``` will be added to state machine, which will be destination for unhandled events or unsupported destination types for supported transitions.

Initial state of state machine is first ```state``` used to create this machine - it could mean that initial state will be ```invalid_state_t``` if no ```state``` objects are given to ```create_state_machine```.

## Glossary

### Transition guard

This is object of type which should meet following requirements:
* is copy constructible[^2],
* is predicate[^3] for transition event argument (```transition::event_t const&```).

Value returned by this object will be used to determine if considered transition should occur:
* for ```true``` transition will be made,
* for ```false``` transition will not be made (and possible second - unguarded - transition for same event will be made).

### Transition action

This is object of type which should meet following requirements:
* is copy constructible[^2],
* is regular invocable[^4] with transition event argument (```transition::event_t const&```),
* doesn't return anything (return type is ```void```).

If selected transition will be made, this object will be called ***before*** switching states in state machine.

## Examples
### Coin-operated turnstile[^6]

Turnstile state machine can be represented by this transition table:
|State   |Event|Destination|
|--------|-----|-----------|
|Locked  |Push |Locked     |
|Locked  |Coin |Unlocked   |
|Unlocked|Push |Locked     |
|Unlocked|Coin |Unlocked   |

First of all we need to define our state and event types:
```c++
struct locked
{};

struct unlocked
{};

struct push
{};

struct coin
{};
```

Now we can create transitions (transitions are the same for both states, so we can reuse them):
```c++
constexpr transition auto to_locked = transition_factory::create_transition<push, locked>();
constexpr transition auto to_unlocked = transition_factory::create_transition<coin, unlocked>();
```

Next step is to create internal state objects with transitions:
```c++
constexpr state auto state_locked = state_factory::create_state<locked>(to_locked, to_unlocked);
constexpr state auto state_unlocked = state_factory::create_state<unlocked>(to_locked, to_unlocked);
```

Final step is to use states to create state machine implementation:
```c++
constexpr state_machine auto machine = state_machine_factory::create_state_machine(state_locked, state_unlocked);
```

To process events we use ```process_event``` method from state machine object:
* ```machine.process_event(push{});```,
* ```machine.process_event(coin{});```.

We can now create state visitor:
```c++
class visitor
{
public:
    template<typename State>
    void operator()(tmplt::fsm::type_tag_t<State>) const noexcept;
};
```
and use ```visit_state``` method from state machine object to execute code based on current state:
```c++
machine.visit_state(visitor{});
```

Live example is available on [godbolt.org](https://godbolt.org/z/rf6q58jnq).

### Substring matcher

Consider simple state machine for finding _fsm_ substring in provided string. It can be represented by this transition table:
|State     |Event                  |Destination|
|----------|-----------------------|-----------|
|Unmatched |Character not equal _f_|Unmatched  |
|Unmatched |Character equal _f_    |Letter _f_ |
|Letter _f_|Character not equal _s_|Unmatched  |
|Letter _f_|Character equal _s_    |Letter _s_ |
|Letter _s_|Character not equal _m_|Unmatched  |
|Letter _s_|Character equal _m_    |Matched    |
|Matched   |Any character          |Matched    |

Such transition table would require creating four event types:
* for character _f_,
* for character _s_,
* for character _m_,
* for other characters.

Instead of doing above, lets create only one event type which will be storing actual character value and use guards:
|State     |Event    |Guard           |Destination|
|----------|---------|----------------|-----------|
|Unmatched |Character|                |Unmatched  |
|Unmatched |Character|Is character _f_|Letter _f_ |
|Letter _f_|Character|                |Unmatched  |
|Letter _f_|Character|Is character _s_|Letter _s_ |
|Letter _s_|Character|                |Unmatched  |
|Letter _s_|Character|Is character _m_|Matched    |
|Matched   |Character|                |Matched    |

Our state and event types are defined as follows:
```c++
struct unmatched
{};

struct letter_f
{};

struct letter_s
{};

struct matched
{};

struct character
{
    char value;
};
```

We also need guard type definition:
```c++
template<char C>
struct is_character
{
    bool operator()(character const& c) const noexcept
    {
        return c.value == C;
    }
};
```

Now we can create transitions:
```c++
constexpr transition auto to_unmatched = transition_factory::create_transition<character, unmatched>();
constexpr transition auto to_f = transition_factory::create_transition<character, letter_f>() | is_character<'f'>{};
constexpr transition auto to_s = transition_factory::create_transition<character, letter_s>() | is_character<'s'>{};
constexpr transition auto to_matched = transition_factory::create_transition<character, matched>() | is_character<'m'>{};
constexpr transition auto to_self_matched = transition_factory::create_transition<character, matched>();
```

Next step is to create state objects using transitions from above:
```c++
constexpr state auto state_unmatched = state_factory::create_state<unmatched>(to_unmatched, to_f);
constexpr state auto state_f = state_factory::create_state<letter_f>(to_unmatched, to_s);
constexpr state auto state_s = state_factory::create_state<letter_s>(to_unmatched, to_matched);
constexpr state auto state_matched = state_factory::create_state<matched>(to_self_matched);
```

Final step is using states to create state machine object:
```c++
constexpr state_machine auto machine = state_machine_factory::create_state_machine(state_unmatched, state_f, state_s, state_matched);
```

Having some ```std::string_view``` named ```view```, we can now find substring _fsm_ in it:
```c++
for(auto c : view)
{
    if(!sm.process_event(character{c}))
    {
        break;
    }
}
```

We can now create state visitor:
```c++
class visitor
{
public:
    template<typename State>
    bool operator()(tmplt::fsm::type_tag_t<State>) const noexcept
    {
        return std::is_same_v<State, matched>;
    }
};
```
to check if such substring was found:
```c++
sm.visit_state(visitor{});
```

Live example is available on [godbolt.org](https://godbolt.org/z/5eMKGsvYM).

### Traffic light

Consider traffic light with four states:
* red,
* red and yellow,
* green,
* yellow.

Transition between states occurs when time for each state elapses. When switching between states, light bulbs are turned off/on based on next state and next state timer is restarted. This can be represented by following transition table:
|State               |Guard                          |Action                                                      |
|--------------------|-------------------------------|------------------------------------------------------------|
|State red           |Time for red elapsed           |Reset red and yellow timer and turn on yellow               |
|State red and yellow|Time for red and yellow elapsed|Reset green timer, turn off red and yellow and turn on green|
|State green         |Time for green elapsed         |Reset yellow timer, turn off green and turn on yellow       |
|State yellow        |Time for yellow elapsed        |Reset red timer, turn off yellow and turn on red            |

We can start by declaring light color tag types:
```c++
struct red
{};

struct yellow
{};

struct green
{};
```

Next thing is to define light bulbs driver class:
```c++
template<typename Color>
struct lamp
{
    static void turn_on() noexcept;
    static void turn_off() noexcept;
};
```
state types:
```c++
template<typename ... Colors>
struct traffic_light_state
{};

using state_red = traffic_light_state<red>;
using state_red_yellow = traffic_light_state<red, yellow>;
using state_green = traffic_light_state<green>;
using state_yellow = traffic_light_state<yellow>;
```
and timer type:
```c++
template<typename State>
struct timer
{
    void restart() noexcept;
    bool hit() const noexcept;
};
```

We also need following guard type definition:
```c++
template<typename State>
struct guard
{
    explicit constexpr guard(timer<State>& t) noexcept : t{t}
    {}

    bool operator()(auto&&) const noexcept
    {
        return t.hit();
    }

private:
    timer<State>& t;
};
```
and action type definition:
```c++
template<typename State>
struct action
{
    explicit constexpr action(timer<State>& t) noexcept : t{t}
    {}

    void operator()(auto&&) const noexcept
    {
        t.restart();
        turn_off_all();
        turn_on_colors(State{});
    }

private:
    static void turn_off_all() noexcept
    {
        lamp<red>::turn_off();
        lamp<yellow>::turn_off();
        lamp<green>::turn_off();
    }

    template<typename ... Colors>
    static void turn_on_colors(traffic_light_state<Colors...>)
    {
        (lamp<Colors>::turn_on(), ...);
    }

    timer<State>& t;
};
```

To create state machine lets first assume that we have following references objects available:
```c++
timer<state_red>& timer_red = /* initializer */;
timer<state_red_yellow>& timer_red_yellow = /* initializer */;
timer<state_green>& timer_green = /* initializer */;
timer<state_yellow>& timer_yellow = /* initializer */;
```

We start creation of state machine by definition transition objects:
```c++
transition auto to_red_yellow = transition_factory::create_default_transition<state_red_yellow>() | guard<state_red>{timer_red} | action<state_red_yellow>{timer_red_yellow};
transition auto to_green = transition_factory::create_default_transition<state_green>() | guard<state_red_yellow>{timer_red_yellow} | action<state_green>{timer_green};
transition auto to_yellow = transition_factory::create_default_transition<state_yellow>() | guard<state_green>{timer_green} | action<state_yellow>{timer_yellow};
transition auto to_red = transition_factory::create_default_transition<state_red>() | guard<state_yellow>{timer_yellow} | action<state_red>{timer_red};
```

Next step is to create state objects:
```c++
state auto state_r = state_factory::create_state<state_red>(to_red_yellow);
state auto state_ry = state_factory::create_state<state_red_yellow>(to_green);
state auto state_g = state_factory::create_state<state_green>(to_yellow);
state auto state_y = state_factory::create_state<state_yellow>(to_red);
```

Final step is to use state objects to create state machine:
```c++
state_machine auto machine = state_machine_factory::create_state_machine(state_r, state_ry, state_g, state_y);
```

We can now run it in loop:
```c++
while(sm.process_event(tmplt::fsm::default_event_t{}));
```

Live example is available on [godbolt.org](https://godbolt.org/z/8rMcrcfEv).

[^1]: [https://en.wikipedia.org/wiki/Finite-state_machine]
[^2]: [https://en.cppreference.com/w/cpp/concepts/copy_constructible]
[^3]: [https://en.cppreference.com/w/cpp/concepts/predicate]
[^4]: [https://en.cppreference.com/w/cpp/concepts/invocable]
[^5]: [https://en.cppreference.com/w/cpp/concepts/convertible_to]
[^6]: [https://en.wikipedia.org/wiki/Finite-state_machine#Example:_coin-operated_turnstile]
