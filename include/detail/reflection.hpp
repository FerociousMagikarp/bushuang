#pragma once

#include <memory>
#include <string>
#include <array>
#include <variant>
#include <optional>
#include <algorithm>
#include <concepts>
#include <source_location>
#include "struct_to_tuple.hpp"

namespace bushuang
{

template <typename Enum>
consteval auto get_enum_min_number()
{
    static_assert(std::is_enum_v<Enum>);
    using res_type = std::underlying_type_t<Enum>;
    if (std::is_signed_v<res_type>)
        return static_cast<res_type>(-128);
    else
        return static_cast<res_type>(0);
}

template <typename Enum>
consteval auto get_enum_max_number()
{
    static_assert(std::is_enum_v<Enum>);
    using res_type = std::underlying_type_t<Enum>;
    return static_cast<res_type>(128);
}

namespace detail
{

template <char... C>
struct compile_string
{
    constexpr static size_t size = sizeof...(C);
    constexpr static std::array<char, size> value = { C... };
    consteval static std::string_view string_view() noexcept { return std::string_view{value.data(), size}; }
};

template <typename... CompileString>
struct compile_string_sequence
{
    constexpr static size_t size = sizeof...(CompileString);
};

template <typename T>
extern T fake_object;

template <auto* ptr>
consteval auto get_ptr_name()
{
    auto name = std::string_view {std::source_location::current().function_name()};

#ifdef __GNUC__
    name = name.substr(name.rfind(':') + 1);
    name = name.substr(0, name.rfind(')'));
#elif defined(_MSC_VER)
    name = name.substr(0, name.rfind('>'));
    name = name.substr(name.rfind('>') + 1);
#elif defined(__clang__)
    name = name.substr(name.rfind('.') + 1);
    name = name.substr(0, name.rfind(']'));
#endif
    return name;
}

template <auto e>
consteval auto get_enum_name_impl()
{
    static_assert(std::is_enum_v<std::decay_t<decltype(e)>>);
#ifdef _MSC_VER
    auto name = std::string_view {__FUNCSIG__};
    name = name.substr(name.rfind('<') + 1);
    name = name.substr(0, name.rfind('>'));
    if (name[0] == '(')
        return std::string_view{};
    return name;
#elif defined (__GNUC__)
    auto name = std::string_view {std::source_location::current().function_name()};

    name = name.substr(name.rfind('=') + 1);
    name = name.substr(0, name.rfind(']'));

    auto pos = name.rfind(':');
    if (pos == std::string_view::npos)
        return std::string_view{};
    return name.substr(pos + 1);
#elif defined(__clang__)
    auto name = std::string_view {std::source_location::current().function_name()};

    name = name.substr(name.rfind(' ') + 1);
    name = name.substr(0, name.rfind(']'));
    if (name[0] == '(')
        return std::string_view{};
    return name;
#endif
}

template <typename E, auto Val>
consteval auto get_enum_name()
{
    static_assert(std::is_same_v<std::decay_t<std::underlying_type_t<E>>, std::decay_t<decltype(Val)>>);
// from https://github.com/Neargye/magic_enum.git
#if defined(__clang__) && __clang_major__ >= 16
    // https://reviews.llvm.org/D130058, https://reviews.llvm.org/D131307
    constexpr E e = __builtin_bit_cast(E, Val);
#else
    constexpr E e = static_cast<E>(Val);
#endif
    return get_enum_name_impl<e>();
}

template <typename T>
consteval auto get_class_name()
{
#ifdef _MSC_VER
    auto name = std::string_view {__FUNCSIG__};
    name = name.substr(name.rfind(' ') + 1);
    return name.substr(0, name.rfind('>'));
#elif defined (__GNUC__) || defined (__clang__)
    auto name = std::string_view {std::source_location::current().function_name()};
    name = name.substr(name.rfind(' ') + 1);
    return name.substr(0, name.rfind(']'));
#endif
}

template <typename Var, typename... Args>
struct unique_variant : std::type_identity<Var> {};

template <typename... VArgs, typename Arg, typename... Others>
struct unique_variant<std::variant<VArgs...>, Arg, Others...> : std::conditional_t <
                                                                    (std::is_same_v<Arg, VArgs> || ...),
                                                                    unique_variant<std::variant<VArgs...>, Others...>,
                                                                    unique_variant<std::variant<VArgs..., Arg>, Others...>
                                                                > {};

template <typename... Args>
using unique_variant_t = typename unique_variant<std::variant<>, Args...>::type;

template <typename E>
consteval std::size_t get_enum_count()
{
    using type = std::underlying_type_t<E>;
    std::size_t res = 0;

    [&res] <typename T, T... N> (std::integer_sequence<T, N...>)->void
    {
        res = (... + static_cast<std::size_t>(!get_enum_name<E, N + get_enum_min_number<E>()>().empty()));
    } (std::make_integer_sequence<type, get_enum_max_number<E>() - get_enum_min_number<E>() + 1>{});
    return res;
}

template <typename T, std::size_t N>
consteval auto get_field_name()
{
    constexpr auto tuple = get_tuple(std::addressof(fake_object<T>));
    constexpr auto name = get_ptr_name<std::addressof(std::get<N>(tuple))>();
    return [name] <std::size_t... NC> (std::index_sequence<NC...>)->auto
    {
        return compile_string<name[NC]...>{};
    }(std::make_index_sequence<name.size()>{});
}

template <typename T>
consteval auto get_field_names()
{
    return [] <std::size_t... N> (std::index_sequence<N...>)->auto
    {
        return compile_string_sequence<decltype(get_field_name<T, N>())...>{};
    }(std::make_index_sequence<get_field_num<T>()>{});
}

template <typename T>
using filed_names_t = decltype(get_field_names<std::decay_t<T>>());

template <typename T>
consteval auto get_field_ptr_variant()
{
    constexpr auto tuple = get_tuple(std::addressof(fake_object<T>));
    return [] <typename Tuple, std::size_t... N> (Tuple&& t, std::index_sequence<N...>)->auto
    {
        return unique_variant_t<std::decay_t<std::tuple_element_t<N, std::decay_t<Tuple>>>*...>{};
    }(tuple, std::make_index_sequence<get_field_num<T>()>{});
}

template <typename T>
using field_ptr_variant_t = decltype(get_field_ptr_variant<std::decay_t<T>>());

template <typename Cont, typename F>
consteval void sort(Cont& cont, F&& f)
{
    std::sort(std::begin(cont), std::end(cont), std::forward<F>(f));
}

struct hash_index_pair
{
    std::size_t hash;
    std::size_t index;
};

inline constexpr bool operator<(const std::size_t hash, const hash_index_pair& pair) noexcept { return hash < pair.hash; }
inline constexpr bool operator<(const hash_index_pair& pair, const std::size_t hash) noexcept { return pair.hash < hash; }

template <typename E>
struct hash_enum_pair
{
    std::size_t hash;
    E           val;
};

template <typename E>
inline constexpr bool operator<(const std::size_t hash, const hash_enum_pair<E>& pair) noexcept { return hash < pair.hash; }
template <typename E>
inline constexpr bool operator<(const hash_enum_pair<E>& pair, const std::size_t hash) noexcept { return pair.hash < hash; }

template <typename Hash, std::size_t N, typename NameCont>
consteval auto get_hash_to_index_array(NameCont&& names)
{
    std::array<hash_index_pair, N> res{};
    for (std::size_t i = 0; i < N; i++)
        res[i] = {Hash()(names[i]), i};

    sort(res, [](const auto& left, const auto& right)->bool{ return left.hash < right.hash; });

    for (std::size_t i = 0; i < res.size() - 1; i++)
    {
        if (res[i].hash == res[i + 1].hash)
            throw "Class hash collision. Please change another hash function.";
    }

    return res;
}

template <typename E>
consteval auto get_enum_tmp_array()
{
    using type = std::underlying_type_t<E>;
    constexpr auto total_count = get_enum_max_number<E>() - get_enum_min_number<E>() + 1;
    return [] <typename T, T... N> (std::integer_sequence<T, N...>)->auto
    {
        return std::array<std::string_view, total_count>{get_enum_name<E, N + get_enum_min_number<E>()>()...};
    } (std::make_integer_sequence<type, total_count>{});
}

template <typename E>
consteval auto get_enum_array()
{
    using type = std::underlying_type_t<E>;
    std::array<E, get_enum_count<E>()> res{};

    constexpr auto tmp = get_enum_tmp_array<E>();
    std::size_t index = 0;
    for (std::size_t i = 0; i < tmp.size(); i++)
    {
        if (!tmp[i].empty())
            res[index++] = static_cast<E>(static_cast<type>(i) + get_enum_min_number<E>());
    }
    
    return res;
}

template <typename E>
consteval auto get_enum_name_array()
{
    std::array<std::string_view, get_enum_count<E>()> res{};

    constexpr auto tmp = get_enum_tmp_array<E>();
    std::size_t index = 0;
    for (auto n : tmp)
    {
        if (!n.empty())
            res[index++] = n;
    }

    return res;
}

template <typename E, typename Hash, std::size_t N, typename NameCont, typename EnumCont>
consteval auto get_hash_to_enum_array(NameCont&& name_array, EnumCont&& enum_array)
{
    std::array<hash_enum_pair<E>, N> res{};

    for (std::size_t i = 0; i < N; i++)
        res[i] = {Hash()(name_array[i]), enum_array[i]};

    sort(res, [](const auto& left, const auto& right)->bool{ return left.hash < right.hash; });

    for (std::size_t i = 0; i < res.size() - 1; i++)
    {
        if (res[i].hash == res[i + 1].hash)
            throw "Enum hash collision. Please change another hash function.";
    }

    return res;
}

template <typename T>
consteval auto get_name_array()
{
    using names = filed_names_t<T>;
    return []<typename... Name>(compile_string_sequence<Name...>)->auto
    {
        return std::array<std::string_view, names::size> { std::decay_t<Name>::string_view()... };
    }(names{});
}

} // namespace detail

class fnv_hash;

template <typename T, typename Hash = bushuang::fnv_hash>
    requires std::is_class_v<T> &&
    requires(Hash hash) { {hash(std::string_view{})} -> std::same_as<std::size_t>; } &&
    requires{ detail::get_field_num<T>() > 0; }
class fields_class
{
    using field_ptr_type = typename detail::field_ptr_variant_t<T>;
    constexpr static auto name_array = detail::get_name_array<T>();
    constexpr static auto hash_to_index_array = detail::get_hash_to_index_array<Hash, detail::get_field_num<T>()>(name_array);

    std::array<field_ptr_type, detail::get_field_num<T>()> m_field_ptrs;

public:
    using type = T;
    using hash_type = Hash;

    explicit fields_class(T& t) : m_field_ptrs([&t]<std::size_t... N>(std::index_sequence<N...>)->auto
        {
            auto tuple = detail::get_tuple(std::addressof(t));
            return decltype(fields_class::m_field_ptrs){std::addressof(std::get<N>(tuple))...};
        }(std::make_index_sequence<detail::get_field_num<T>()>{}))
    {}
    ~fields_class() = default;

    [[nodiscard]] constexpr field_ptr_type& operator[](std::string_view name)
    {
        auto result = std::equal_range(hash_to_index_array.cbegin(), hash_to_index_array.cend(), Hash()(name));
        if (result.first == result.second)
            return m_field_ptrs[0];
        return m_field_ptrs[result.first.index];
    }

    [[nodiscard]] constexpr static bool exist(std::string_view name)
    {
        return std::binary_search(hash_to_index_array.cbegin(), hash_to_index_array.cend(), Hash()(name));
    }

    [[nodiscard]] constexpr field_ptr_type* get_if(std::string_view name)
    {
        auto result = std::equal_range(hash_to_index_array.cbegin(), hash_to_index_array.cend(), Hash()(name));
        if (result.first == result.second)
            return nullptr;
        return &m_field_ptrs[result.first.index];
    }
};

template <typename E, typename Hash = bushuang::fnv_hash>
    requires std::is_enum_v<E> &&
    requires(Hash hash) { {hash(std::string_view{})} -> std::same_as<std::size_t>; }
class fields_enum
{
private:
    constexpr static auto enum_class_name = detail::get_class_name<E>();
    constexpr static std::size_t enum_count = detail::get_enum_count<E>();
    constexpr static auto name_array = detail::get_enum_name_array<E>();
    constexpr static auto enum_array = detail::get_enum_array<E>();
    constexpr static auto hash_to_enum_array = detail::get_hash_to_enum_array<E, Hash, enum_count>(name_array, enum_array);

public:
    using type = E;
    using underlying_type = std::underlying_type_t<E>;
    using hash_type = Hash;

    [[nodiscard]] consteval static auto size() noexcept { return enum_count; }
    [[nodiscard]] consteval static std::string_view name() noexcept { return enum_class_name; }

    [[nodiscard]] constexpr static E get(std::string_view name)
    {
        auto result = std::equal_range(hash_to_enum_array.cbegin(), hash_to_enum_array.cend(), Hash()(name));
        if (result.first == result.second)
            return E{};
        return result.first->val;
    }

    [[nodiscard]] constexpr static std::optional<E> get_if(std::string_view name)
    {
        auto result = std::equal_range(hash_to_enum_array.cbegin(), hash_to_enum_array.cend(), Hash()(name));
        if (result.first == result.second)
            return std::nullopt;
        return result.first->val;
    }

    [[nodiscard]] constexpr static bool exist(std::string_view name)
    {
        return std::binary_search(hash_to_enum_array.cbegin(), hash_to_enum_array.cend(), Hash()(name));
    }

    [[nodiscard]] constexpr static std::string_view get(E e)
    {
        auto res = std::find(enum_array.cbegin(), enum_array.cend(), e);
        if (res == enum_array.cend())
            return "";
        return name_array[res - enum_array.cbegin()];
    }

    [[nodiscard]] constexpr static std::optional<std::string_view> get_if(E e)
    {
        auto res = std::find(enum_array.cbegin(), enum_array.cend(), e);
        if (res == enum_array.cend())
            return std::nullopt;
        return name_array[res - enum_array.cbegin()];
    }
};

} // namespace bushuang
