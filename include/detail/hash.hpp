#pragma once

#include <concepts>
#include <cstdint>
#include <utility>

namespace bushuang
{

namespace detail
{
template <typename T>
concept remove_reference_integral = std::is_integral_v<std::remove_reference_t<T>>;

template <typename Cont>
concept container = requires (Cont cont)
{
    requires std::integral<typename std::decay_t<Cont>::value_type>;
    { cont[std::size_t{}] }     -> remove_reference_integral;
    { cont.size() }             -> std::same_as<std::size_t>;
};
} // detail


class fnv_hash
{
private:
    constexpr static auto fnv_offset_basis = 14695981039346656037ull;
    constexpr static auto fnv_prime = 1099511628211ull;

    template <typename Cont>
    constexpr std::uint64_t calc(std::uint64_t hash, Cont&& cont) noexcept
    {
        for (std::size_t i = 0; i < cont.size(); i++)
        {
            hash = hash ^ static_cast<std::size_t>(cont[i]);
            hash = hash * fnv_prime;
        }
        return hash;
    }

public:
    template <typename Cont>
        requires detail::container<Cont>
    constexpr std::uint64_t operator()(Cont&& cont) noexcept
    {
        return calc(fnv_offset_basis, std::forward<Cont>(cont));
    }
};

} // bushuang
