#ifndef MEMORY_HPP
#define MEMORY_HPP

#include <memory>

/**
 * @brief Constructs an object at the given location.
 * @details This function does same thing as @p std::construct_at() in C++20 standard, which is equivalent to
 * @code{.cpp}
 * return ::new (const_cast<void*>(static_cast<const volatile void*>(p))) T(std::forward<Args>(args)...);
 * @endcode
 * However, casting a pointer type to @p void* (sometimes called "voidify") is differently defined in C++17 standard
 * library:
 * @code{.cpp}
 * static_cast<void *>(p)                                    // until C++20
 * const_cast<void *>(static_cast<const volatile void*>(p))  // since C++20
 * @endcode
 * For more informations about "voidify", see the definition of @p std::uninitialized_copy().
 *
 * @tparam T Type of the object.
 * @tparam Args Types of the constructor arguments.
 * @param p Pointer to the location where the object is constructed.
 * @param args Constructor arguments.
 * @return Pointer to the object.
 *
 * @see [std::construct_at()](https://en.cppreference.com/w/cpp/memory/construct_at)
 * @see [std::uninitialized_copy()](https://en.cppreference.com/w/cpp/memory/uninitialized_copy)
 */
template <typename T, typename... Args>
inline T *construct_at(T *p, Args &&...args)
{
    return ::new (static_cast<void *>(p)) T(std::forward<Args>(args)...);
}

#endif
