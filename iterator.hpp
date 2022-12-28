#ifndef ITERATOR_HPP
#define ITERATOR_HPP

#include <iterator>
#include <type_traits>
/**
 * @brief Checks if the given type is an input iterator.
 *
 * @tparam T Type to check.
 */
template <typename T>
using is_input_iterator =
    std::is_convertible<typename std::iterator_traits<T>::iterator_category, std::input_iterator_tag>;

#endif
