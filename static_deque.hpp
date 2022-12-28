#ifndef STATIC_DEQUE_HPP
#define STATIC_DEQUE_HPP

#include "iterator.hpp"
#include "memory.hpp"

/**
 * @brief Static double-ended queue (deque) without dynamic memory allocation.
 * @details @p std::deque defined in C++ standard library dynamically allocates its internal buffer. In contrast,
 * static_deque allocates the buffer statically as an array. Because of that, the capacity of the queue is fixed at the
 * compile time.
 *
 * The elements of static_deque are not stored contiguously; the following implementation uses a circular array so that
 * the elements are stored in 2 segments at most, which also indicates that dereferencing an element requires the modulo
 * operation of the index. The current implementation requires for the capacity to be a power of 2 in order not to
 * consider a modulo operation of a negative index and the performance issue.
 *
 * static_deque meets the requirements of Container, SequenceContainer, and ReversibleContainer. The complexities of
 * common operations are as follows:
 * @li Random access - constant
 * @li Insertion or removal at the beginning or at the end - constant
 * @li Insertion or removal at the middle - linear
 *
 * @tparam T Type of the elements.
 * @tparam N Capacity of the deque.
 *
 * @see [std::deque](https://en.cppreference.com/w/cpp/container/deque)
 * @see [Container](https://en.cppreference.com/w/cpp/named_req/Container)
 * @see [SequenceContainer](https://en.cppreference.com/w/cpp/named_req/SequenceContainer)
 * @see [ReversibleContainer](https://en.cppreference.com/w/cpp/named_req/ReversibleContainer)
 */
template <typename T, size_t N>
class static_deque
{
    static_assert(N > 0, "capacity must be greater than 0");
    static_assert((N & (N - 1)) == 0, "capacity must be a power of 2");

public:
    /** Iterator pointing to an element. */
    class iterator;
    /** Constant iterator pointing to an element. */
    class const_iterator;

    /* Requirements of Container. ------------------------------------------------------------------------------------*/

    /** Type of elements: @p T. */
    using value_type = T;
    /** Reference to @p T. */
    using reference = T &;
    /** Constant reference to @p T. */
    using const_reference = const T &;
    /** Signed integer type that can represent a difference of two iterators. */
    using difference_type = ptrdiff_t;
    /** Unsigned integer type that can represent all positive values of @p difference_type. */
    using size_type = size_t;

    /**
     * @brief Constructs the empty deque.
     * @complexity Constant.
     */
    static_deque(): head(this->arr, 0), tail(this->arr, 0) {}

    /**
     * @brief Constructs the deque with the copy of elements in @p a.
     * @complexity Linear.
     *
     * @param a Deque to copy from.
     */
    static_deque(const static_deque<T, N> &a): head(this->arr, 0), tail(this->arr, a.size())
    {
        std::uninitialized_copy(a.begin(), a.end(), this->head);
    }

    /**
     * @brief Constructs the deque with the elements in @p a using move semantics.
     * @complexity Linear.
     *
     * @param a Deque to move from.
     */
    static_deque(static_deque<T, N> &&a): head(this->arr, 0), tail(this->arr, a.size())
    {
        /* Move elements. */
        std::uninitialized_move(a.begin(), a.end(), this->head);

        /* Destorys the moved-from deque. */
        a.clear();
    }

    /**
     * @brief Replaces the contents with the copy of elements in @p a.
     * @complexity Linear.
     *
     * @param a Deque to copy from.
     * @return This deque.
     */
    static_deque<T, N> &operator=(const static_deque<T, N> &a)
    {
        if (this == &a)
            return *this;

        this->~static_deque();

        this->head = iterator(this->arr, 0);
        this->tail = iterator(this->arr, a.size());
        std::uninitialized_copy(a.begin(), a.end(), this->head);

        return *this;
    }

    /**
     * @brief Replaces the contents with the elements in @p a using move semantics.
     * @complexity Linear.
     *
     * @param a Deque to move from.
     * @return This queue.
     */
    static_deque<T, N> &operator=(static_deque<T, N> &&a)
    {
        if (this == &a)
            return *this;

        this->~static_deque();

        this->head = iterator(this->arr, 0);
        this->tail = iterator(this->arr, a.size());
        std::uninitialized_move(a.begin(), a.end(), this->head);

        return *this;
    }

    /**
     * @brief Destructs the queue calling destructors of the elements.
     * @complexity Linear.
     */
    ~static_deque()
    {
        std::destroy(this->head, this->tail);
    }

    /**
     * @brief Returns an iterator to the first element.
     * @details If the deque is empty, the returned iterator is equal to end().
     * @complexity Constant.
     *
     * @return Iterator to the first element.
     */
    iterator begin(void) noexcept
    {
        return this->head;
    }

    /**
     * @brief Returns a constant iterator to the first element.
     * @details If the deque is empty, the returned iterator is equal to end().
     * @complexity Constant.
     *
     * @return Constant iterator to the first element.
     */
    const_iterator begin(void) const noexcept
    {
        return this->head;
    }

    /**
     * @brief Returns a iterator to the past-the-last element.
     * @details The past-the-last element is a placeholder; any attempt to access it results in undefined behavior.
     * @complexity Constant.
     *
     * @return Iterator to the past-the-last element.
     */
    iterator end(void) noexcept
    {
        return this->tail;
    }

    /**
     * @brief Returns a constant iterator to the past-the-last element.
     * @details The past-the-last element is a placeholder; any attempt to access it results in undefined behavior.
     * @complexity Constant.
     *
     * @return Constant iterator to the past-the-last element.
     */
    const_iterator end(void) const noexcept
    {
        return this->tail;
    }

    /**
     * @brief Returns a constant iterator to the first element.
     * @details If the deque is empty, the returned iterator is equal to cend().
     * @complexity Constant.
     *
     * @return Constant iterator to the first element.
     */
    const_iterator cbegin(void) const noexcept
    {
        return this->head;
    }

    /**
     * @brief Returns a constant iterator to the past-the-last element.
     * @details The past-the-last element is a placeholder; any attempt to access it results in undefined behavior.
     * @complexity Constant.
     *
     * @return Constant iterator to the past-the-last element.
     */
    const_iterator cend(void) const noexcept
    {
        return this->tail;
    }

    /**
     * @brief Compares with the other deque @p a.
     * @complexity Linear.
     *
     * @param a Deque to compare with.
     * @return true if the deques are equal, false otherwise.
     */
    bool operator==(const static_deque<T, N> &a) const
    {
        return std::equal(this->head, this->tail, a.begin(), a.end());
    }

    /**
     * @brief Compares with the other deque @p a.
     * @complexity Linear.
     *
     * @param a Deque to compare with.
     * @return true if the deques are not equal, false otherwise.
     */
    bool operator!=(const static_deque<T, N> &a) const
    {
        return !(*this == a);
    }

    /**
     * @brief Swaps the contents with those of the other deque @p a.
     * @details Moves the contents of the deques using a temporary array. All iterators and references are invalidated.
     * @complexity Linear.
     *
     * @param[inout] a Deque to swap with.
     */
    void swap(static_deque<T, N> &a)
    {
        size_type old_size = this->size();

        /* Create temporary uninitialized storage. */
        alignas(alignof(T)) char raw[sizeof(T) * N];
        T *tmp = reinterpret_cast<T *>(raw);

        /* Swap elements. */
        std::uninitialized_move(this->head, this->tail, tmp);
        std::destroy(this->head, this->tail);

        this->head = iterator(this->arr, 0);
        this->tail = iterator(this->arr, a.size());
        std::uninitialized_move(a.begin(), a.end(), this->head);
        std::destroy(a.begin(), a.end());

        a.head = iterator(a.arr, 0);
        a.tail = iterator(a.arr, old_size);
        std::uninitialized_move(tmp, tmp + old_size, a.head);
        std::destroy(tmp, tmp + old_size);
    }

    /**
     * @brief Returns the number of elements in the deque.
     * @complexity Constant.
     *
     * @return Number of elements in the deque.
     */
    size_type size(void) const noexcept
    {
        return this->tail - this->head;
    }

    /**
     * @brief Returns the maximum number of elements the deque can hold.
     * @details Equals to the capacity @p N.
     * @complexity Constant.
     *
     * @return Maximum number of elements.
     */
    size_type max_size(void) const noexcept
    {
        return N;
    }

    /**
     * @brief Checks if the deque is empty.
     * @details Equivalent to `begin() == end()`.
     * @complexity Constant.
     *
     * @return true if the deque is empty, false otherwise.
     */
    bool empty(void) const noexcept
    {
        return this->head == this->tail;
    }

    /* Requirements of a sequence container. -------------------------------------------------------------------------*/

    /**
     * @brief Constructs the deque with @p n copies of elements with value @p t.
     * @complexity Linear.
     *
     * @param n Number of elements.
     * @param t Value of elements.
     *
     * @exception std::bad_alloc Not enough memory to store @p n elements.
     */
    static_deque(size_type n, const T &t): head(this->arr, 0), tail(this->arr, n)
    {
        if (static_cast<size_type>(this->tail.idx) > N)
            throw std::bad_alloc();

        std::uninitialized_fill(this->head, this->tail, t);
    }

    /**
     * @brief Constructs the deque with the contents of the range @p [i,j).
     * @complexity Linear.
     *
     * @tparam II Input iterator type.
     * @param i Start of the range.
     * @param j End of the range.
     *
     * @exception std::bad_alloc Not enough memory to store the all contents of the range.
     */
    template <typename II, std::enable_if_t<is_input_iterator<II>::value, bool> = true>
    static_deque(II i, II j): head(this->arr, 0), tail(this->arr, std::distance(i, j))
    {
        if (this->tail.idx < 0 || static_cast<size_type>(this->tail.idx) > N)
            throw std::bad_alloc();

        std::uninitialized_copy(i, j, this->head);
    }

    /**
     * @brief Constructs the queue with the contents of the initializer list @p il.
     * @complexity Linear.
     *
     * @param il Initializer list.
     *
     * @exception std::bad_alloc Not enough memory to store the all elements in the initializer list.
     */
    static_deque(std::initializer_list<T> il): head(this->arr, 0), tail(this->arr, il.size())
    {
        if (static_cast<size_type>(this->tail.idx) > N)
            throw std::bad_alloc();

        std::uninitialized_copy(il.begin(), il.end(), this->head);
    }

    /**
     * @brief Replaces the contents with the contents of the initializer list @p il.
     * @details All iterators and references are invalidated.
     * @complexity Linear.
     *
     * @param il Initializer list.
     * @return This deque.
     *
     * @exception std::bad_alloc Not enough memory to store the all elements in the initializer list.
     */
    static_deque &operator=(std::initializer_list<T> il)
    {
        if (il.size() > N)
            throw std::bad_alloc();

        this->~static_deque();
        this->head = iterator(this->arr, 0);
        this->tail = iterator(this->arr, il.size());
        std::copy(il.begin(), il.end(), this->head);
        return *this;
    }

    /**
     * @brief Inserts a new element directly before @p p.
     * @details All iterators and references are invalidated unless `p == begin()` or `p == end()`.
     * @complexity Linear.
     *
     * @tparam Args Types of arguments for the constructor of @p T.
     * @param p Constant iterator to the element.
     * @param args Arguments for the constructor of @p T.
     * @return Iterator to the inserted element.
     *
     * @exception std::bad_alloc Not enough memory to store the inserted element.
     * @exceptionsafety If an exception is thrown, the effects are unspecified.
     */
    template <typename... Args>
    iterator emplace(const_iterator p, Args &&...args)
    {
        if (this->size() > N - 1)
            throw std::bad_alloc();

        iterator q(this->arr, p.idx);

        if (q - this->head < this->tail - q)
        {
            /* Move elements. */
            iterator old_head = this->head;
            this->make_hole_forward(q, 1);

            /* Insert element. */
            if (q > old_head)
                *(q - 1) = T(std::forward<Args>(args)...);
            else
                construct_at(std::addressof(*(q - 1)), std::forward<Args>(args)...);

            return q - 1;
        }
        else
        {
            /* Move elements. */
            iterator old_tail = this->tail;
            this->make_hole_backward(q, 1);

            /* Insert element. */
            if (q < old_tail)
                *q = T(std::forward<Args>(args)...);
            else
                construct_at(std::addressof(*q), std::forward<Args>(args)...);

            return q;
        }
    }

    /**
     * @brief Inserts the value @p t directly before @p p.
     * @details All iterators and references are invalidated unless `p == begin()` or `p == end()`.
     * @complexity Linear.
     *
     * @param p Constant iterator to the element.
     * @param t Value to insert.
     * @return Iterator to the inserted element.
     *
     * @exception std::bad_alloc Not enough memory to store the inserted element.
     * @exceptionsafety If an exception is thrown, the effects are unspecified.
     */
    iterator insert(const_iterator p, const T &t)
    {
        if (this->size() > N - 1)
            throw std::bad_alloc();

        iterator q(this->arr, p.idx);

        if (q - this->head < this->tail - q)
        {
            /* Move elements. */
            iterator old_head = this->head;
            this->make_hole_forward(q, 1);

            /* Insert element. */
            if (q > old_head)
                *(q - 1) = t;
            else
                std::uninitialized_fill(q - 1, q, t);

            return q - 1;
        }
        else
        {
            /* Move elements. */
            iterator old_tail = this->tail;
            this->make_hole_backward(q, 1);

            /* Copy element. */
            if (q < old_tail)
                *q = t;
            else
                std::uninitialized_fill(q, q + 1, t);

            return q;
        }
    }

    /**
     * @brief Inserts the value @p t directly before @p p using move semantics.
     * @details All iterators and references are invalidated unless `p == begin()` or `p == end()`.
     * @complexity Linear.
     *
     * @param p Constant iterator to the element.
     * @param t Value to insert.
     * @return Iterator to the inserted element.
     *
     * @exception std::bad_alloc Not enough memory to store the inserted element.
     * @exceptionsafety If an exception is thrown, the effects are unspecified.
     */
    iterator insert(const_iterator p, T &&t)
    {
        if (this->size() > N - 1)
            throw std::bad_alloc();

        iterator q(this->arr, p.idx);

        if (q - this->head < this->tail - q)
        {
            /* Move elements. */
            iterator old_head = this->head;
            this->make_hole_forward(q, 1);

            /* Move element. */
            if (q > old_head)
                *(q - 1) = std::move(t);
            else
                std::uninitialized_move(std::addressof(t), std::addressof(t) + 1, q - 1);

            return q - 1;
        }
        else
        {
            /* Move elements. */
            iterator old_tail = this->tail;
            this->make_hole_backward(q, 1);

            /* Move element. */
            if (q < old_tail)
                *q = std::move(t);
            else
                std::uninitialized_move(std::addressof(t), std::addressof(t) + 1, q);

            return q;
        }
    }

    /**
     * @brief Inserts @p n copies of the value @p t directly before @p p using move semantics.
     * @details All iterators and references are invalidated unless `p == begin()` or `p == end()`.
     * @complexity Linear.
     *
     * @param p Constant iterator to the element.
     * @param n Number of elements to insert.
     * @param t Value to insert.
     * @return Iterator to the first element inserted.
     *
     * @exception std::bad_alloc Not enough memory to store the inserted elements.
     * @exceptionsafety If an exception is thrown, the effects are unspecified.
     */
    iterator insert(const_iterator p, size_type n, const T &t)
    {
        if (this->size() > N - n)
            throw std::bad_alloc();

        iterator q(this->arr, p.idx);

        if (q - this->head < this->tail - q)
        {
            /* Move elements. */
            iterator old_head = this->head;
            this->make_hole_forward(q, n);

            /* Copy elements. */
            if (q - old_head >= static_cast<difference_type>(n))
                std::fill(q - n, q, t);
            else
            {
                std::fill(old_head, q, t);
                std::uninitialized_fill(q - n, old_head, t);
            }

            return q - n;
        }
        else
        {
            /* Move elements. */
            iterator old_tail = this->tail;
            this->make_hole_backward(q, n);

            /* Copy elements. */
            if (old_tail - q >= static_cast<difference_type>(n))
                std::fill(q, q + n, t);
            else
            {
                std::fill(q, old_tail, t);
                std::uninitialized_fill(old_tail, q + n, t);
            }

            return q;
        }
    }

    /**
     * @brief Inserts the contents of the range @p [i,j) directly before @p p.
     * @details All iterators and references are invalidated unless `p == begin()` or `p == end()`.
     * @complexity Linear.
     *
     * @tparam II Input iterator type.
     * @param p Constant iterator to the element.
     * @param i Start of the range.
     * @param j End of the range.
     * @return Iterator to the first element inserted.
     *
     * @exception std::bad_alloc Not enough memory to store the inserted elements.
     * @exceptionsafety If an exception is thrown, the effects are unspecified.
     */
    template <typename II, std::enable_if_t<is_input_iterator<II>::value, bool> = true>
    iterator insert(const_iterator p, II i, II j)
    {
        difference_type n = std::distance(i, j);

        if (n < 0 || this->size() > N - n)
            throw std::bad_alloc();

        iterator q(this->arr, p.idx);

        if (q - this->head < this->tail - q)
        {
            iterator old_head = this->head;
            this->make_hole_forward(q, n);

            /* Copy elements. */
            if (q - old_head >= static_cast<difference_type>(n))
                std::copy(i, j, q - n);
            else
            {
                II m = std::next(j, old_head - q);
                std::copy(m, j, old_head);
                std::uninitialized_copy(i, m, q - n);
            }

            return q - n;
        }
        else
        {
            /* Move elements. */
            iterator old_tail = this->tail;
            this->make_hole_backward(q, n);

            /* Copy elements. */
            if (old_tail - q >= static_cast<difference_type>(n))
                std::copy(i, j, q);
            else
            {
                II m = std::next(i, old_tail - q);
                std::copy(i, m, q);
                std::uninitialized_copy(m, j, old_tail);
            }

            return q;
        }
    }

    /**
     * @brief Inserts the contents of the initializer list @p il directly before @p p.
     * @details All iterators and references are invalidated unless `p == begin()` or `p == end()`.
     * @complexity Linear.
     *
     * @param p Constant iterator to the element.
     * @param il Initializer list.
     * @return Iterator to the first element inserted.
     *
     * @exception std::bad_alloc Not enough memory to store the inserted elements.
     * @exceptionsafety If an exception is thrown, the effects are unspecified.
     */
    iterator insert(const_iterator p, std::initializer_list<T> il)
    {
        if (this->size() > N - il.size())
            throw std::bad_alloc();

        return this->insert(p, il.begin(), il.end());
    }

    /**
     * @brief Removes the element at @p q.
     * @details All iterators and references are invalidated, unless `q == begin()` or `q == end()`, in which case only
     * the iterators and references to the removed element are invalidated. The past-the-last iterator is always
     * invalidated.
     *
     * If the iterator @p q is not valid nor dereferenceable, the behavior is undefined.
     * @complexity Linear.
     *
     * @param q Constant iterator to the element.
     * @return Iterator to the element following the last removed element.
     *
     * @exceptionsafety If an exception is thrown, the effects are unspecified.
     *
     * @see clear()
     */
    iterator erase(const_iterator q)
    {
        /* Destruct element. */
        iterator p(this->arr, q.idx);
        std::destroy(p, p + 1);

        /* Move elements. */
        if (p - this->head < this->tail - p)
        {
            this->remove_hole_backward(p, 1);
            return p + 1;
        }
        else
        {
            this->remove_hole_forward(p + 1, 1);
            return p;
        }
    }

    /**
     * @brief Removes the elements in the range @p [q1,q2).
     * @details All iterators and references are invalidated, unless `q1 == begin()` or `q2 == end()`, in which case
     * only the iterators and references to the removed elements are invalidated. The past-the-last iterator is always
     * invalidated.
     *
     * If all iterators to the range @p [q1,q2) are not valid nor dereferenceable, the behavior is undefined. Removing
     * an empty range does nothing; the iterator @p q1 need not be dereferenceable.
     * @complexity Linear.
     *
     * @param q1 Start of the range.
     * @param q2 End of the range.
     * @return Iterator to the element following the last removed element.
     *
     * @exceptionsafety If an exception is thrown, the effects are unspecified.
     *
     * @see clear()
     */
    iterator erase(const_iterator q1, const_iterator q2)
    {
        /* Destruct elements. */
        iterator p1(this->arr, q1.idx), p2(this->arr, q2.idx);
        difference_type n = p2 - p1;
        std::destroy(p1, p2);

        /* Move elements. */
        if (p1 - this->head < this->tail - p2)
        {
            this->remove_hole_backward(p1, n);
            return p2;
        }
        else
        {
            this->remove_hole_forward(p2, n);
            return p1;
        }
    }

    /**
     * @brief Removes all elements in the deque.
     * @details All iterators, references, and pointers are invalidated.
     * @complexity Linear.
     */
    void clear(void) noexcept
    {
        std::destroy(this->head, this->tail);
        this->head = this->tail = iterator(this->arr, 0);
    }

    /**
     * @brief Replaces the contents with the contents of the range @p [i,j).
     * @details All iterators, references, and pointers are invalidated.
     * @complexity Linear.
     *
     * @tparam II Input iterator type.
     * @param i Start of the range.
     * @param j End of the range.
     *
     * @exception std::bad_alloc Not enough memory to store the all contents of the range.
     */
    template <typename II, std::enable_if_t<is_input_iterator<II>::value, bool> = true>
    void assign(II i, II j)
    {
        difference_type n = std::distance(i, j);
        if (n < 0 || static_cast<size_type>(n) > N)
            throw std::bad_alloc();

        std::destroy(this->head, this->tail);
        this->head = iterator(this->arr, 0);
        this->tail = iterator(this->arr, n);
        std::uninitialized_copy(i, j, this->head);
    }

    /**
     * @brief Replaces the contents with the contents of the initializer list @p il.
     * @details All iterators, references, and pointers are invalidated.
     * @complexity Linear.
     *
     * @param il Initializer list.
     *
     * @exception std::bad_alloc Not enough memory to store the all contents in the initializer list.
     */
    void assign(std::initializer_list<T> il)
    {
        if (il.size() > N)
            throw std::bad_alloc();

        this->assign(il.begin(), il.end());
    }

    /**
     * @brief Replaces the contents with @p n copies of the value @p t.
     * @details All iterators, references, and pointers are invalidated.
     * @complexity Linear.
     *
     * @param n Number of elements.
     * @param t Value to replace with.
     *
     * @exception std::bad_alloc Not enough memory to store @p n elements.
     */
    void assign(size_type n, const T &t)
    {
        if (n > N)
            throw std::bad_alloc();

        std::destroy(this->head, this->tail);
        this->head = iterator(this->arr, 0);
        this->tail = iterator(this->arr, n);
        std::uninitialized_fill(this->head, this->tail, t);
    }

    /* Requirements of a reversible container. -----------------------------------------------------------------------*/

    /** Reverse iterator pointing to an element. */
    using reverse_iterator = std::reverse_iterator<iterator>;
    /** Constant reverse iterator pointing to an element. */
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    /**
     * @brief Returns a reverse iterator to the first element of the reversed deque.
     * @details If the deque is empty, the returned iterator is equal to `rend()`.
     *
     * @return Reverse iterator to the first element of the reversed deque.
     */
    reverse_iterator rbegin(void) noexcept
    {
        return reverse_iterator(this->tail);
    }

    /**
     * @brief Returns a constant reverse iterator to the first element of the reversed deque.
     * @details If the deque is empty, the returned iterator is equal to `rend()`.
     *
     * @return Constant reverse iterator to the first element of the reversed deque.
     */
    const_reverse_iterator rbegin(void) const noexcept
    {
        return const_reverse_iterator(this->tail);
    }

    /**
     * @brief Returns a reverse iterator to the past-the-last element of the reversed deque.
     * @details The past-the-last element is a placeholder; any attempt to access it results in undefined behavior.
     *
     * @return Reverse iterator to the past-the-last element of the reversed deque.
     */
    reverse_iterator rend(void) noexcept
    {
        return reverse_iterator(this->head);
    }

    /**
     * @brief Returns a constant reverse iterator to the past-the-last element of the reversed deque.
     * @details The past-the-last element is a placeholder; any attempt to access it results in undefined behavior.
     *
     * @return Constant reverse iterator to the past-the-last element of the reversed deque.
     */
    const_reverse_iterator rend(void) const noexcept
    {
        return const_reverse_iterator(this->head);
    }

    /**
     * @brief Returns a constant reverse iterator to the first element of the reversed deque.
     * @details If the deque is empty, the returned iterator is equal to `rbegin()`.
     *
     * @return Constant reverse iterator to the first element of the reversed deque.
     */
    const_reverse_iterator crbegin(void) const noexcept
    {
        return const_reverse_iterator(this->tail);
    }

    /**
     * @brief Returns a constant reverse iterator to the past-the-last element of the reversed deque.
     * @details The past-the-last element is a placeholder; any attempt to access it results in undefined behavior.
     *
     * @return Constant reverse iterator to the past-the-last element of the reversed deque.
     */
    const_reverse_iterator crend(void) const noexcept
    {
        return const_reverse_iterator(this->head);
    }

    /* Other operations implemented in std::deque. -------------------------------------------------------------------*/

    /**
     * @brief Returns a reference to the first element.
     * @details If the deque is empty, the behavior is undefined.
     * @complexity Constant.
     *
     * @return Reference to the first element.
     */
    T &front(void)
    {
        return *this->head;
    }

    /**
     * @brief Returns a constant reference to the first element.
     * @details If the deque is empty, the behavior is undefined.
     * @complexity Constant.
     *
     * @return Constant reference to the first element.
     */
    const T &front(void) const
    {
        return *this->head;
    }

    /**
     * @brief Returns a reference to the last element.
     * @details If the queue is empty, the behavior is undefined.
     * @complexity Constant.
     *
     * @return Reference to the last element.
     */
    T &back(void)
    {
        return *(this->tail - 1);
    }

    /**
     * @brief Returns a constant reference to the last element.
     * @details If the queue is empty, the behavior is undefined.
     * @complexity Constant.
     *
     * @return Constant reference to the last element.
     */
    const T &back(void) const
    {
        return *(this->tail - 1);
    }

    /**
     * @brief Inserts a new element at the front of the deque.
     * @details All iterators are invalidated. References are not affected.
     * @complexity Constant.
     *
     * @tparam Args Types of the arguments for the constructor of @p T.
     * @param args Arguments for the constructor of @p T.
     *
     * @exception std::bad_alloc Not enough memory to store the inserted element.
     * @exceptionsafety If an exception is thrown, the effects are unspecified.
     */
    template <typename... Args>
    void emplace_front(Args &&...args)
    {
        if (this->size() > N - 1)
            throw std::bad_alloc();

        --this->head;
        construct_at(std::addressof(*this->head), std::forward<Args>(args)...);
    }

    /**
     * @brief Inserts a new element at the end of the deque.
     * @details All iterators are invalidated. References are not affected.
     * @complexity Constant.
     *
     * @tparam Args Types of the arguments for the constructor of @p T.
     * @param args Arguments for the constructor of @p T.
     *
     * @exception std::bad_alloc Not enough memory to store the inserted element.
     * @exceptionsafety If an exception is thrown, the effects are unspecified.
     */
    template <typename... Args>
    void emplace_back(Args &&...args)
    {
        if (this->size() > N - 1)
            throw std::bad_alloc();

        construct_at(std::addressof(*this->tail), std::forward<Args>(args)...);
        ++this->tail;
    }

    /**
     * @brief Inserts a new element at the front of the deque.
     * @details All iterators are invalidated. References are not affected.
     * @complexity Constant.
     *
     * @param t Element to insert.
     *
     * @exception std::bad_alloc Not enough memory to store the inserted element.
     * @exceptionsafety If an exception is thrown, the effects are unspecified.
     */
    void push_front(const T &t)
    {
        if (this->size() > N - 1)
            throw std::bad_alloc();

        --this->head;
        construct_at(std::addressof(*this->head), t);
    }

    /**
     * @brief Inserts a new element at the front of the deque.
     * @details All iterators are invalidated. References are not affected.
     * @complexity Constant.
     *
     * @param t Element to insert.
     *
     * @exception std::bad_alloc Not enough memory to store the inserted element.
     * @exceptionsafety If an exception is thrown, the effects are unspecified.
     */
    void push_front(T &&t)
    {
        if (this->size() > N - 1)
            throw std::bad_alloc();

        --this->head;
        construct_at(std::addressof(*this->head), std::move(t));
    }

    /**
     * @brief Inserts a new element at the end of the deque.
     * @details All iterators are invalidated. References are not affected.
     * @complexity Constant.
     *
     * @param t Element to insert.
     *
     * @exception std::bad_alloc Not enough memory to store the inserted element.
     * @exceptionsafety If an exception is thrown, the effects are unspecified.
     */
    void push_back(const T &t)
    {
        if (this->size() > N - 1)
            throw std::bad_alloc();

        construct_at(std::addressof(*this->tail), t);
        ++this->tail;
    }

    /**
     * @brief Inserts a new element at the end of the deque.
     * @details All iterators are invalidated. References are not affected.
     * @complexity Constant.
     *
     * @param t Element to insert.
     *
     * @exception std::bad_alloc Not enough memory to store the inserted element.
     * @exceptionsafety If an exception is thrown, the effects are unspecified.
     */
    void push_back(T &&t)
    {
        if (this->size() > N - 1)
            throw std::bad_alloc();

        construct_at(std::addressof(*this->tail), std::move(t));
        ++this->tail;
    }

    /**
     * @brief Removes the first element.
     * @details Iterators and references to the removed element are invalidated. Other iterators and references are not
     * affected. If the deque is empty, the behavior is undefined.
     * @complexity Constant.
     */
    void pop_front(void)
    {
        std::destroy_at(std::addressof(*this->head));
        ++this->head;
    }

    /**
     * @brief Removes the last element.
     * @details Iterators and references to the removed element are invalidated. The past-the-last iterator is also
     * invalidated. Other iterators and references are not affected. If the deque is empty, the behavior is undefined.
     * @complexity Constant.
     */
    void pop_back(void)
    {
        --this->tail;
        std::destroy_at(std::addressof(*this->tail));
    }

    /**
     * @brief Returns a reference to the element at the index.
     * @details No bounds check. The behavior is undefined if @p n is out of range.
     *
     * @param n Index.
     * @return Reference to the element.
     */
    T &operator[](size_type n)
    {
        return *(this->head + n);
    }

    /**
     * @brief Returns a constant reference to the element at the index.
     * @details No bounds check. The behavior is undefined if @p n is out of range.
     *
     * @param n Index.
     * @return Constant reference to the element.
     */
    const T &operator[](size_type n) const
    {
        return *(this->head + n);
    }

    /**
     * @brief Returns a reference to the element at the index.
     * @complexity Constant.
     *
     * @param n Index.
     * @return Reference to the element.
     *
     * @exception std::out_of_range Index is out of range.
     */
    T &at(size_type n)
    {
        if (n >= this->size())
            throw std::out_of_range("static_deque::at: n (which is " + std::to_string(n) +
                                    ") >= this->size() (which is " + std::to_string(this->size()) + ")");

        return *(this->head + n);
    }

    /**
     * @brief Returns a constant reference to the element at the index.
     * @complexity Constant.
     *
     * @param n Index.
     * @return Constant reference to the element.
     *
     * @exception std::out_of_range Index is out of range.
     */
    const T &at(size_type n) const
    {
        if (n >= this->size())
            throw std::out_of_range("static_deque::at: n (which is " + std::to_string(n) +
                                    ") >= this->size() (which is " + std::to_string(this->size()) + ")");

        return *(this->head + n);
    }

private:
    /**
     * @brief Moves elements in range @p [head,p) forward to make a hole in the range @p [p-n,p).
     *
     * @param p Past-the-last element to move.
     * @param n Distance to move forward.
     */
    void make_hole_forward(iterator p, size_type n)
    {
        if (p - this->head <= static_cast<difference_type>(n))
            std::uninitialized_move(this->head, p, this->head - n);
        else
        {
            std::uninitialized_move(this->head, this->head + n, this->head - n);
            std::move(this->head + n, p, this->head);
        }
        this->head -= n;
    }

    /**
     * @brief Moves elements in range @p [p,tail) backward to make a hole in the range @p [p,p+n).
     *
     * @param p First element to move.
     * @param n Distance to move backward.
     */
    void make_hole_backward(iterator p, size_type n)
    {
        if (this->tail - p <= static_cast<difference_type>(n))
            std::uninitialized_move(p, this->tail, p + n);
        else
        {
            std::uninitialized_move(this->tail - n, this->tail, this->tail);
            std::move_backward(p, this->tail - n, this->tail);
        }
        this->tail += n;
    }

    /**
     * @brief Moves elements in range @p [p,tail) forward to remove a hole in the range @p [p-n,p).
     *
     * @param p First element to move.
     * @param n Distance to move forward.
     */
    void remove_hole_forward(iterator p, size_type n)
    {
        if (this->tail - p <= static_cast<difference_type>(n))
        {
            std::uninitialized_move(p, this->tail, p - n);
            std::destroy(p, this->tail);
        }
        else
        {
            std::uninitialized_move(p, p + n, p - n);
            std::move(p + n, this->tail, p);
            std::destroy(this->tail - n, this->tail);
        }
        this->tail -= n;
    }

    /**
     * @brief Moves elements in range @p [head,p) backward to remove a hole in the range @p [p,p+n).
     *
     * @param p Past-the-last element to move.
     * @param n Distance to move backward.
     */
    void remove_hole_backward(iterator p, size_type n)
    {
        if (p - head <= static_cast<difference_type>(n))
        {
            std::uninitialized_move(this->head, p, this->head + n);
            std::destroy(this->head, p);
        }
        else
        {
            std::uninitialized_move(p - n, p, p);
            std::move_backward(this->head, p - n, p);
            std::destroy(this->head, this->head + n);
        }
        this->head += n;
    }

    /** Internal buffer to store elements. */
    alignas(alignof(T)) char raw[sizeof(T) * N];
    /** Type-casted buffer. */
    T *arr = reinterpret_cast<T *>(raw);
    /** Iterator to the first element. */
    iterator head;
    /** Iterator to the past-the-last element. */
    iterator tail;
};

template <typename T, size_t N>
class static_deque<T, N>::iterator
{
public:
    using difference_type = ptrdiff_t;
    using value_type = T;
    using pointer = T *;
    using reference = T &;
    using iterator_category = std::random_access_iterator_tag;

    iterator() noexcept: arr(nullptr), idx(0) {}
    T &operator*() const
    {
        return this->arr[this->idx % N];
    }
    T *operator->() const
    {
        return this->arr + this->idx % N;
    }
    iterator &operator++()
    {
        this->idx++;
        return *this;
    }
    iterator operator++(int)
    {
        iterator tmp(*this);
        this->idx++;
        return tmp;
    }
    iterator &operator--()
    {
        this->idx--;
        return *this;
    }
    iterator operator--(int)
    {
        iterator tmp(*this);
        this->idx--;
        return tmp;
    }
    iterator &operator+=(difference_type n)
    {
        this->idx += n;
        return *this;
    }
    iterator &operator-=(difference_type n)
    {
        this->idx -= n;
        return *this;
    }
    iterator operator+(difference_type n) const
    {
        iterator tmp(*this);
        tmp.idx += n;
        return tmp;
    }
    friend iterator operator+(difference_type n, const iterator &it)
    {
        return it + n;
    }
    iterator operator-(difference_type n) const
    {
        iterator tmp(*this);
        tmp.idx -= n;
        return tmp;
    }
    difference_type operator-(iterator a) const
    {
        return this->idx - a.idx;
    }
    T &operator[](difference_type n) const
    {
        return this->arr[(this->idx + n) % N];
    }
    bool operator==(iterator a) const
    {
        return this->idx == a.idx;
    }
    bool operator!=(iterator a) const
    {
        return this->idx != a.idx;
    }
    bool operator<(iterator a) const
    {
        return this->idx < a.idx;
    }
    bool operator>(iterator a) const
    {
        return this->idx > a.idx;
    }
    bool operator<=(iterator a) const
    {
        return this->idx <= a.idx;
    }
    bool operator>=(iterator a) const
    {
        return this->idx >= a.idx;
    }

private:
    iterator(T *arr, difference_type idx): arr(arr), idx(idx) {}
    T *arr;
    difference_type idx;

    friend class static_deque<T, N>;
    friend class static_deque<T, N>::const_iterator;
};

template <typename T, size_t N>
class static_deque<T, N>::const_iterator
{
public:
    using difference_type = ptrdiff_t;
    using value_type = T;
    using pointer = const T *;
    using reference = const T &;
    using iterator_category = std::random_access_iterator_tag;

    const_iterator() noexcept: arr(nullptr), idx(0) {}
    /* A constant iterator is able to implicitly converted from an iterator. */
    const_iterator(const iterator &a) noexcept: arr(a.arr), idx(a.idx) {}
    const T &operator*() const
    {
        return this->arr[this->idx % N];
    }
    const T *operator->() const
    {
        return this->arr + this->idx % N;
    }
    const_iterator &operator++()
    {
        this->idx++;
        return *this;
    }
    const_iterator operator++(int)
    {
        const_iterator tmp(*this);
        this->idx++;
        return tmp;
    }
    const_iterator &operator--()
    {
        this->idx--;
        return *this;
    }
    const_iterator operator--(int)
    {
        const_iterator tmp(*this);
        this->idx--;
        return tmp;
    }
    const_iterator &operator+=(difference_type n)
    {
        this->idx += n;
        return *this;
    }
    const_iterator &operator-=(difference_type n)
    {
        this->idx -= n;
        return *this;
    }
    const_iterator operator+(difference_type n) const
    {
        const_iterator tmp(*this);
        tmp.idx += n;
        return tmp;
    }
    friend const_iterator operator+(difference_type n, const const_iterator &it)
    {
        return it + n;
    }
    const_iterator operator-(difference_type n) const
    {
        const_iterator tmp(*this);
        tmp.idx -= n;
        return tmp;
    }
    difference_type operator-(const_iterator a) const
    {
        return this->idx - a.idx;
    }
    const T &operator[](difference_type n) const
    {
        return this->arr[(this->idx + n) % N];
    }
    bool operator==(const_iterator a) const
    {
        return this->idx == a.idx;
    }
    bool operator!=(const_iterator a) const
    {
        return this->idx != a.idx;
    }
    bool operator<(const_iterator a) const
    {
        return this->idx < a.idx;
    }
    bool operator>(const_iterator a) const
    {
        return this->idx > a.idx;
    }
    bool operator<=(const_iterator a) const
    {
        return this->idx <= a.idx;
    }
    bool operator>=(const_iterator a) const
    {
        return this->idx >= a.idx;
    }

private:
    const_iterator(const T *arr, difference_type idx): arr(arr), idx(idx) {}
    const T *arr;
    difference_type idx;

    friend class static_deque<T, N>;
};

#endif
