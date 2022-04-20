#pragma once

#include <iostream>
#include <algorithm>
#include <string>

// print pair

template <typename T, typename U>
std::ostream& operator<<(std::ostream& stream, std::pair<T,U> p) {
    stream << "(" << p.first << "," << p.second << ")";
    return stream;
}

// member detection

template <typename, template<typename> typename, typename=void>
struct detect : std::false_type {};

template <typename T, template<typename> typename Check>
struct detect<T, Check, std::void_t<Check<T>>> : std::true_type {};

template <typename T>
using has_begin_impl = decltype(std::declval<T>().begin());

template <typename T>
using has_begin = detect<T,has_begin_impl>;

template <typename T>
using has_end_impl = decltype(std::declval<T>().end());

template <typename T>
using has_end = detect<T,has_end_impl>;

// check if the two type is same

template <typename, typename, typename=void>
struct is_same_t : std::false_type {};

template <typename T, typename U>
struct is_same_t<T,U,typename std::enable_if<std::is_same<T,U>::value>::type> : std::true_type {};

template <typename T>
using is_string = is_same_t<T,std::string>;

// enable if given requirements are all satisfied

template <typename T, typename... Check>
using conjunction_t = typename std::enable_if<std::conjunction<Check...>::value,T>::type;

// print container

template <typename Cont>
auto operator<<(std::ostream& stream, const Cont &c)
    -> conjunction_t<std::ostream&,has_begin<Cont>,has_end<Cont>,std::negation<is_string<Cont>>>
{
    for (auto x : c) {
        stream << x << " ";
    }
    return stream;
}

// print arbitrary number of values

template <typename T>
void print(T&& val) {
    std::cout << val << std::endl;
}

template <typename T, typename... Args>
void print(T&& val, Args&&... args) {
    std::cout << val << " ";
    print(std::forward<Args>(args)...);
}
