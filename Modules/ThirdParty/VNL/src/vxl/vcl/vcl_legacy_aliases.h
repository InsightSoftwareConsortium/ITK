#ifndef VCL_LEGACY_ALIASES_H_
#define VCL_LEGACY_ALIASES_H_

#if !defined(VXL_LEGACY_FUTURE_REMOVE)
# define VXL_CONSTEXPR_FUNC constexpr  //constexpr in C++11, empty in C++98
# define VXL_CONSTEXPR_VAR  constexpr  //constexpr in C++11, empty in C++98
# define VXL_FULLCXX11SUPPORT 1
# define VCL_STATIC_CONST_INIT_FLOAT_DECL(x) = x
# define VCL_STATIC_CONST_INIT_FLOAT_DEFN(x) /* initialized at declaration */
#endif

/*
 * These defines need to be here for backwards
 * compatibility in external packages.
 * External packages previously often
 * had transitive dependancies from the
 * vnl_* include files that included
 * vcl_* header files.  Since these
 * dependancies are removed (i.e. vnl_*
 * headers now include <cmath> instead
 * of <vcl_cmath.h>, the defines need
 * to be provided here.
 *
 * This mechanism ONLY addresses the problem
 * of transitive includes, and relies on the
 * assumption that the vnl_* headers provide
 * the proper std header files (i.e. <cmath>)
 * that declare the std::* functions.
 * The std header files are NOT included
 * here in an attempt to minimize include file
 * explosion.
 *
 */
// NOTE: VXL_LEGACY_FUTURE_REMOVE is an advanced developer
//       feature that can only be activated by explicitly
//       using a compiler flag like "-DVXL_LEGACY_FUTURE_REMOVE"
#define vcl_adjacent_find std::adjacent_find
#define vcl_and std::and
#define vcl_binary std::binary
#define vcl_binary_search std::binary_search
#define vcl_copy std::copy
#define vcl_copy_ std::copy_
#define vcl_count std::count
#define vcl_count_if std::count_if
#define vcl_equal std::equal
#define vcl_equal_range std::equal_range
#define vcl_fill std::fill
#define vcl_fill_n std::fill_n
#define vcl_find std::find
#define vcl_find_end std::find_end
#define vcl_find_first_of std::find_first_of
#define vcl_find_if std::find_if
#define vcl_for_each std::for_each
#define vcl_generate std::generate
#define vcl_generate_n std::generate_n
#define vcl_generators_ std::generators_
#define vcl_heap std::heap
#define vcl_includes std::includes
#define vcl_inplace_merge std::inplace_merge
#define vcl_iter_swap std::iter_swap
#define vcl_lexicographical_compare std::lexicographical_compare
#define vcl_lower_bound std::lower_bound
#define vcl_make_heap std::make_heap
#define vcl_max std::max
#define vcl_min std::min
#define vcl_max_element std::max_element
#define vcl_merge std::merge
#define vcl_merge_ std::merge_
#define vcl_min_element std::min_element
#define vcl_mismatch std::mismatch
#define vcl_next_permutation std::next_permutation
#define vcl_nth_element std::nth_element
#define vcl_partial_sort std::partial_sort
#define vcl_partial_sort_copy std::partial_sort_copy
#define vcl_partition std::partition
#define vcl_stable_partition std::stable_partition
#define vcl_partitions_ std::partitions_
#define vcl_pop_heap std::pop_heap
#define vcl_prev_permutation std::prev_permutation
#define vcl_push_heap std::push_heap
#define vcl_random_shuffle std::random_shuffle
#define vcl_remove std::remove
#define vcl_remove_copy std::remove_copy
#define vcl_remove_copy_if std::remove_copy_if
#define vcl_remove_if std::remove_if
#define vcl_replace std::replace
#define vcl_replace_copy std::replace_copy
#define vcl_replace_copy_if std::replace_copy_if
#define vcl_replace_if std::replace_if
#define vcl_reverse std::reverse
#define vcl_reverse_copy std::reverse_copy
#define vcl_rotate std::rotate
#define vcl_rotate_copy std::rotate_copy
#define vcl_search std::search
#define vcl_search_n std::search_n
#define vcl_set_difference std::set_difference
#define vcl_set_intersection std::set_intersection
#define vcl_set_symmetric_difference std::set_symmetric_difference
#define vcl_set_union std::set_union
#define vcl_sort std::sort
#define vcl_sort_ std::sort_
#define vcl_sort_heap std::sort_heap
#define vcl_stable_sort std::stable_sort
#define vcl_swap std::swap
#define vcl_swap_ std::swap_
#define vcl_swap_ranges std::swap_ranges
#define vcl_transform std::transform
#define vcl_unique std::unique
#define vcl_unique_copy std::unique_copy
#define vcl_upper_bound std::upper_bound
#define vcl_bitset std::bitset
#define vcl_abs std::abs
#define vcl_acos std::acos
#define vcl_asin std::asin
#define vcl_atan std::atan
#define vcl_atan2 std::atan2
#define vcl_ceil std::ceil
#define vcl_cos std::cos
#define vcl_cosh std::cosh
#define vcl_exp std::exp
#define vcl_fabs std::fabs
#define vcl_floor std::floor
#define vcl_fmod std::fmod
#define vcl_frexp std::frexp
#define vcl_ldexp std::ldexp
#define vcl_log std::log
#define vcl_log10 std::log10
#define vcl_modf std::modf
#define vcl_pow std::pow
#define vcl_sin std::sin
#define vcl_sinh std::sinh
#define vcl_sqrt std::sqrt
#define vcl_tan std::tan
#define vcl_tanh std::tanh
#define vcl_complex std::complex
#define vcl_real std::real
#define vcl_imag std::imag
#define vcl_arg std::arg
#define vcl_norm std::norm
#define vcl_conj std::conj
#define vcl_polar std::polar
#define vcl_jmp_buf std::jmp_buf
#define vcl_longjmp std::longjmp
#define vcl_sig_atomic_t std::sig_atomic_t
#define vcl_raise std::raise
#define vcl_signal std::signal
#define vcl_va_list std::va_list
#define vcl_ptrdiff_t std::ptrdiff_t
#define vcl_size_t std::size_t
#define vcl_FILE std::FILE
#define vcl_fpos_t std::fpos_t
#define vcl_fopen std::fopen
#define vcl_fclose std::fclose
#define vcl_feof std::feof
#define vcl_ferror std::ferror
#define vcl_fflush std::fflush
#define vcl_fgetc std::fgetc
#define vcl_fgetpos std::fgetpos
#define vcl_fgets std::fgets
#define vcl_fwrite std::fwrite
#define vcl_fread std::fread
#define vcl_fseek std::fseek
#define vcl_ftell std::ftell
#define vcl_perror std::perror
#define vcl_clearerr std::clearerr
#define vcl_rename std::rename
#define vcl_fputc std::fputc
#define vcl_fputs std::fputs
#define vcl_freopen std::freopen
#define vcl_fsetpos std::fsetpos
#define vcl_getc std::getc
#define vcl_getchar std::getchar
#define vcl_gets std::gets
#define vcl_putc std::putc
#define vcl_putchar std::putchar
#define vcl_puts std::puts
#define vcl_rewind std::rewind
#define vcl_setbuf std::setbuf
#define vcl_setvbuf std::setvbuf
#define vcl_tmpfile std::tmpfile
#define vcl_tmpnam std::tmpnam
#define vcl_ungetc std::ungetc
#define vcl_printf std::printf
#define vcl_sprintf std::sprintf
#define vcl_fprintf std::fprintf
#define vcl_vprintf std::vprintf
#define vcl_vsprintf std::vsprintf
#define vcl_vfprintf std::vfprintf

// Adapted from
// http://stackoverflow.com/questions/2915672/snprintf-and-visual-studio-2010
//
// Microsoft has finally implemented snprintf in Visual Studio 2015. On earlier
// versions you can simulate it as below.
#if defined(_MSC_VER) && _MSC_VER < 1900
__inline int c99_vsnprintf(char *outBuf, size_t size, const char *format, va_list ap)
{
    int count = -1;

    if (size != 0)
        count = _vsnprintf_s(outBuf, size, _TRUNCATE, format, ap);
    if (count == -1)
        count = _vscprintf(format, ap);

    return count;
}

__inline int vcl_snprintf(char *outBuf, size_t size, const char *format, ...)
{
    int count;
    va_list ap;

    va_start(ap, format);
    count = c99_vsnprintf(outBuf, size, format, ap);
    va_end(ap);

    return count;
}
#else
#define vcl_snprintf std::snprintf
#endif

#define vcl_scanf std::scanf
#define vcl_sscanf std::sscanf
#define vcl_fscanf std::fscanf
#define vcl_vscanf std::vscanf
#define vcl_vsscanf std::vsscanf
#define vcl_vfscanf std::vfscanf

#define vcl_div std::div
#define vcl_labs std::labs
#define vcl_ldiv std::ldiv
#define vcl_srand std::srand
#define vcl_rand std::rand
#define vcl_atexit std::atexit
#define vcl_getenv std::getenv
#define vcl_system std::system
#define vcl_exit std::exit
#define vcl_abort std::abort
#define vcl_qsort std::qsort
#define vcl_calloc std::calloc
#define vcl_malloc std::malloc
#define vcl_free std::free
#define vcl_realloc std::realloc
#define vcl_atol std::atol
#define vcl_atof std::atof
#define vcl_atoi std::atoi
#define vcl_mblen std::mblen
#define vcl_mbstowcs std::mbstowcs
#define vcl_mbtowc std::mbtowc
#define vcl_strtod std::strtod
#define vcl_strtol std::strtol
#define vcl_strtoul std::strtoul
#define vcl_wctomb std::wctomb
#define vcl_wcstombs std::wcstombs
#define vcl_memchr std::memchr
#define vcl_memcmp std::memcmp
#define vcl_memcpy std::memcpy
#define vcl_memmove std::memmove
#define vcl_memset std::memset
#define vcl_strcat std::strcat
#define vcl_strchr std::strchr
#define vcl_strcmp std::strcmp
#define vcl_strcoll std::strcoll
#define vcl_strcpy std::strcpy
#define vcl_strcspn std::strcspn
#define vcl_strerror std::strerror
#define vcl_strlen std::strlen
#define vcl_strncat std::strncat
#define vcl_strncmp std::strncmp
#define vcl_strncpy std::strncpy
#define vcl_strpbrk std::strpbrk
#define vcl_strrchr std::strrchr
#define vcl_strspn std::strspn
#define vcl_strstr std::strstr
#define vcl_strtok std::strtok
#define vcl_strxfrm std::strxfrm
#define vcl_clock_t std::clock_t
#define vcl_time_t std::time_t
#define vcl_tm std::tm
#define vcl_asctime std::asctime
#define vcl_clock std::clock
#define vcl_difftime std::difftime
#define vcl_localtime std::localtime
#define vcl_strftime std::strftime
#define vcl_ctime std::ctime
#define vcl_gmtime std::gmtime
#define vcl_mktime std::mktime
#define vcl_time std::time
#define vcl_mbstate_t std::mbstate_t
#define vcl_wint_t std::wint_t
#define vcl_btowc std::btowc
#define vcl_fgetwc std::fgetwc
#define vcl_fgetws std::fgetws
#define vcl_fputwc std::fputwc
#define vcl_fputws std::fputws
#define vcl_fwide std::fwide
#define vcl_fwprintf std::fwprintf
#define vcl_fwscanf std::fwscanf
#define vcl_getwc std::getwc
#define vcl_getwchar std::getwchar
#define vcl_mbrlen std::mbrlen
#define vcl_mbrtowc std::mbrtowc
#define vcl_putwc std::putwc
#define vcl_putwchar std::putwchar
#define vcl_swprintf std::swprintf
#define vcl_swscanf std::swscanf
#define vcl_ungetwc std::ungetwc
#define vcl_vfwprintf std::vfwprintf
#define vcl_vswprintf std::vswprintf
#define vcl_vwprintf std::vwprintf
#define vcl_wcrtomb std::wcrtomb
#define vcl_wcscat std::wcscat
#define vcl_wcschr std::wcschr
#define vcl_wcscmp std::wcscmp
#define vcl_wcscoll std::wcscoll
#define vcl_wcscpy std::wcscpy
#define vcl_wcscspn std::wcscspn
#define vcl_wcsftime std::wcsftime
#define vcl_wcslen std::wcslen
#define vcl_wcsncat std::wcsncat
#define vcl_wcsncmp std::wcsncmp
#define vcl_wcsncpy std::wcsncpy
#define vcl_wcspbrk std::wcspbrk
#define vcl_wcsrchr std::wcsrchr
#define vcl_wcsrtombs std::wcsrtombs
#define vcl_wcsspn std::wcsspn
#define vcl_wcsstr std::wcsstr
#define vcl_wcstod std::wcstod
#define vcl_wcstok std::wcstok
#define vcl_wcstol std::wcstol
#define vcl_wctoul std::wctoul
#define vcl_wcsxfrm std::wcsxfrm
#define vcl_wctob std::wctob
#define vcl_wmemchr std::wmemchr
#define vcl_wmemcmp std::wmemcmp
#define vcl_wmemcpy std::wmemcpy
#define vcl_wmemmove std::wmemmove
#define vcl_wmemset std::wmemset
#define vcl_wprintf std::wprintf
#define vcl_wscanf std::wscanf
#define vcl_iswalnum std::iswalnum
#define vcl_iswalpha std::iswalpha
#define vcl_iswcntrl std::iswcntrl
#define vcl_iswctrans std::iswctrans
#define vcl_iswctype std::iswctype
#define vcl_iswdigit std::iswdigit
#define vcl_iswgraph std::iswgraph
#define vcl_iswlower std::iswlower
#define vcl_iswprint std::iswprint
#define vcl_iswpunct std::iswpunct
#define vcl_iswspace std::iswspace
#define vcl_iswupper std::iswupper
#define vcl_iswxdigit std::iswxdigit
#define vcl_towctrans std::towctrans
#define vcl_towlower std::towlower
#define vcl_towupper std::towupper
#define vcl_wctrans std::wctrans
#define vcl_wctrans_t std::wctrans_t
#define vcl_wctype std::wctype
#define vcl_wctype_t std::wctype_t
#define vcl_deque std::deque
#define vcl_exception std::exception
#define vcl_bad_exception std::bad_exception
#define vcl_filebuf std::filebuf
#define vcl_fstream std::fstream
#define vcl_ifstream std::ifstream
#define vcl_ofstream std::ofstream
#define vcl_unary_function std::unary_function
#define vcl_binary_function std::binary_function
#define vcl_plus std::plus
#define vcl_minus std::minus
#define vcl_multiplies std::multiplies
#define vcl_divides std::divides
#define vcl_modulus std::modulus
#define vcl_negate std::negate
#define vcl_equal_to std::equal_to
#define vcl_not_equal_to std::not_equal_to
#define vcl_greater std::greater
#define vcl_less std::less
#define vcl_greater_equal std::greater_equal
#define vcl_less_equal std::less_equal
#define vcl_logical_and std::logical_and
#define vcl_logical_or std::logical_or
#define vcl_logical_not std::logical_not
#define vcl_unary_negate std::unary_negate
#define vcl_not1 std::not1
#define vcl_binary_negate std::binary_negate
#define vcl_not2 std::not2
#define vcl_binder1st std::binder1st
#define vcl_bind1st std::bind1st
#define vcl_binder2nd std::binder2nd
#define vcl_bind2nd std::bind2nd
#define vcl_pointer_to_unary_function std::pointer_to_unary_function
#define vcl_ptr_fun std::ptr_fun
#define vcl_pointer_to_binary_function std::pointer_to_binary_function
#define vcl_mem_fun_t std::mem_fun_t
#define vcl_mem_fun std::mem_fun
#define vcl_mem_fun1_t std::mem_fun1_t
#define vcl_mem_fun1 std::mem_fun1
#define vcl_mem_fun_ref_t std::mem_fun_ref_t
#define vcl_mem_fun_ref std::mem_fun_ref
#define vcl_mem_fun1_ref_t std::mem_fun1_ref_t
#define vcl_mem_fun1_ref std::mem_fun1_ref
#define vcl_const_mem_fun_t std::const_mem_fun_t
#define vcl_const_mem_fun std::const_mem_fun
#define vcl_const_mem_fun1_t std::const_mem_fun1_t
#define vcl_const_mem_fun1 std::const_mem_fun1
#define vcl_const_mem_fun_ref_t std::const_mem_fun_ref_t
#define vcl_const_mem_fun_ref std::const_mem_fun_ref
#define vcl_const_mem_fun1_ref_t std::const_mem_fun1_ref_t
#define vcl_const_mem_fun1_ref std::const_mem_fun1_ref
#define vcl_resetiosflags std::resetiosflags
#define vcl_setiosflags std::setiosflags
#define vcl_setbase std::setbase
#define vcl_setfill std::setfill
#define vcl_setprecision std::setprecision
#define vcl_setw std::setw
#define vcl_boolalpha std::boolalpha
#define vcl_noboolalpha std::noboolalpha
#define vcl_showbase std::showbase
#define vcl_noshowbase std::noshowbase
#define vcl_showpoint std::showpoint
#define vcl_noshowpoint std::noshowpoint
#define vcl_showpos std::showpos
#define vcl_noshowpos std::noshowpos
#define vcl_skipws std::skipws
#define vcl_noskipws std::noskipws
#define vcl_uppercase std::uppercase
#define vcl_nouppercase std::nouppercase
#define vcl_internal std::internal
#define vcl_left std::left
#define vcl_right std::right
#define vcl_dec std::dec
#define vcl_hex std::hex
#define vcl_oct std::oct
#define vcl_fixed std::fixed
#define vcl_scientific std::scientific
#define vcl_ios std::ios
#define vcl_streamoff std::streamoff
#define vcl_streamsize std::streamsize
#define vcl_fpos std::fpos
#define vcl_basic_ios std::basic_ios
#define vcl_ios_base std::ios_base
#define vcl_ios_basefield std::ios::basefield
#define vcl_ios_floatfield std::ios::floatfield
#define vcl_ios_adjustfield std::ios::adjustfield
#define vcl_ios_boolalpha std::ios::boolalpha
#define vcl_ios_noboolalpha std::ios::noboolalpha
#define vcl_ios_showbase std::ios::showbase
#define vcl_ios_noshowbase std::ios::noshowbase
#define vcl_ios_showpoint std::ios::showpoint
#define vcl_ios_noshowpoint std::ios::noshowpoint
#define vcl_ios_showpos std::ios::showpos
#define vcl_ios_noshowpos std::ios::noshowpos
#define vcl_ios_skipws std::ios::skipws
#define vcl_ios_noskipws std::ios::noskipws
#define vcl_ios_uppercase std::ios::uppercase
#define vcl_ios_nouppercase std::ios::nouppercase
#define vcl_ios_internal std::ios::internal
#define vcl_ios_left std::ios::left
#define vcl_ios_right std::ios::right
#define vcl_ios_dec std::ios::dec
#define vcl_ios_hex std::ios::hex
#define vcl_ios_oct std::ios::oct
#define vcl_ios_fixed std::ios::fixed
#define vcl_ios_scientific std::ios::scientific
#define vcl_ios_fmtflags std::ios::fmtflags
#define vcl_ios_seekdir std::ios::seekdir
#define vcl_ios_beg std::ios::beg
#define vcl_ios_cur std::ios::cur
#define vcl_ios_end std::ios::end
#define vcl_basic_istringstream std::basic_istringstream
#define vcl_basic_ostringstream std::basic_ostringstream
#define vcl_basic_filebuf std::basic_filebuf
#define vcl_basic_ifstream std::basic_ifstream
#define vcl_basic_ofstream std::basic_ofstream
#define vcl_basic_fstream std::basic_fstream
#define vcl_istreambuf_iterator std::istreambuf_iterator
#define vcl_ostreambuf_iterator std::ostreambuf_iterator
#define vcl_wios std::wios
#define vcl_stringstream std::stringstream
#define vcl_wstreambuf std::wstreambuf
#define vcl_wistringstream std::wistringstream
#define vcl_wostringstream std::wostringstream
#define vcl_wstringstream std::wstringstream
#define vcl_wfilebuf std::wfilebuf
#define vcl_wifstream std::wifstream
#define vcl_wofstream std::wofstream
#define vcl_wfstream std::wfstream
#define vcl_wstreampos std::wstreampos
#define vcl_cin std::cin
#define vcl_cout std::cout
#define vcl_cerr std::cerr
#define vcl_clog std::clog
#define vcl_wcout std::wcout
#define vcl_ios_in std::ios::in
#define vcl_ios_out std::ios::out
#define vcl_ios_ate std::ios::ate
#define vcl_ios_app std::ios::app
#define vcl_ios_trunc std::ios::trunc
#define vcl_ios_binary std::ios::binary
#define vcl_ios_openmode std::ios::openmode
#define vcl_streampos std::streampos
#define vcl_ws std::ws
#define vcl_basic_istream std::basic_istream
#define vcl_istream std::istream
#define vcl_wistream std::wistream
#define vcl_basic_iostream std::basic_iostream
#define vcl_iostream std::iostream
#define vcl_wiostream std::wiostream
#define vcl_iterator_traits std::iterator_traits
#define vcl_iterator std::iterator
#define vcl_reverse_iterator std::reverse_iterator
#define vcl_istream_iterator std::istream_iterator
#define vcl_ostream_iterator std::ostream_iterator
#define vcl_back_insert_iterator std::back_insert_iterator
#define vcl_front_insert_iterator std::front_insert_iterator
#define vcl_insert_iterator std::insert_iterator
#define vcl_inserter std::inserter
#define vcl_back_inserter std::back_inserter
#define vcl_front_inserter std::front_inserter
#define vcl_input_iterator_tag std::input_iterator_tag
#define vcl_output_iterator_tag std::output_iterator_tag
#define vcl_forward_iterator_tag std::forward_iterator_tag
#define vcl_bidirectional_iterator_tag std::bidirectional_iterator_tag
#define vcl_random_access_iterator_tag std::random_access_iterator_tag
#define vcl_advance std::advance
#define vcl_distance std::distance
#define vcl_numeric_limits std::numeric_limits
#define vcl_float_round_style std::float_round_style
#define vcl_float_denorm_style std::float_denorm_style
#define vcl_round_toward_zero std::round_toward_zero
#define vcl_round_toward_neg_infinity std::round_toward_neg_infinity
#define vcl_list std::list
#define vcl_use_facet std::use_facet
#define vcl_has_facet std::has_facet
#define vcl_isspace std::isspace
#define vcl_isprint std::isprint
#define vcl_iscntrl std::iscntrl
#define vcl_isupper std::isupper
#define vcl_islower std::islower
#define vcl_isalpha std::isalpha
#define vcl_isdigit std::isdigit
#define vcl_ispunct std::ispunct
#define vcl_isxdigit std::isxdigit
#define vcl_isalnum std::isalnum
#define vcl_isgraph std::isgraph
#define vcl_toupper std::toupper
#define vcl_tolower std::tolower
#define vcl_ctype std::ctype
#define vcl_codecvt_base std::codecvt_base
#define vcl_codecvt std::codecvt
#define vcl_codecvt_byname std::codecvt_byname
#define vcl_num_get std::num_get
#define vcl_num_put std::num_put
#define vcl_numpunct std::numpunct
#define vcl_numpunct_byname std::numpunct_byname
#define vcl_collate std::collate
#define vcl_collate_byname std::collate_byname
#define vcl_time_get std::time_get
#define vcl_time_get_byname std::time_get_byname
#define vcl_time_put std::time_put
#define vcl_time_put_byname std::time_put_byname
#define vcl_money_get std::money_get
#define vcl_money_put std::money_put
#define vcl_moneypunct std::moneypunct
#define vcl_moneypunct_byname std::moneypunct_byname
#define vcl_messages std::messages
#define vcl_messages_byname std::messages_byname
#define vcl_map std::map
#define vcl_multimap std::multimap
#define vcl_allocator std::allocator
#define vcl_raw_storage_iterator std::raw_storage_iterator
#define vcl_get_temporary_buffer std::get_temporary_buffer
#define vcl_return_temporary_buffer std::return_temporary_buffer
#define vcl_uninitialized_copy std::uninitialized_copy
#define vcl_uninitialized_fill std::uninitialized_fill
#define vcl_uninitialized_fill_n std::uninitialized_fill_n
#define vcl_auto_ptr vcl_auto_ptr
#define vcl_bad_alloc std::bad_alloc
#define vcl_set_new_handler std::set_new_handler
#define vcl_accumulate std::accumulate
#define vcl_inner_product std::inner_product
#define vcl_partial_sum std::partial_sum
#define vcl_adjacent_difference std::adjacent_difference
#define vcl_basic_ostream std::basic_ostream
#define vcl_ostream std::ostream
#define vcl_wostream std::wostream
#define vcl_endl std::endl
#define vcl_ends std::ends
#define vcl_flush std::flush
#define vcl_queue std::queue
#define vcl_priority_queue std::priority_queue
#define vcl_set std::set
#define vcl_multiset std::multiset
#define vcl_basic_stringbuf std::basic_stringbuf
#define vcl_stringbuf std::stringbuf
#define vcl_wstringbuf std::wstringbuf
#define vcl_istringstream std::istringstream
#define vcl_ostringstream std::ostringstream
#define vcl_stack std::stack
#define vcl_logic_error std::logic_error
#define vcl_domain_error std::domain_error
#define vcl_invalid_argument std::invalid_argument
#define vcl_length_error std::length_error
#define vcl_out_of_range std::out_of_range
#define vcl_runtime_error std::runtime_error
#define vcl_range_error std::range_error
#define vcl_overflow_error std::overflow_error
#define vcl_underflow_error std::underflow_error
#define vcl_basic_streambuf std::basic_streambuf
#define vcl_streambuf std::streambuf
#define vcl_char_traits std::char_traits
#define vcl_basic_string std::basic_string
#define vcl_string std::string
#define vcl_wstring std::wstring
#define vcl_getline std::getline
#define vcl_type_info std::type_info
#define vcl_bad_cast std::bad_cast
#define vcl_bad_typeid std::bad_typeid
#define vcl_pair std::pair
#define vcl_make_pair std::make_pair
#define vcl_valarray std::valarray
#define vcl_slice std::slice
#define vcl_slice_array std::slice_array
#define vcl_gslice std::gslice
#define vcl_gslice_array std::gslice_array
#define vcl_mask_array std::mask_array
#define vcl_indirect_array std::indirect_array
#define vcl_vector std::vector

# define vcl_memory_prefix std

#define vcl_bad_weak_ptr            std::bad_weak_ptr
#define vcl_shared_ptr              std::shared_ptr
#define vcl_static_pointer_cast     std::static_pointer_cast
#define vcl_dynamic_pointer_cast    std::dynamic_pointer_cast
#define vcl_const_pointer_cast      std::const_pointer_cast
#define vcl_get_deleter             std::get_deleter
#define vcl_weak_ptr                std::weak_ptr
#define vcl_enable_shared_from_this std::enable_shared_from_this

#define vnl_math_isnan vnl_math::isnan
#define vnl_math_isinf vnl_math::isinf
#define vnl_math_isfinite vnl_math::isfinite
#define vnl_math_rnd_halfinttoeven vnl_math::rnd_halfinttoeven
#define vnl_math_rnd_halfintup vnl_math::rnd_halfintup
#define vnl_math_rnd vnl_math::rnd
#define vnl_math_floor vnl_math::floor
#define vnl_math_ceil vnl_math::ceil
#define vnl_math_abs vnl_math::abs
#define vnl_math_max vnl_math::max
#define vnl_math_min vnl_math::min
#define vnl_math_sqr vnl_math::sqr
#define vnl_math_cube vnl_math::cube
#define vnl_math_sgn vnl_math::sgn
#define vnl_math_sgn0 vnl_math::sgn0
#define vnl_math_squared_magnitude vnl_math::squared_magnitude
#define vnl_math_cuberoot std::cbrt
#define vnl_math_hypot vnl_math::hypot


#endif //VCL_LEGACY_ALIASES_H_
