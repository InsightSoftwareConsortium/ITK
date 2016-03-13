#!python
# \author Hans J. Johnson
#
# Now that all supported compilers simply
# use exactly one function signature (i.e.
# namely the one provided in the std:: namespace)
# there is no need to use the vcl_ aliases.

import os
import sys
from collections import OrderedDict

if len(sys.argv) != 2:
    usage = """
INCORRECT USAGE:
   {0}

USAGE:
  python {1} source_file_to_modernize

Examples:
  SRC_BASE_DIR=~/MYSRC/Submodule
  for ext in ".h" ".cxx" ".cpp" ".hxx" ".hpp" ".txx"; do
    find ${{SRC_BASE_DIR}} -type f -name "*${{ext}}" -exec python Utilities/Maintenance/VCL_ModernizeNaming.py {{}} \;
  done
""".format(sys.argv, sys.argv[0])
    print(usage)
    sys.exit(-1)


# slight modification from grep command
info_for_conversion = """
vcl_algorithm.h,vcl_adjacent_find,std::adjacent_find
vcl_algorithm.h,vcl_and,std::and
vcl_algorithm.h,vcl_binary,std::binary
vcl_algorithm.h,vcl_binary_search,std::binary_search
vcl_algorithm.h,vcl_copy,std::copy
vcl_algorithm.h,vcl_copy_,std::copy_
vcl_algorithm.h,vcl_count,std::count
vcl_algorithm.h,vcl_count_if,std::count_if
vcl_algorithm.h,vcl_equal,std::equal
vcl_algorithm.h,vcl_equal_range,std::equal_range
vcl_algorithm.h,vcl_fill,std::fill
vcl_algorithm.h,vcl_fill_n,std::fill_n
vcl_algorithm.h,vcl_find,std::find
vcl_algorithm.h,vcl_find_end,std::find_end
vcl_algorithm.h,vcl_find_first_of,std::find_first_of
vcl_algorithm.h,vcl_find_if,std::find_if
vcl_algorithm.h,vcl_for_each,std::for_each
vcl_algorithm.h,vcl_generate,std::generate
vcl_algorithm.h,vcl_generate_n,std::generate_n
vcl_algorithm.h,vcl_generators_,std::generators_
vcl_algorithm.h,vcl_heap,std::heap
vcl_algorithm.h,vcl_includes,std::includes
vcl_algorithm.h,vcl_inplace_merge,std::inplace_merge
vcl_algorithm.h,vcl_iter_swap,std::iter_swap
vcl_algorithm.h,vcl_lexicographical_compare,std::lexicographical_compare
vcl_algorithm.h,vcl_lower_bound,std::lower_bound
vcl_algorithm.h,vcl_make_heap,std::make_heap
vcl_algorithm.h,vcl_max,std::max
vcl_algorithm.h,vcl_max_element,std::max_element
vcl_algorithm.h,vcl_merge,std::merge
vcl_algorithm.h,vcl_merge_,std::merge_
vcl_algorithm.h,vcl_min,std::min
vcl_algorithm.h,vcl_min_element,std::min_element
vcl_algorithm.h,vcl_mismatch,std::mismatch
vcl_algorithm.h,vcl_next_permutation,std::next_permutation
vcl_algorithm.h,vcl_nth_element,std::nth_element
vcl_algorithm.h,vcl_partial_sort,std::partial_sort
vcl_algorithm.h,vcl_partial_sort_copy,std::partial_sort_copy
vcl_algorithm.h,vcl_partition,std::partition
vcl_algorithm.h,vcl_partitions_,std::partitions_
vcl_algorithm.h,vcl_pop_heap,std::pop_heap
vcl_algorithm.h,vcl_prev_permutation,std::prev_permutation
vcl_algorithm.h,vcl_push_heap,std::push_heap
vcl_algorithm.h,vcl_random_shuffle,std::random_shuffle
vcl_algorithm.h,vcl_remove,std::remove
vcl_algorithm.h,vcl_remove_copy,std::remove_copy
vcl_algorithm.h,vcl_remove_copy_if,std::remove_copy_if
vcl_algorithm.h,vcl_remove_if,std::remove_if
vcl_algorithm.h,vcl_replace,std::replace
vcl_algorithm.h,vcl_replace_copy,std::replace_copy
vcl_algorithm.h,vcl_replace_copy_if,std::replace_copy_if
vcl_algorithm.h,vcl_replace_if,std::replace_if
vcl_algorithm.h,vcl_reverse,std::reverse
vcl_algorithm.h,vcl_reverse_copy,std::reverse_copy
vcl_algorithm.h,vcl_rotate,std::rotate
vcl_algorithm.h,vcl_rotate_copy,std::rotate_copy
vcl_algorithm.h,vcl_search,std::search
vcl_algorithm.h,vcl_search_n,std::search_n
vcl_algorithm.h,vcl_set_difference,std::set_difference
vcl_algorithm.h,vcl_set_intersection,std::set_intersection
vcl_algorithm.h,vcl_set_symmetric_difference,std::set_symmetric_difference
vcl_algorithm.h,vcl_set_union,std::set_union
vcl_algorithm.h,vcl_sort,std::sort
vcl_algorithm.h,vcl_sort_,std::sort_
vcl_algorithm.h,vcl_sort_heap,std::sort_heap
vcl_algorithm.h,vcl_stable_partition,std::stable_partition
vcl_algorithm.h,vcl_stable_sort,std::stable_sort
vcl_algorithm.h,vcl_swap,std::swap
vcl_algorithm.h,vcl_swap_,std::swap_
vcl_algorithm.h,vcl_swap_ranges,std::swap_ranges
vcl_algorithm.h,vcl_transform,std::transform
vcl_algorithm.h,vcl_unique,std::unique
vcl_algorithm.h,vcl_unique_copy,std::unique_copy
vcl_algorithm.h,vcl_upper_bound,std::upper_bound
vcl_bitset.h,vcl_bitset,std::bitset
vcl_cctype.h,vcl_isalnum,std::isalnum
vcl_cctype.h,vcl_isalpha,std::isalpha
vcl_cctype.h,vcl_iscntrl,std::iscntrl
vcl_cctype.h,vcl_isdigit,std::isdigit
vcl_cctype.h,vcl_isgraph,std::isgraph
vcl_cctype.h,vcl_islower,std::islower
vcl_cctype.h,vcl_isprint,std::isprint
vcl_cctype.h,vcl_ispunct,std::ispunct
vcl_cctype.h,vcl_isspace,std::isspace
vcl_cctype.h,vcl_isupper,std::isupper
vcl_cctype.h,vcl_isxdigit,std::isxdigit
vcl_cctype.h,vcl_tolower,std::tolower
vcl_cctype.h,vcl_toupper,std::toupper
vcl_cmath.h,vcl_abs,std::abs
vcl_cmath.h,vcl_acos,std::acos
vcl_cmath.h,vcl_asin,std::asin
vcl_cmath.h,vcl_atan,std::atan
vcl_cmath.h,vcl_atan2,std::atan2
vcl_cmath.h,vcl_ceil,std::ceil
vcl_cmath.h,vcl_cos,std::cos
vcl_cmath.h,vcl_cosh,std::cosh
vcl_cmath.h,vcl_exp,std::exp
vcl_cmath.h,vcl_fabs,std::fabs
vcl_cmath.h,vcl_floor,std::floor
vcl_cmath.h,vcl_fmod,std::fmod
vcl_cmath.h,vcl_frexp,std::frexp
vcl_cmath.h,vcl_ldexp,std::ldexp
vcl_cmath.h,vcl_log,std::log
vcl_cmath.h,vcl_log10,std::log10
vcl_cmath.h,vcl_modf,std::modf
vcl_cmath.h,vcl_pow,std::pow
vcl_cmath.h,vcl_sin,std::sin
vcl_cmath.h,vcl_sinh,std::sinh
vcl_cmath.h,vcl_sqrt,std::sqrt
vcl_cmath.h,vcl_tan,std::tan
vcl_cmath.h,vcl_tanh,std::tanh
vcl_complex_fwd.h,vcl_abs,std::abs
vcl_complex.h,vcl_abs,std::abs
vcl_complex.h,vcl_arg,std::arg
vcl_complex.h,vcl_complex,std::complex
vcl_complex.h,vcl_conj,std::conj
vcl_complex.h,vcl_cos,std::cos
vcl_complex.h,vcl_cosh,std::cosh
vcl_complex.h,vcl_exp,std::exp
vcl_complex.h,vcl_imag,std::imag
vcl_complex.h,vcl_log,std::log
vcl_complex.h,vcl_log10,std::log10
vcl_complex.h,vcl_norm,std::norm
vcl_complex.h,vcl_polar,std::polar
vcl_complex.h,vcl_pow,std::pow
vcl_complex.h,vcl_real,std::real
vcl_complex.h,vcl_sin,std::sin
vcl_complex.h,vcl_sinh,std::sinh
vcl_complex.h,vcl_sqrt,std::sqrt
vcl_complex.h,vcl_tan,std::tan
vcl_complex.h,vcl_tanh,std::tanh
vcl_csetjmp.h,vcl_jmp_buf,std::jmp_buf
vcl_csetjmp.h,vcl_longjmp,std::longjmp
vcl_csignal.h,vcl_raise,std::raise
vcl_csignal.h,vcl_sig_atomic_t,std::sig_atomic_t
vcl_csignal.h,vcl_signal,std::signal
vcl_cstdarg.h,vcl_va_list,std::va_list
vcl_cstddef.h,vcl_ptrdiff_t,std::ptrdiff_t
vcl_cstddef.h,vcl_size_t,std::size_t
vcl_cstdio.h,vcl_FILE,std::FILE
vcl_cstdio.h,vcl_clearerr,std::clearerr
vcl_cstdio.h,vcl_fclose,std::fclose
vcl_cstdio.h,vcl_feof,std::feof
vcl_cstdio.h,vcl_ferror,std::ferror
vcl_cstdio.h,vcl_fflush,std::fflush
vcl_cstdio.h,vcl_fgetc,std::fgetc
vcl_cstdio.h,vcl_fgetpos,std::fgetpos
vcl_cstdio.h,vcl_fgets,std::fgets
vcl_cstdio.h,vcl_fopen,std::fopen
vcl_cstdio.h,vcl_fpos_t,std::fpos_t
vcl_cstdio.h,vcl_fprintf,std::fprintf
vcl_cstdio.h,vcl_fputc,std::fputc
vcl_cstdio.h,vcl_fputs,std::fputs
vcl_cstdio.h,vcl_fread,std::fread
vcl_cstdio.h,vcl_freopen,std::freopen
vcl_cstdio.h,vcl_fscanf,std::fscanf
vcl_cstdio.h,vcl_fseek,std::fseek
vcl_cstdio.h,vcl_fsetpos,std::fsetpos
vcl_cstdio.h,vcl_ftell,std::ftell
vcl_cstdio.h,vcl_fwrite,std::fwrite
vcl_cstdio.h,vcl_getc,std::getc
vcl_cstdio.h,vcl_getchar,std::getchar
vcl_cstdio.h,vcl_gets,std::gets
vcl_cstdio.h,vcl_perror,std::perror
vcl_cstdio.h,vcl_printf,std::printf
vcl_cstdio.h,vcl_putc,std::putc
vcl_cstdio.h,vcl_putchar,std::putchar
vcl_cstdio.h,vcl_puts,std::puts
vcl_cstdio.h,vcl_remove,std::remove
vcl_cstdio.h,vcl_rename,std::rename
vcl_cstdio.h,vcl_rewind,std::rewind
vcl_cstdio.h,vcl_scanf,std::scanf
vcl_cstdio.h,vcl_setbuf,std::setbuf
vcl_cstdio.h,vcl_setvbuf,std::setvbuf
vcl_cstdio.h,vcl_snprintf,vcl_snprintf
vcl_cstdio.h,vcl_sprintf,std::sprintf
vcl_cstdio.h,vcl_sscanf,std::sscanf
vcl_cstdio.h,vcl_tmpfile,std::tmpfile
vcl_cstdio.h,vcl_tmpnam,std::tmpnam
vcl_cstdio.h,vcl_ungetc,std::ungetc
vcl_cstdio.h,vcl_vfprintf,std::vfprintf
vcl_cstdio.h,vcl_vfscanf,std::vfscanf
vcl_cstdio.h,vcl_vprintf,std::vprintf
vcl_cstdio.h,vcl_vscanf,std::vscanf
vcl_cstdio.h,vcl_vsprintf,std::vsprintf
vcl_cstdio.h,vcl_vsscanf,std::vsscanf
vcl_cstdlib.h,vcl_abort,std::abort
vcl_cstdlib.h,vcl_abs,std::abs
vcl_cstdlib.h,vcl_atexit,std::atexit
vcl_cstdlib.h,vcl_atof,std::atof
vcl_cstdlib.h,vcl_atoi,std::atoi
vcl_cstdlib.h,vcl_atol,std::atol
vcl_cstdlib.h,vcl_calloc,std::calloc
vcl_cstdlib.h,vcl_div,std::div
vcl_cstdlib.h,vcl_exit,std::exit
vcl_cstdlib.h,vcl_free,std::free
vcl_cstdlib.h,vcl_getenv,std::getenv
vcl_cstdlib.h,vcl_labs,std::labs
vcl_cstdlib.h,vcl_ldiv,std::ldiv
vcl_cstdlib.h,vcl_malloc,std::malloc
vcl_cstdlib.h,vcl_mblen,std::mblen
vcl_cstdlib.h,vcl_mbstowcs,std::mbstowcs
vcl_cstdlib.h,vcl_mbtowc,std::mbtowc
vcl_cstdlib.h,vcl_qsort,std::qsort
vcl_cstdlib.h,vcl_rand,std::rand
vcl_cstdlib.h,vcl_realloc,std::realloc
vcl_cstdlib.h,vcl_srand,std::srand
vcl_cstdlib.h,vcl_strtod,std::strtod
vcl_cstdlib.h,vcl_strtol,std::strtol
vcl_cstdlib.h,vcl_strtoul,std::strtoul
vcl_cstdlib.h,vcl_system,std::system
vcl_cstdlib.h,vcl_wcstombs,std::wcstombs
vcl_cstdlib.h,vcl_wctomb,std::wctomb
vcl_cstring.h,vcl_memchr,std::memchr
vcl_cstring.h,vcl_memcmp,std::memcmp
vcl_cstring.h,vcl_memcpy,std::memcpy
vcl_cstring.h,vcl_memmove,std::memmove
vcl_cstring.h,vcl_memset,std::memset
vcl_cstring.h,vcl_strcat,std::strcat
vcl_cstring.h,vcl_strchr,std::strchr
vcl_cstring.h,vcl_strcmp,std::strcmp
vcl_cstring.h,vcl_strcoll,std::strcoll
vcl_cstring.h,vcl_strcpy,std::strcpy
vcl_cstring.h,vcl_strcspn,std::strcspn
vcl_cstring.h,vcl_strerror,std::strerror
vcl_cstring.h,vcl_strlen,std::strlen
vcl_cstring.h,vcl_strncat,std::strncat
vcl_cstring.h,vcl_strncmp,std::strncmp
vcl_cstring.h,vcl_strncpy,std::strncpy
vcl_cstring.h,vcl_strpbrk,std::strpbrk
vcl_cstring.h,vcl_strrchr,std::strrchr
vcl_cstring.h,vcl_strspn,std::strspn
vcl_cstring.h,vcl_strstr,std::strstr
vcl_cstring.h,vcl_strtok,std::strtok
vcl_cstring.h,vcl_strxfrm,std::strxfrm
vcl_ctime.h,vcl_asctime,std::asctime
vcl_ctime.h,vcl_clock,std::clock
vcl_ctime.h,vcl_clock_t,std::clock_t
vcl_ctime.h,vcl_ctime,std::ctime
vcl_ctime.h,vcl_difftime,std::difftime
vcl_ctime.h,vcl_gmtime,std::gmtime
vcl_ctime.h,vcl_localtime,std::localtime
vcl_ctime.h,vcl_mktime,std::mktime
vcl_ctime.h,vcl_strftime,std::strftime
vcl_ctime.h,vcl_time,std::time
vcl_ctime.h,vcl_time_t,std::time_t
vcl_ctime.h,vcl_tm,std::tm
vcl_cwchar.h,vcl_btowc,std::btowc
vcl_cwchar.h,vcl_fgetwc,std::fgetwc
vcl_cwchar.h,vcl_fgetws,std::fgetws
vcl_cwchar.h,vcl_fputwc,std::fputwc
vcl_cwchar.h,vcl_fputws,std::fputws
vcl_cwchar.h,vcl_fwide,std::fwide
vcl_cwchar.h,vcl_fwprintf,std::fwprintf
vcl_cwchar.h,vcl_fwscanf,std::fwscanf
vcl_cwchar.h,vcl_getwc,std::getwc
vcl_cwchar.h,vcl_getwchar,std::getwchar
vcl_cwchar.h,vcl_mbrlen,std::mbrlen
vcl_cwchar.h,vcl_mbrtowc,std::mbrtowc
vcl_cwchar.h,vcl_mbstate_t,std::mbstate_t
vcl_cwchar.h,vcl_putwc,std::putwc
vcl_cwchar.h,vcl_putwchar,std::putwchar
vcl_cwchar.h,vcl_swprintf,std::swprintf
vcl_cwchar.h,vcl_swscanf,std::swscanf
vcl_cwchar.h,vcl_ungetwc,std::ungetwc
vcl_cwchar.h,vcl_vfwprintf,std::vfwprintf
vcl_cwchar.h,vcl_vswprintf,std::vswprintf
vcl_cwchar.h,vcl_vwprintf,std::vwprintf
vcl_cwchar.h,vcl_wcrtomb,std::wcrtomb
vcl_cwchar.h,vcl_wcscat,std::wcscat
vcl_cwchar.h,vcl_wcschr,std::wcschr
vcl_cwchar.h,vcl_wcscmp,std::wcscmp
vcl_cwchar.h,vcl_wcscoll,std::wcscoll
vcl_cwchar.h,vcl_wcscpy,std::wcscpy
vcl_cwchar.h,vcl_wcscspn,std::wcscspn
vcl_cwchar.h,vcl_wcsftime,std::wcsftime
vcl_cwchar.h,vcl_wcslen,std::wcslen
vcl_cwchar.h,vcl_wcsncat,std::wcsncat
vcl_cwchar.h,vcl_wcsncmp,std::wcsncmp
vcl_cwchar.h,vcl_wcsncpy,std::wcsncpy
vcl_cwchar.h,vcl_wcspbrk,std::wcspbrk
vcl_cwchar.h,vcl_wcsrchr,std::wcsrchr
vcl_cwchar.h,vcl_wcsrtombs,std::wcsrtombs
vcl_cwchar.h,vcl_wcsspn,std::wcsspn
vcl_cwchar.h,vcl_wcsstr,std::wcsstr
vcl_cwchar.h,vcl_wcstod,std::wcstod
vcl_cwchar.h,vcl_wcstok,std::wcstok
vcl_cwchar.h,vcl_wcstol,std::wcstol
vcl_cwchar.h,vcl_wcsxfrm,std::wcsxfrm
vcl_cwchar.h,vcl_wctob,std::wctob
vcl_cwchar.h,vcl_wctoul,std::wctoul
vcl_cwchar.h,vcl_wint_t,std::wint_t
vcl_cwchar.h,vcl_wmemchr,std::wmemchr
vcl_cwchar.h,vcl_wmemcmp,std::wmemcmp
vcl_cwchar.h,vcl_wmemcpy,std::wmemcpy
vcl_cwchar.h,vcl_wmemmove,std::wmemmove
vcl_cwchar.h,vcl_wmemset,std::wmemset
vcl_cwchar.h,vcl_wprintf,std::wprintf
vcl_cwchar.h,vcl_wscanf,std::wscanf
vcl_cwctype.h,vcl_iswalnum,std::iswalnum
vcl_cwctype.h,vcl_iswalpha,std::iswalpha
vcl_cwctype.h,vcl_iswcntrl,std::iswcntrl
vcl_cwctype.h,vcl_iswctrans,std::iswctrans
vcl_cwctype.h,vcl_iswctype,std::iswctype
vcl_cwctype.h,vcl_iswdigit,std::iswdigit
vcl_cwctype.h,vcl_iswgraph,std::iswgraph
vcl_cwctype.h,vcl_iswlower,std::iswlower
vcl_cwctype.h,vcl_iswprint,std::iswprint
vcl_cwctype.h,vcl_iswpunct,std::iswpunct
vcl_cwctype.h,vcl_iswspace,std::iswspace
vcl_cwctype.h,vcl_iswupper,std::iswupper
vcl_cwctype.h,vcl_iswxdigit,std::iswxdigit
vcl_cwctype.h,vcl_towctrans,std::towctrans
vcl_cwctype.h,vcl_towlower,std::towlower
vcl_cwctype.h,vcl_towupper,std::towupper
vcl_cwctype.h,vcl_wctrans,std::wctrans
vcl_cwctype.h,vcl_wctrans_t,std::wctrans_t
vcl_cwctype.h,vcl_wctype,std::wctype
vcl_cwctype.h,vcl_wctype_t,std::wctype_t
vcl_cwctype.h,vcl_wint_t,std::wint_t
vcl_deque.h,vcl_deque,std::deque
vcl_deque.h,vcl_swap,std::swap
vcl_exception.h,vcl_bad_exception,std::bad_exception
vcl_exception.h,vcl_exception,std::exception
vcl_fstream.h,vcl_filebuf,std::filebuf
vcl_fstream.h,vcl_fstream,std::fstream
vcl_fstream.h,vcl_ifstream,std::ifstream
vcl_fstream.h,vcl_ofstream,std::ofstream
vcl_functional.h,vcl_binary_function,std::binary_function
vcl_functional.h,vcl_binary_negate,std::binary_negate
vcl_functional.h,vcl_bind1st,std::bind1st
vcl_functional.h,vcl_bind2nd,std::bind2nd
vcl_functional.h,vcl_binder1st,std::binder1st
vcl_functional.h,vcl_binder2nd,std::binder2nd
vcl_functional.h,vcl_const_mem_fun,std::const_mem_fun
vcl_functional.h,vcl_const_mem_fun1,std::const_mem_fun1
vcl_functional.h,vcl_const_mem_fun1_ref,std::const_mem_fun1_ref
vcl_functional.h,vcl_const_mem_fun1_ref_t,std::const_mem_fun1_ref_t
vcl_functional.h,vcl_const_mem_fun1_t,std::const_mem_fun1_t
vcl_functional.h,vcl_const_mem_fun_ref,std::const_mem_fun_ref
vcl_functional.h,vcl_const_mem_fun_ref_t,std::const_mem_fun_ref_t
vcl_functional.h,vcl_const_mem_fun_t,std::const_mem_fun_t
vcl_functional.h,vcl_divides,std::divides
vcl_functional.h,vcl_equal_to,std::equal_to
vcl_functional.h,vcl_greater,std::greater
vcl_functional.h,vcl_greater_equal,std::greater_equal
vcl_functional.h,vcl_less,std::less
vcl_functional.h,vcl_less_equal,std::less_equal
vcl_functional.h,vcl_logical_and,std::logical_and
vcl_functional.h,vcl_logical_not,std::logical_not
vcl_functional.h,vcl_logical_or,std::logical_or
vcl_functional.h,vcl_mem_fun,std::mem_fun
vcl_functional.h,vcl_mem_fun1,std::mem_fun1
vcl_functional.h,vcl_mem_fun1_ref,std::mem_fun1_ref
vcl_functional.h,vcl_mem_fun1_ref_t,std::mem_fun1_ref_t
vcl_functional.h,vcl_mem_fun1_t,std::mem_fun1_t
vcl_functional.h,vcl_mem_fun_ref,std::mem_fun_ref
vcl_functional.h,vcl_mem_fun_ref_t,std::mem_fun_ref_t
vcl_functional.h,vcl_mem_fun_t,std::mem_fun_t
vcl_functional.h,vcl_minus,std::minus
vcl_functional.h,vcl_modulus,std::modulus
vcl_functional.h,vcl_multiplies,std::multiplies
vcl_functional.h,vcl_negate,std::negate
vcl_functional.h,vcl_not1,std::not1
vcl_functional.h,vcl_not2,std::not2
vcl_functional.h,vcl_not_equal_to,std::not_equal_to
vcl_functional.h,vcl_plus,std::plus
vcl_functional.h,vcl_pointer_to_binary_function,std::pointer_to_binary_function
vcl_functional.h,vcl_pointer_to_unary_function,std::pointer_to_unary_function
vcl_functional.h,vcl_ptr_fun,std::ptr_fun
vcl_functional.h,vcl_transform,std::transform
vcl_functional.h,vcl_unary_function,std::unary_function
vcl_functional.h,vcl_unary_negate,std::unary_negate
vcl_iomanip.h,vcl_boolalpha,std::boolalpha
vcl_iomanip.h,vcl_dec,std::dec
vcl_iomanip.h,vcl_fixed,std::fixed
vcl_iomanip.h,vcl_hex,std::hex
vcl_iomanip.h,vcl_internal,std::internal
vcl_iomanip.h,vcl_left,std::left
vcl_iomanip.h,vcl_noboolalpha,std::noboolalpha
vcl_iomanip.h,vcl_noshowbase,std::noshowbase
vcl_iomanip.h,vcl_noshowpoint,std::noshowpoint
vcl_iomanip.h,vcl_noshowpos,std::noshowpos
vcl_iomanip.h,vcl_noskipws,std::noskipws
vcl_iomanip.h,vcl_nouppercase,std::nouppercase
vcl_iomanip.h,vcl_oct,std::oct
vcl_iomanip.h,vcl_resetiosflags,std::resetiosflags
vcl_iomanip.h,vcl_right,std::right
vcl_iomanip.h,vcl_scientific,std::scientific
vcl_iomanip.h,vcl_setbase,std::setbase
vcl_iomanip.h,vcl_setfill,std::setfill
vcl_iomanip.h,vcl_setiosflags,std::setiosflags
vcl_iomanip.h,vcl_setprecision,std::setprecision
vcl_iomanip.h,vcl_setw,std::setw
vcl_iomanip.h,vcl_showbase,std::showbase
vcl_iomanip.h,vcl_showpoint,std::showpoint
vcl_iomanip.h,vcl_showpos,std::showpos
vcl_iomanip.h,vcl_skipws,std::skipws
vcl_iomanip.h,vcl_uppercase,std::uppercase
vcl_ios.h,vcl_basic_ios,std::basic_ios
vcl_ios.h,vcl_fpos,std::fpos
vcl_ios.h,vcl_ios_adjustfield,std::ios::adjustfield
vcl_ios.h,vcl_ios_base,std::ios_base
vcl_ios.h,vcl_ios_basefield,std::ios::basefield
vcl_ios.h,vcl_ios_beg,std::ios::beg
vcl_ios.h,vcl_ios_boolalpha,std::ios::boolalpha
vcl_ios.h,vcl_ios_cur,std::ios::cur
vcl_ios.h,vcl_ios_dec,std::ios::dec
vcl_ios.h,vcl_ios_end,std::ios::end
vcl_ios.h,vcl_ios_fixed,std::ios::fixed
vcl_ios.h,vcl_ios_floatfield,std::ios::floatfield
vcl_ios.h,vcl_ios_fmtflags,std::ios::fmtflags
vcl_ios.h,vcl_ios_hex,std::ios::hex
vcl_ios.h,vcl_ios_internal,std::ios::internal
vcl_ios.h,vcl_ios_left,std::ios::left
vcl_ios.h,vcl_ios_noboolalpha,std::ios::noboolalpha
vcl_ios.h,vcl_ios_noshowbase,std::ios::noshowbase
vcl_ios.h,vcl_ios_noshowpoint,std::ios::noshowpoint
vcl_ios.h,vcl_ios_noshowpos,std::ios::noshowpos
vcl_ios.h,vcl_ios_noskipws,std::ios::noskipws
vcl_ios.h,vcl_ios_nouppercase,std::ios::nouppercase
vcl_ios.h,vcl_ios_oct,std::ios::oct
vcl_ios.h,vcl_ios_right,std::ios::right
vcl_ios.h,vcl_ios_scientific,std::ios::scientific
vcl_ios.h,vcl_ios_seekdir,std::ios::seekdir
vcl_ios.h,vcl_ios_showbase,std::ios::showbase
vcl_ios.h,vcl_ios_showpoint,std::ios::showpoint
vcl_ios.h,vcl_ios_showpos,std::ios::showpos
vcl_ios.h,vcl_ios_skipws,std::ios::skipws
vcl_ios.h,vcl_ios_uppercase,std::ios::uppercase
vcl_ios.h,vcl_streamoff,std::streamoff
vcl_ios.h,vcl_streamsize,std::streamsize
vcl_iosfwd.h,vcl_allocator,std::allocator
vcl_iosfwd.h,vcl_basic_filebuf,std::basic_filebuf
vcl_iosfwd.h,vcl_basic_fstream,std::basic_fstream
vcl_iosfwd.h,vcl_basic_ifstream,std::basic_ifstream
vcl_iosfwd.h,vcl_basic_ios,std::basic_ios
vcl_iosfwd.h,vcl_basic_iostream,std::basic_iostream
vcl_iosfwd.h,vcl_basic_istream,std::basic_istream
vcl_iosfwd.h,vcl_basic_istringstream,std::basic_istringstream
vcl_iosfwd.h,vcl_basic_ofstream,std::basic_ofstream
vcl_iosfwd.h,vcl_basic_ostream,std::basic_ostream
vcl_iosfwd.h,vcl_basic_ostringstream,std::basic_ostringstream
vcl_iosfwd.h,vcl_basic_streambuf,std::basic_streambuf
vcl_iosfwd.h,vcl_char_traits,std::char_traits
vcl_iosfwd.h,vcl_filebuf,std::filebuf
vcl_iosfwd.h,vcl_fpos,std::fpos
vcl_iosfwd.h,vcl_fstream,std::fstream
vcl_iosfwd.h,vcl_ifstream,std::ifstream
vcl_iosfwd.h,vcl_ios,std::ios
vcl_iosfwd.h,vcl_iostream,std::iostream
vcl_iosfwd.h,vcl_istream,std::istream
vcl_iosfwd.h,vcl_istreambuf_iterator,std::istreambuf_iterator
vcl_iosfwd.h,vcl_ofstream,std::ofstream
vcl_iosfwd.h,vcl_ostream,std::ostream
vcl_iosfwd.h,vcl_ostreambuf_iterator,std::ostreambuf_iterator
vcl_iosfwd.h,vcl_streambuf,std::streambuf
vcl_iosfwd.h,vcl_streamoff,std::streamoff
vcl_iosfwd.h,vcl_streampos,std::streampos
vcl_iosfwd.h,vcl_stringstream,std::stringstream
vcl_iosfwd.h,vcl_wfilebuf,std::wfilebuf
vcl_iosfwd.h,vcl_wfstream,std::wfstream
vcl_iosfwd.h,vcl_wifstream,std::wifstream
vcl_iosfwd.h,vcl_wios,std::wios
vcl_iosfwd.h,vcl_wiostream,std::wiostream
vcl_iosfwd.h,vcl_wistream,std::wistream
vcl_iosfwd.h,vcl_wistringstream,std::wistringstream
vcl_iosfwd.h,vcl_wofstream,std::wofstream
vcl_iosfwd.h,vcl_wostream,std::wostream
vcl_iosfwd.h,vcl_wostringstream,std::wostringstream
vcl_iosfwd.h,vcl_wstreambuf,std::wstreambuf
vcl_iosfwd.h,vcl_wstreampos,std::wstreampos
vcl_iosfwd.h,vcl_wstringbuf,std::wstringbuf
vcl_iosfwd.h,vcl_wstringstream,std::wstringstream
vcl_iostream.h,vcl_cerr,std::cerr
vcl_iostream.h,vcl_cin,std::cin
vcl_iostream.h,vcl_clog,std::clog
vcl_iostream.h,vcl_cout,std::cout
vcl_iostream.h,vcl_dec,std::dec
vcl_iostream.h,vcl_endl,std::endl
vcl_iostream.h,vcl_ends,std::ends
vcl_iostream.h,vcl_flush,std::flush
vcl_iostream.h,vcl_hex,std::hex
vcl_iostream.h,vcl_ios_app,std::ios::app
vcl_iostream.h,vcl_ios_ate,std::ios::ate
vcl_iostream.h,vcl_ios_binary,std::ios::binary
vcl_iostream.h,vcl_ios_in,std::ios::in
vcl_iostream.h,vcl_ios_openmode,std::ios::openmode
vcl_iostream.h,vcl_ios_out,std::ios::out
vcl_iostream.h,vcl_ios_trunc,std::ios::trunc
vcl_iostream.h,vcl_oct,std::oct
vcl_iostream.h,vcl_ostream,std::ostream
vcl_iostream.h,vcl_streambuf,std::streambuf
vcl_iostream.h,vcl_streampos,std::streampos
vcl_iostream.h,vcl_wcout,std::wcout
vcl_iostream.h,vcl_ws,std::ws
vcl_istream.h,vcl_basic_iostream,std::basic_iostream
vcl_istream.h,vcl_basic_istream,std::basic_istream
vcl_istream.h,vcl_iostream,std::iostream
vcl_istream.h,vcl_istream,std::istream
vcl_istream.h,vcl_wiostream,std::wiostream
vcl_istream.h,vcl_wistream,std::wistream
vcl_iterator.h,vcl_advance,std::advance
vcl_iterator.h,vcl_back_insert_iterator,std::back_insert_iterator
vcl_iterator.h,vcl_back_inserter,std::back_inserter
vcl_iterator.h,vcl_bidirectional_iterator_tag,std::bidirectional_iterator_tag
vcl_iterator.h,vcl_distance,std::distance
vcl_iterator.h,vcl_forward_iterator_tag,std::forward_iterator_tag
vcl_iterator.h,vcl_front_insert_iterator,std::front_insert_iterator
vcl_iterator.h,vcl_front_inserter,std::front_inserter
vcl_iterator.h,vcl_input_iterator_tag,std::input_iterator_tag
vcl_iterator.h,vcl_insert_iterator,std::insert_iterator
vcl_iterator.h,vcl_inserter,std::inserter
vcl_iterator.h,vcl_istream_iterator,std::istream_iterator
vcl_iterator.h,vcl_istreambuf_iterator,std::istreambuf_iterator
vcl_iterator.h,vcl_iterator,std::iterator
vcl_iterator.h,vcl_iterator_traits,std::iterator_traits
vcl_iterator.h,vcl_ostream_iterator,std::ostream_iterator
vcl_iterator.h,vcl_ostreambuf_iterator,std::ostreambuf_iterator
vcl_iterator.h,vcl_output_iterator_tag,std::output_iterator_tag
vcl_iterator.h,vcl_random_access_iterator_tag,std::random_access_iterator_tag
vcl_iterator.h,vcl_reverse_iterator,std::reverse_iterator
vcl_limits.h,vcl_float_denorm_style,std::float_denorm_style
vcl_limits.h,vcl_float_round_style,std::float_round_style
vcl_limits.h,vcl_numeric_limits,std::numeric_limits
vcl_limits.h,vcl_round_toward_neg_infinity,std::round_toward_neg_infinity
vcl_limits.h,vcl_round_toward_zero,std::round_toward_zero
vcl_list.h,vcl_list,std::list
vcl_list.h,vcl_swap,std::swap
vcl_locale.h,vcl_codecvt,std::codecvt
vcl_locale.h,vcl_codecvt_base,std::codecvt_base
vcl_locale.h,vcl_codecvt_byname,std::codecvt_byname
vcl_locale.h,vcl_collate,std::collate
vcl_locale.h,vcl_collate_byname,std::collate_byname
vcl_locale.h,vcl_ctype,std::ctype
vcl_locale.h,vcl_has_facet,std::has_facet
vcl_locale.h,vcl_isalnum,std::isalnum
vcl_locale.h,vcl_isalpha,std::isalpha
vcl_locale.h,vcl_iscntrl,std::iscntrl
vcl_locale.h,vcl_isdigit,std::isdigit
vcl_locale.h,vcl_isgraph,std::isgraph
vcl_locale.h,vcl_islower,std::islower
vcl_locale.h,vcl_isprint,std::isprint
vcl_locale.h,vcl_ispunct,std::ispunct
vcl_locale.h,vcl_isspace,std::isspace
vcl_locale.h,vcl_isupper,std::isupper
vcl_locale.h,vcl_isxdigit,std::isxdigit
vcl_locale.h,vcl_messages,std::messages
vcl_locale.h,vcl_messages_byname,std::messages_byname
vcl_locale.h,vcl_money_get,std::money_get
vcl_locale.h,vcl_money_put,std::money_put
vcl_locale.h,vcl_moneypunct,std::moneypunct
vcl_locale.h,vcl_moneypunct_byname,std::moneypunct_byname
vcl_locale.h,vcl_num_get,std::num_get
vcl_locale.h,vcl_num_put,std::num_put
vcl_locale.h,vcl_numpunct,std::numpunct
vcl_locale.h,vcl_numpunct_byname,std::numpunct_byname
vcl_locale.h,vcl_time_get,std::time_get
vcl_locale.h,vcl_time_get_byname,std::time_get_byname
vcl_locale.h,vcl_time_put,std::time_put
vcl_locale.h,vcl_time_put_byname,std::time_put_byname
vcl_locale.h,vcl_tolower,std::tolower
vcl_locale.h,vcl_toupper,std::toupper
vcl_locale.h,vcl_use_facet,std::use_facet
vcl_map.h,vcl_map,std::map
vcl_map.h,vcl_multimap,std::multimap
vcl_map.h,vcl_swap,std::swap
vcl_memory.h,vcl_allocator,std::allocator
vcl_memory.h,vcl_auto_ptr,std::auto_ptr
vcl_memory.h,vcl_get_temporary_buffer,std::get_temporary_buffer
vcl_memory.h,vcl_raw_storage_iterator,std::raw_storage_iterator
vcl_memory.h,vcl_return_temporary_buffer,std::return_temporary_buffer
vcl_memory.h,vcl_uninitialized_copy,std::uninitialized_copy
vcl_memory.h,vcl_uninitialized_fill,std::uninitialized_fill
vcl_memory.h,vcl_uninitialized_fill_n,std::uninitialized_fill_n
vcl_new.h,vcl_bad_alloc,std::bad_alloc
vcl_new.h,vcl_set_new_handler,std::set_new_handler
vcl_numeric.h,vcl_accumulate,std::accumulate
vcl_numeric.h,vcl_adjacent_difference,std::adjacent_difference
vcl_numeric.h,vcl_inner_product,std::inner_product
vcl_numeric.h,vcl_partial_sum,std::partial_sum
vcl_ostream.h,vcl_basic_ostream,std::basic_ostream
vcl_ostream.h,vcl_endl,std::endl
vcl_ostream.h,vcl_ends,std::ends
vcl_ostream.h,vcl_flush,std::flush
vcl_ostream.h,vcl_ostream,std::ostream
vcl_ostream.h,vcl_wostream,std::wostream
vcl_queue.h,vcl_priority_queue,std::priority_queue
vcl_queue.h,vcl_queue,std::queue
vcl_set.h,vcl_multiset,std::multiset
vcl_set.h,vcl_set,std::set
vcl_set.h,vcl_swap,std::swap
vcl_sstream.h,vcl_basic_stringbuf,std::basic_stringbuf
vcl_sstream.h,vcl_istringstream,std::istringstream
vcl_sstream.h,vcl_ostringstream,std::ostringstream
vcl_sstream.h,vcl_stringbuf,std::stringbuf
vcl_sstream.h,vcl_stringstream,std::stringstream
vcl_sstream.h,vcl_wstringbuf,std::wstringbuf
vcl_stack.h,vcl_stack,std::stack
vcl_stdexcept.h,vcl_domain_error,std::domain_error
vcl_stdexcept.h,vcl_invalid_argument,std::invalid_argument
vcl_stdexcept.h,vcl_length_error,std::length_error
vcl_stdexcept.h,vcl_logic_error,std::logic_error
vcl_stdexcept.h,vcl_out_of_range,std::out_of_range
vcl_stdexcept.h,vcl_overflow_error,std::overflow_error
vcl_stdexcept.h,vcl_range_error,std::range_error
vcl_stdexcept.h,vcl_runtime_error,std::runtime_error
vcl_stdexcept.h,vcl_underflow_error,std::underflow_error
vcl_streambuf.h,vcl_basic_streambuf,std::basic_streambuf
vcl_streambuf.h,vcl_streambuf,std::streambuf
vcl_string.h,vcl_basic_string,std::basic_string
vcl_string.h,vcl_char_traits,std::char_traits
vcl_string.h,vcl_getline,std::getline
vcl_string.h,vcl_string,std::string
vcl_string.h,vcl_swap,std::swap
vcl_string.h,vcl_wstring,std::wstring
vcl_typeinfo.h,vcl_bad_cast,std::bad_cast
vcl_typeinfo.h,vcl_bad_typeid,std::bad_typeid
vcl_typeinfo.h,vcl_type_info,std::type_info
vcl_utility.h,vcl_make_pair,std::make_pair
vcl_utility.h,vcl_pair,std::pair
vcl_valarray.h,vcl_abs,std::abs
vcl_valarray.h,vcl_acos,std::acos
vcl_valarray.h,vcl_asin,std::asin
vcl_valarray.h,vcl_atan,std::atan
vcl_valarray.h,vcl_atan2,std::atan2
vcl_valarray.h,vcl_cos,std::cos
vcl_valarray.h,vcl_cosh,std::cosh
vcl_valarray.h,vcl_exp,std::exp
vcl_valarray.h,vcl_gslice,std::gslice
vcl_valarray.h,vcl_gslice_array,std::gslice_array
vcl_valarray.h,vcl_indirect_array,std::indirect_array
vcl_valarray.h,vcl_log,std::log
vcl_valarray.h,vcl_log10,std::log10
vcl_valarray.h,vcl_mask_array,std::mask_array
vcl_valarray.h,vcl_pow,std::pow
vcl_valarray.h,vcl_sin,std::sin
vcl_valarray.h,vcl_sinh,std::sinh
vcl_valarray.h,vcl_slice,std::slice
vcl_valarray.h,vcl_slice_array,std::slice_array
vcl_valarray.h,vcl_sqrt,std::sqrt
vcl_valarray.h,vcl_tan,std::tan
vcl_valarray.h,vcl_tanh,std::tanh
vcl_valarray.h,vcl_valarray,std::valarray
vcl_vector.h,vcl_swap,std::swap
vcl_vector.h,vcl_vector,std::vector
vcl_cerrno.h,vcl_cerr,std::cerr
vcl_exception.h,vcl_throw,throw
vcl_exception.h,vcl_try,try
vcl_exception.h,vcl_catch_all,catch(...)
vcl_exception.h,vcl_catch,catch
vcl_ios.h,vcl_ios,std::ios
"""

vcl_replace_head_names = OrderedDict()
vcl_replace_functionnames = OrderedDict()
vcl_replace_manual = OrderedDict()


for line in info_for_conversion.splitlines():
    linevalues = line.split(",")
    if len(linevalues) != 3:
        #print("SKIPPING: " + str(linevalues))
        continue
    fname = linevalues[0]
    new_name = fname.replace("vcl_", "").replace(".h", "")
    vcl_replace_head_names['#include "{0}"'.format(
        fname)] = '#include "{0}"'.format(new_name)
    vcl_replace_head_names['#include <{0}>'.format(
        fname)] = '#include <{0}>'.format(new_name)
    vcl_pat = linevalues[1]
    new_pat = linevalues[2]
    vcl_replace_functionnames[vcl_pat] = new_pat
    # Need to fix the fact that both std::ios is a base and a prefix
    if "std::ios::" in new_pat:
        vcl_replace_manual[new_pat.replace(
            "std::ios::", "std::ios_")] = new_pat

# print(vcl_replace_head_names)
# print(vcl_replace_functionnames)

cfile = sys.argv[1]

file_as_string = ""
with open(cfile, "r") as rfp:
    file_as_string = rfp.read()
orig_file = file_as_string

if file_as_string.find("std::cout") or file_as_string.find("std::cerr") or file_as_string.find("std::cin"):
    required_header = "#include <vcl_compiler.h>\n#include <iostream>\n"
else:
    required_header = "#include <vcl_compiler.h>\n"

for searchval, replaceval in vcl_replace_head_names.items():
    file_as_string_new = file_as_string.replace(
        searchval, required_header + replaceval)
    if file_as_string_new != file_as_string:
        required_header = ""
    file_as_string = file_as_string_new


for searchval, replaceval in vcl_replace_functionnames.items():
    file_as_string = file_as_string.replace(searchval, replaceval)
for searchval, replaceval in vcl_replace_manual.items():
    file_as_string = file_as_string.replace(searchval, replaceval)

if orig_file != file_as_string:
    print("Processing: " + cfile)
    with open(cfile, "w") as wfp:
        wfp.write(file_as_string)
else:
    print("NO CHANGES NEEDED: " + cfile)
