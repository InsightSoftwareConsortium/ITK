// This is vcl/emulation/vcl_stlfwd.h
#ifndef vcl_emulation_stlfwd_h
#define vcl_emulation_stlfwd_h
// include vcl_compiler.h to get the settings for this system and compiler
#include <vcl_compiler.h>

#include "vcl_stlconf.h"

// This section contains the abbreviations used throughout the SGI STL when
// __STL_USE_ABBREVS is defined. Including this file before forward
// references of STL containers allows files to use STL abbreviated names
// without including the actual STL header files. Although most of these defines
// will never be used by most programs, all the abbreviations are included for
// completeness.

// pass shortened names to all files, if used
#ifdef __STL_USE_ABBREVS

// from vcl_alloc.h:
#include "vcl_alloc.h"

// from vcl_deque.h:
# define __deque_iterator         dQIt
# define __deque_const_iterator   dQcIt

// from vcl_hashtable.h:
# define __hashtable_iterator         hTIt
# define __hashtable_const_iterator   hTcIt
# define __hashtable_node             hTN
# define __hashtable_base             hTB
# define vcl_hashtable                hT

// from iterator.h:
// ugliness is intentional - to reduce conflicts
# define vcl_input_iterator_tag             InItT
# define vcl_output_iterator_tag            OuItT
# define vcl_bidirectional_iterator_tag     BdItT
# define vcl_random_access_iterator_tag     RaItT
# define vcl_forward_iterator               FwIt
# define vcl_input_iterator                 InIt
# define vcl_output_iterator                OuIt
# define vcl_bidirectional_iterator         BdIt
# define vcl_random_access_iterator         RaIt
# define vcl_reverse_bidirectional_iterator rBdIt
# define vcl_reverse_iterator               rIt
# define vcl_back_insert_iterator           bIIt
# define vcl_front_insert_iterator          fIIt
# define vcl_raw_storage_iterator           rSIt
# define vcl_istream_iterator               iSIt
# define vcl_ostream_iterator               oSIt

// from vcl_list.h:
# define __list_iterator         LIt
# define __list_const_iterator   LcIt

#if 0
// from pthreah_alloc.h:
# define __pthread_alloc_template   pTHr_Al
#endif

// from vcl_tree.h:
// ugliness is intentional - to reduce conflicts possibility
# define __rb_tree_node_base       rbTNB
# define __rb_tree_node            rbTN
# define __rb_tree_base_iterator   rbTBIt
# define __rb_tree_iterator        rbTIt
# define __rb_tree_const_iterator  rbTcIt
# define __rb_tree_base            rbTB

// from vcl_string.h:
# undef  vcl_char_traits
# define vcl_char_traits sCt
# undef  vcl_basic_string
# define vcl_basic_string bS

#endif //__STL_USE_ABBREVS

// forward declare the default vcl_allocator, which is used by almost every
// STL container

#ifdef __STL_USE_NEWALLOC

typedef __new_alloc<0> vcl_alloc;
// forward declare the default vcl_allocator, since it is used in almost
// every STL container declaration
#ifndef vcl_emulation_alloc_h
template <int inst> class __new_alloc;
#endif

#else

// linux uses malloc
# ifdef __STL_USE_MALLOC

// forward declare the default vcl_allocator, since it is used in almost
// every STL container declaration
#ifndef vcl_emulation_alloc_h
template <int inst> class __malloc_alloc;
#endif
typedef __malloc_alloc<0> vcl_alloc;

# else // the default vcl_allocator case - used by g++ and VC50

// forward declare the default vcl_allocator, since it is used in almost
// every STL container declaration
#ifndef vcl_emulation_alloc_h
template <bool threads, int inst> class __alloc;
#endif

// threads require locking for the vcl_allocator
# if !defined (_NOTHREADS) && (defined (_PTHREADS) || defined (__STL_SGI_THREADS) || defined (__STL_WIN32THREADS))
typedef __alloc<true,0> vcl_alloc;
# else
typedef __alloc<false,0> vcl_alloc;
# endif

#endif

#endif

#if defined (VCL_GCC_EGCS)
#undef VCL_DFL_TYPE_PARAM_STLDECL
#define VCL_DFL_TYPE_PARAM_STLDECL(A,a) class A
#endif

// forward declarations of all STL container classes and support function objects

#ifndef vcl_emulation_function_h
    template <class T> struct vcl_less;
    template <class T> struct vcl_equal_to;
#endif

#ifndef vcl_emulation_vector_h
# if !defined(VECTOR_H) && !defined(__STL_DEFAULT_TYPE_PARAM) && ( !defined(__STL_NAMESPACES) || defined(__STL_NO_NAMESPACES))
    template <class T> class vcl_vector;
# else
    template <class T, VCL_DFL_TYPE_PARAM_STLDECL(Alloc,vcl_alloc) > class vcl_vector;
# endif
#endif

#ifndef vcl_emulation_deque_h
# if !defined ( __STL_DEFAULT_TYPE_PARAM)
    template <class T> class vcl_deque;
# else
    template <class T, VCL_DFL_TYPE_PARAM_STLDECL(Alloc,vcl_alloc) > class vcl_deque;
# endif
#endif

#ifndef vcl_emulation_list_h
# if !defined ( __STL_DEFAULT_TYPE_PARAM )
    template <class T> class vcl_list;
# else
    template <class T, VCL_DFL_TYPE_PARAM_STLDECL(Alloc,vcl_alloc) > class vcl_list;
# endif
#endif

#ifndef vcl_emulation_stack_h
    // stack, queue, and vcl_priority_queue are defined in stack.h
    template <class T, VCL_DFL_TMPL_PARAM_STLDECL(Sequence,vcl_vector<T>) > class stack;
    template <class T, VCL_DFL_TMPL_PARAM_STLDECL(Sequence,vcl_deque<T>) > class queue;
    template <class T, VCL_DFL_TMPL_PARAM_STLDECL(Sequence,vcl_vector<T>),
          VCL_DFL_TMPL_PARAM_STLDECL(Compare,vcl_less<typename Sequence::value_type>) > class  vcl_priority_queue;
#endif

#ifndef vcl_emulation_set_h
# ifndef __STL_DEFAULT_TYPE_PARAM
    template <class Key, class Compare> class vcl_set;
# else
    template <class Key, VCL_DFL_TMPL_PARAM_STLDECL(Compare,vcl_less<Key>),
                        VCL_DFL_TYPE_PARAM_STLDECL(Alloc,vcl_alloc) > class vcl_set;
# endif
#endif

#ifndef vcl_emulation_map_h
# ifndef __STL_DEFAULT_TYPE_PARAM
    template <class Key, class T, class Compare> class vcl_map;
# else
    template <class Key, class T, VCL_DFL_TMPL_PARAM_STLDECL(Compare,vcl_less<Key>),
                                  VCL_DFL_TYPE_PARAM_STLDECL(Alloc,vcl_alloc) > class vcl_map;
# endif
#endif

#ifndef vcl_emulation_multiset_h
# ifndef __STL_DEFAULT_TYPE_PARAM
    template <class Key, class Compare> class vcl_multiset;
# else
    template <class Key, VCL_DFL_TMPL_PARAM_STLDECL(Compare,vcl_less<Key>),
                         VCL_DFL_TYPE_PARAM_STLDECL(Alloc,vcl_alloc) > class vcl_multiset;
# endif
#endif

#ifndef vcl_emulation_multimap_h
# if !defined(MULTIMAP_H) && !defined(__STL_DEFAULT_TYPE_PARAM) &&  ( !defined(__STL_NAMESPACES) || defined(__STL_NO_NAMESPACES) )
    template <class Key, class T, class Compare> class vcl_multimap;
# else
    template <class Key, class T, VCL_DFL_TMPL_PARAM_STLDECL(Compare,vcl_less<Key>),
                                  VCL_DFL_TYPE_PARAM_STLDECL(Alloc,vcl_alloc) > class vcl_multimap;
# endif
#endif

#ifndef vcl_emulation_hashtable_h
    template <class Key> struct hash;
#endif

#ifndef vcl_emulation_hash_set_h
# ifndef __STL_DEFAULT_TYPE_PARAM
    template <class Value, class HashFcn, class EqualKey > class vcl_hash_set;

    template <class Value, class HashFcn, class EqualKey > class vcl_hash_multiset;
# else
    template <class Value, VCL_DFL_TMPL_PARAM_STLDECL(HashFcn,hash<Value>),
              VCL_DFL_TMPL_PARAM_STLDECL(EqualKey,vcl_equal_to<Value>),
              VCL_DFL_TYPE_PARAM_STLDECL(Alloc,vcl_alloc) > class vcl_hash_set;

    template <class Value, VCL_DFL_TMPL_PARAM_STLDECL(HashFcn,hash<Value>),
              VCL_DFL_TMPL_PARAM_STLDECL(EqualKey,vcl_equal_to<Value>),
              VCL_DFL_TYPE_PARAM_STLDECL(Alloc,vcl_alloc) > class vcl_hash_multiset;
# endif
#endif

#ifndef vcl_emulation_hash_map_h
# ifndef __STL_DEFAULT_TYPE_PARAM
    template <class Key, class T, class HashFcn, class EqualKey > class vcl_hash_map;
    template <class Key, class T, class HashFcn, class EqualKey > class vcl_hash_multimap;
# else
    template <class Key, class T, VCL_DFL_TMPL_PARAM_STLDECL(HashFcn,hash<Key>),
              VCL_DFL_TMPL_PARAM_STLDECL(EqualKey,vcl_equal_to<Key>),
              VCL_DFL_TYPE_PARAM_STLDECL(Alloc,vcl_alloc) > class vcl_hash_map;
    template <class Key, class T, VCL_DFL_TMPL_PARAM_STLDECL(HashFcn,hash<Key>),
              VCL_DFL_TMPL_PARAM_STLDECL(EqualKey,vcl_equal_to<Key>),
              VCL_DFL_TYPE_PARAM_STLDECL(Alloc,vcl_alloc) > class vcl_hash_multimap;
# endif
#endif

#ifndef vcl_emulation_pair_h
template <class T1, class T2> class vcl_pair;
#endif

#endif // vcl_emulation_stlfwd_h
