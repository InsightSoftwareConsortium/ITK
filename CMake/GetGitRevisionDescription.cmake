# - Returns a version string from Git
#
# These functions force a re-configure on each git commit so that you can
# trust the values of the variables in your build system.
#
#  get_git_head_revision(<refvar> <hashvar> [<additonal arguments to git describe> ...])
#
# Returns the ref and sha hash of the current head revision
#
#  git_describe(<var> [<additonal arguments to git describe> ...])
#
# Returns the results of git describe on the source tree, and adjusting
# the output so that it tests false if an error occurs.
#
#  git_get_exact_tag(<var> [<additonal arguments to git describe> ...])
#
# Returns the results of git describe --exact-match on the source tree,
# and adjusting the output so that it tests false if there was no exact
# matching tag.
#
# Requires CMake 2.6 or newer (uses the 'function' command)
#
# Original Author:
# 2009-2010 Ryan Pavlik <rpavlik@iastate.edu> <abiryan@ryand.net>
# http://academic.cleardefinition.com
# Iowa State University HCI Graduate Program/VRAC
#
# Copyright Iowa State University 2009-2010.
# Distributed under the Boost Software License, Version 1.0.
# (See accompanying file LICENSE_1_0.txt or copy at
# http://www.boost.org/LICENSE_1_0.txt)

if(__get_git_revision_description)
    return()
endif()
set(__get_git_revision_description YES)

# We must run the following at "include" time, not at function call time,
# to find the path to this module rather than the path to a calling list file
get_filename_component(_gitdescmoddir ${CMAKE_CURRENT_LIST_FILE} PATH)

find_package(Git QUIET)

function(get_git_head_revision _refvar _hashvar)
    if(NOT GIT_EXECUTABLE)
        set(${_refvar} "GIT-NOTFOUND" PARENT_SCOPE)
        set(${_hashvar} "GIT-NOTFOUND" PARENT_SCOPE)
        return()
    endif()

    set(src_dir ${PROJECT_SOURCE_DIR})
    set(bin_dir ${PROJECT_BINARY_DIR})

    execute_process(COMMAND ${GIT_EXECUTABLE} rev-parse --git-dir
        WORKING_DIRECTORY ${src_dir}
        OUTPUT_VARIABLE GIT_DIR
        ERROR_VARIABLE error
        RESULT_VARIABLE failed
        OUTPUT_STRIP_TRAILING_WHITESPACE
        )

    if(NOT IS_ABSOLUTE "${GIT_DIR}")
        set(GIT_DIR "${src_dir}/${GIT_DIR}")
    endif()
    if(failed OR NOT EXISTS "${GIT_DIR}/HEAD")
        # not in git
        set(${_refvar} "GITDIR-NOTFOUND" PARENT_SCOPE)
        set(${_hashvar} "GITDIR-NOTFOUND" PARENT_SCOPE)
        return()
    endif()
    set(GIT_DATA "${bin_dir}/CMakeFiles/git-data")
    if(NOT EXISTS "${GIT_DATA}")
        file(MAKE_DIRECTORY "${GIT_DATA}")
    endif()
    configure_file("${GIT_DIR}/HEAD" "${GIT_DATA}/HEAD" COPYONLY)

    file(STRINGS "${GIT_DIR}/HEAD" head LIMIT_COUNT 1 LIMIT_INPUT 1024)
    if("${head}" MATCHES "^ref: (.*)$")
        set(HEAD_REF "${CMAKE_MATCH_1}")
        if(EXISTS "${GIT_DIR}/${HEAD_REF}")
            configure_file("${GIT_DIR}/${HEAD_REF}" "${GIT_DATA}/HEAD-REF" COPYONLY)
        endif()
    else()
        set(HEAD_REF "")
    endif()
    execute_process(COMMAND ${GIT_EXECUTABLE} rev-parse HEAD
        WORKING_DIRECTORY ${src_dir}
        OUTPUT_VARIABLE HEAD_HASH OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_VARIABLE error
        RESULT_VARIABLE failed)
    if(failed)
        set(HEAD_HASH "HEAD-HASH-NOTFOUND")
    endif()
    set(${_refvar} "${HEAD_REF}" PARENT_SCOPE)
    set(${_hashvar} "${HEAD_HASH}" PARENT_SCOPE)
endfunction()

# get the number of commits since the file has last been modified
function(git_commits_since file _commits )
  get_git_head_revision(ref head)

  set(src_dir ${PROJECT_SOURCE_DIR})

  execute_process(COMMAND ${GIT_EXECUTABLE} rev-list ${head} -n 1 -- ${file}
    WORKING_DIRECTORY ${src_dir}
    OUTPUT_VARIABLE tag OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_VARIABLE error
    RESULT_VARIABLE failed
    )
  if(failed)
    set( tag "")
  endif()

  execute_process(COMMAND ${GIT_EXECUTABLE} rev-list ${tag}..${head}
    WORKING_DIRECTORY ${src_dir}
    OUTPUT_VARIABLE rev_list OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_VARIABLE error
    RESULT_VARIABLE failed
    )

  if(failed)
    set( rev_list "")
  endif()

  string( REGEX MATCHALL "[a-fA-F0-9]+" rev_list "${rev_list}")
  list( LENGTH rev_list COUNT)

  set(${_commits} "${COUNT}" PARENT_SCOPE)
endfunction()

function(git_describe _var)
    get_git_head_revision(refspec hash)
    if(NOT GIT_FOUND)
        set(${_var} "GIT-NOTFOUND"  PARENT_SCOPE)
        return()
    endif()
    if(NOT hash)
        set(${_var} "HEAD-HASH-NOTFOUND"  PARENT_SCOPE)
        return()
    endif()

    set(src_dir ${PROJECT_SOURCE_DIR})

    # TODO sanitize
    #if((${ARGN}" MATCHES "&&") OR
    #    (ARGN MATCHES "||") OR
    #    (ARGN MATCHES "\\;"))
    #    message("Please report the following error to the project!")
    #    message(FATAL_ERROR "Looks like someone's doing something nefarious with git_describe! Passed arguments ${ARGN}")
    #endif()

    #message(STATUS "Arguments to execute_process: ${ARGN}")

    execute_process(COMMAND "${GIT_EXECUTABLE}" describe ${hash} ${ARGN}
        WORKING_DIRECTORY "${src_dir}"
        RESULT_VARIABLE res
        OUTPUT_VARIABLE out
        ERROR_QUIET
        OUTPUT_STRIP_TRAILING_WHITESPACE)
    if(NOT res EQUAL 0)
        set(out "${out}-${res}-NOTFOUND")
    endif()

    set(${_var} "${out}" PARENT_SCOPE)
endfunction()

function(git_get_exact_tag _var)
    git_describe(out --exact-match ${ARGN})
    set(${_var} "${out}" PARENT_SCOPE)
endfunction()

function(git_head_date _var)
    get_git_head_revision(refspec hash)
    if(NOT GIT_FOUND)
        set(${_var} "GIT-NOTFOUND"  PARENT_SCOPE)
        return()
    endif()
    if(NOT hash)
        set(${_var} "HEAD-HASH-NOTFOUND"  PARENT_SCOPE)
        return()
    endif()

    set(src_dir ${PROJECT_SOURCE_DIR})
    execute_process(COMMAND "${GIT_EXECUTABLE}" show -s --format=%ci ${hash} ${ARGN}
        WORKING_DIRECTORY "${src_dir}"
        RESULT_VARIABLE res
        OUTPUT_VARIABLE out
        ERROR_QUIET
        OUTPUT_STRIP_TRAILING_WHITESPACE)
    if(NOT res EQUAL 0)
        set(out "${out}-${res}-NOTFOUND")
    endif()

    set(${_var} "${out}" PARENT_SCOPE)

endfunction()

function(git_head_date _var)
    get_git_head_revision(refspec hash)
    if(NOT GIT_FOUND)
        set(${_var} "GIT-NOTFOUND"  PARENT_SCOPE)
        return()
    endif()
    if(NOT hash)
        set(${_var} "HEAD-HASH-NOTFOUND"  PARENT_SCOPE)
        return()
    endif()

    set(src_dir ${PROJECT_SOURCE_DIR})
    execute_process(COMMAND "${GIT_EXECUTABLE}" show -s --format=%ci ${hash} ${ARGN}
        WORKING_DIRECTORY "${src_dir}"
        RESULT_VARIABLE res
        OUTPUT_VARIABLE out
        ERROR_QUIET
        OUTPUT_STRIP_TRAILING_WHITESPACE)
    if(NOT res EQUAL 0)
        set(out "${out}-${res}-NOTFOUND")
    endif()
    set(${_var} "${out}" PARENT_SCOPE)
endfunction()

function(git_head_localmods _var)
    get_git_head_revision(refspec hash)
    if(NOT GIT_FOUND)
        set(${_var} "GIT-NOTFOUND"  PARENT_SCOPE)
        return()
    endif()
    if(NOT hash)
        set(${_var} "HEAD-HASH-NOTFOUND"  PARENT_SCOPE)
        return()
    endif()

    set(src_dir ${PROJECT_SOURCE_DIR})
    ## Determine if there are local modifications to the HEAD
    execute_process(COMMAND ${GIT_EXECUTABLE} diff --shortstat HEAD
        WORKING_DIRECTORY ${src_dir}
        RESULT_VARIABLE res
        OUTPUT_VARIABLE out
        ERROR_VARIABLE error
        OUTPUT_STRIP_TRAILING_WHITESPACE
        )
    if(NOT res EQUAL 0)
        set(out "${out}-${res}-NOTFOUND")
    endif()
    set(${_var} "${out}" PARENT_SCOPE)
endfunction()
