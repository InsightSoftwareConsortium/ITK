# Copyright 2014-2017 Insight Software Consortium.
# Copyright 2004-2009 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

"""
Define a few algorithms that deal with different properties of std containers.

"""

import string
from . import cpptypes
from . import templates
from . import type_traits
from . import namespace
from . import class_declaration
from . import traits_impl_details
from .. import utils

std_namespaces = ('std', 'stdext', '__gnu_cxx')


class defaults_eraser(object):

    def __init__(self, unordered_maps_and_sets):
        self.unordered_maps_and_sets = unordered_maps_and_sets

    def normalize(self, type_str):
        return type_str.replace(' ', '')

    def replace_basic_string(self, cls_name):

        # Take the lists of all possible string variations
        # and clean them up by replacing ::std by std.
        str_eq = [
            v.replace("::std", "std") for v in
            type_traits.string_equivalences]
        wstr_eq = [
            v.replace("::std", "std") for v in
            type_traits.wstring_equivalences]

        # Replace all the variations of strings by the smallest one.
        strings = {
            "std::string": [v for v in str_eq if not v == "std::string"],
            "std::wstring": [v for v in wstr_eq if not v == "std::wstring"]}

        new_name = cls_name
        for short_name, long_names in strings.items():
            for lname in long_names:
                new_name = new_name.replace(lname, short_name)

        return new_name

    def decorated_call_prefix(self, cls_name, text, doit):
        has_text = cls_name.startswith(text)
        if has_text:
            cls_name = cls_name[len(text):]
        answer = doit(cls_name)
        if has_text:
            answer = text + answer
        return answer

    def decorated_call_suffix(self, cls_name, text, doit):
        has_text = cls_name.endswith(text)
        if has_text:
            cls_name = cls_name[: len(text)]
        answer = doit(cls_name)
        if has_text:
            answer = answer + text
        return answer

    def erase_call(self, cls_name):
        c_traits = find_container_traits(cls_name)
        if not c_traits:
            return cls_name
        return c_traits.remove_defaults(cls_name)

    def no_std(self, cls_name):
        return self.decorated_call_prefix(
            cls_name, 'std::' + utils.get_tr1(cls_name), self.erase_call)

    def no_stdext(self, cls_name):
        return self.decorated_call_prefix(
            cls_name, 'stdext::', self.no_std)

    def no_gnustd(self, cls_name):
        return self.decorated_call_prefix(
            cls_name, '__gnu_cxx::', self.no_stdext)

    def no_const(self, cls_name):
        return self.decorated_call_prefix(
            cls_name, 'const ', self.no_gnustd)

    def no_end_const(self, cls_name):
        return self.decorated_call_suffix(
            cls_name, ' const', self.no_const)

    def erase_recursive(self, cls_name):
        return self.no_end_const(cls_name)

    def erase_allocator(self, cls_name, default_allocator='std::allocator'):
        cls_name = self.replace_basic_string(cls_name)
        c_name, c_args = templates.split(cls_name)
        if len(c_args) != 2:
            return
        value_type = c_args[0]
        tmpl = string.Template(
            "$container< $value_type, $allocator<$value_type> >")
        tmpl = tmpl.substitute(
            container=c_name,
            value_type=value_type,
            allocator=default_allocator)
        if self.normalize(cls_name) == \
                self.normalize(tmpl):
            return templates.join(
                c_name, [self.erase_recursive(value_type)])

    def erase_container(self, cls_name, default_container_name='std::deque'):
        cls_name = self.replace_basic_string(cls_name)
        c_name, c_args = templates.split(cls_name)
        if len(c_args) != 2:
            return
        value_type = c_args[0]
        dc_no_defaults = self.erase_recursive(c_args[1])
        if self.normalize(dc_no_defaults) != self.normalize(
                templates.join(default_container_name, [value_type])):
            return
        return templates.join(
            c_name, [self.erase_recursive(value_type)])

    def erase_container_compare(
            self,
            cls_name,
            default_container_name='std::vector',
            default_compare='std::less'):
        cls_name = self.replace_basic_string(cls_name)
        c_name, c_args = templates.split(cls_name)
        if len(c_args) != 3:
            return
        dc_no_defaults = self.erase_recursive(c_args[1])
        if self.normalize(dc_no_defaults) != self.normalize(
                templates.join(default_container_name, [c_args[0]])):
            return
        dcomp_no_defaults = self.erase_recursive(c_args[2])
        if self.normalize(dcomp_no_defaults) != self.normalize(
                templates.join(default_compare, [c_args[0]])):
            return
        value_type = self.erase_recursive(c_args[0])
        return templates.join(c_name, [value_type])

    def erase_compare_allocator(
            self,
            cls_name,
            default_compare='std::less',
            default_allocator='std::allocator'):
        cls_name = self.replace_basic_string(cls_name)
        c_name, c_args = templates.split(cls_name)
        if len(c_args) != 3:
            return
        value_type = c_args[0]
        tmpl = string.Template(
            "$container< $value_type, $compare<$value_type>, " +
            "$allocator<$value_type> >")
        tmpl = tmpl.substitute(
            container=c_name,
            value_type=value_type,
            compare=default_compare,
            allocator=default_allocator)
        if self.normalize(cls_name) == \
                self.normalize(tmpl):
            return templates.join(
                c_name, [self.erase_recursive(value_type)])

    def erase_map_compare_allocator(
            self,
            cls_name,
            default_compare='std::less',
            default_allocator='std::allocator'):
        cls_name = self.replace_basic_string(cls_name)
        c_name, c_args = templates.split(cls_name)
        if len(c_args) != 4:
            return
        key_type = c_args[0]
        mapped_type = c_args[1]
        tmpls = [
            string.Template(
                "$container< $key_type, $mapped_type, $compare<$key_type>, " +
                "$allocator< std::pair< const $key_type, $mapped_type> > >"),
            string.Template(
                "$container< $key_type, $mapped_type, $compare<$key_type>, " +
                "$allocator< std::pair< $key_type const, $mapped_type> > >"),
            string.Template(
                "$container< $key_type, $mapped_type, $compare<$key_type>, " +
                "$allocator< std::pair< $key_type, $mapped_type> > >")]
        for tmpl in tmpls:
            tmpl = tmpl.substitute(
                container=c_name,
                key_type=key_type,
                mapped_type=mapped_type,
                compare=default_compare,
                allocator=default_allocator)
            if self.normalize(cls_name) == self.normalize(tmpl):
                return templates.join(
                    c_name,
                    [self.erase_recursive(key_type),
                     self.erase_recursive(mapped_type)])

    def erase_hash_allocator(self, cls_name):
        cls_name = self.replace_basic_string(cls_name)
        c_name, c_args = templates.split(cls_name)
        if len(c_args) < 3:
            return

        default_less = 'std::less'
        default_equal_to = 'std::equal_to'
        default_allocator = 'std::allocator'

        if len(c_args) == 3:
            default_hash = 'hash_compare'
            tmpl = (
                "$container< $value_type, $hash<$value_type, " +
                "$less<$value_type> >, $allocator<$value_type> >")
        elif len(c_args) == 4:
            default_hash = 'hash'
            tmpl = (
                "$container< $value_type, $hash<$value_type >, " +
                "$equal_to<$value_type >, $allocator<$value_type> >")
        else:
            return

        value_type = c_args[0]
        template = string.Template(tmpl)
        for ns in std_namespaces:
            inst = template.substitute(
                container=c_name,
                value_type=value_type,
                hash=ns + '::' + utils.get_tr1(cls_name) + default_hash,
                less=default_less,
                equal_to=default_equal_to,
                allocator=default_allocator)
            if self.normalize(cls_name) == \
                    self.normalize(inst):
                return templates.join(
                    c_name, [self.erase_recursive(value_type)])

    def erase_hashmap_compare_allocator(self, cls_name):
        cls_name = self.replace_basic_string(cls_name)
        c_name, c_args = templates.split(cls_name)

        if self.unordered_maps_and_sets:
            default_less_or_hash = 'std::hash'
        else:
            default_less_or_hash = 'std::less'
        default_allocator = 'std::allocator'
        default_equal_to = 'std::equal_to'

        if len(c_args) > 2:
            key_type = c_args[0]
            mapped_type = c_args[1]
        else:
            return

        if len(c_args) == 4:
            default_hash = 'hash_compare'
            tmpl = string.Template(
                "$container< $key_type, $mapped_type, " +
                "$hash<$key_type, $less<$key_type> >, " +
                "$allocator< std::pair< const $key_type, $mapped_type> > >")
            if key_type.startswith('const ') or key_type.endswith(' const'):
                tmpl = string.Template(
                    "$container< $key_type, $mapped_type, $hash<$key_type, " +
                    "$less<$key_type> >, $allocator< std::pair< $key_type, " +
                    "$mapped_type> > >")
        elif len(c_args) == 5:
            default_hash = 'hash'
            if self.unordered_maps_and_sets:
                tmpl = string.Template(
                    "$container<$key_type, $mapped_type, " +
                    "$hash<$key_type>, " +
                    "$equal_to<$key_type>, " +
                    "$allocator<std::pair<const$key_type, " +
                    "$mapped_type> > >")
                if key_type.startswith('const ') or \
                        key_type.endswith(' const'):
                    tmpl = string.Template(
                        "$container<$key_type, $mapped_type, " +
                        "$hash<$key_type >, " +
                        "$equal_to<$key_type >, " +
                        "$allocator<std::pair<$key_type, " +
                        "$mapped_type> > >")
            else:
                tmpl = string.Template(
                    "$container< $key_type, $mapped_type, "
                    "$hash<$key_type >, " +
                    "$equal_to<$key_type>, "
                    "$allocator< $mapped_type> >")
                if key_type.startswith('const ') or \
                        key_type.endswith(' const'):
                    # TODO: this template is the same than above.
                    # Make sure why this was needed and if this is
                    # tested. There may be a const missing somewhere.
                    tmpl = string.Template(
                        "$container< $key_type, $mapped_type, " +
                        "$hash<$key_type >, " +
                        "$equal_to<$key_type>, " +
                        "$allocator< $mapped_type > >")
        else:
            return

        for ns in std_namespaces:
            inst = tmpl.substitute(
                container=c_name,
                key_type=key_type,
                mapped_type=mapped_type,
                hash=ns + '::' + utils.get_tr1(cls_name) + default_hash,
                less=default_less_or_hash,
                equal_to=default_equal_to,
                allocator=default_allocator)

            if self.normalize(cls_name) == self.normalize(inst):
                return templates.join(
                    c_name,
                    [self.erase_recursive(key_type),
                     self.erase_recursive(mapped_type)])


class container_traits_impl_t(object):

    """
    Implements the functionality needed for convenient work with STD container
    classes

    Implemented functionality:
      * find out whether a declaration is STD container or not
      * find out container value( mapped ) type

    This class tries to be useful as much as possible. For example, for class
    declaration (and not definition) it parses the class name in order to
    extract the information.

    """

    def __init__(
            self,
            container_name,
            element_type_index,
            element_type_typedef,
            eraser,
            key_type_index=None,
            key_type_typedef=None,
            unordered_maps_and_sets=False):
        """
        :param container_name: std container name
        :param element_type_index: position of value\\mapped type within
        template arguments list
        :param element_type_typedef: class typedef to the value\\mapped type
        :param key_type_index: position of key type within template arguments
        list
        :param key_type_typedef: class typedef to the key type
        """
        self._name = container_name
        self.element_type_index = element_type_index
        self.element_type_typedef = element_type_typedef
        self.key_type_index = key_type_index
        self.key_type_typedef = key_type_typedef
        self.unordered_maps_and_sets = unordered_maps_and_sets

        # Get the method from defaults_eraser using it's name
        self.remove_defaults_impl = getattr(
            defaults_eraser(unordered_maps_and_sets), eraser)

    def name(self):
        return self._name

    def get_container_or_none(self, type_):
        """
        Returns reference to the class declaration or None.

        """

        type_ = type_traits.remove_alias(type_)
        type_ = type_traits.remove_cv(type_)

        utils.loggers.queries_engine.debug(
            "Container traits: cleaned up search %s", type_)

        if isinstance(type_, cpptypes.declarated_t):
            cls_declaration = type_traits.remove_alias(type_.declaration)
        elif isinstance(type_, class_declaration.class_t):
            cls_declaration = type_
        elif isinstance(type_, class_declaration.class_declaration_t):
            cls_declaration = type_
        else:
            utils.loggers.queries_engine.debug(
                "Container traits: returning None, type not known\n")
            return

        if not cls_declaration.name.startswith(self.name() + '<'):
            utils.loggers.queries_engine.debug(
                "Container traits: returning None, " +
                "declaration starts with " + self.name() + '<\n')
            return

        # When using libstd++, some container traits are defined in
        # std::tr1::. See remove_template_defaults_tester.py.
        # In this case the is_defined_in_xxx test needs to be done
        # on the parent
        decl = cls_declaration
        if isinstance(type_, class_declaration.class_declaration_t):
            is_ns = isinstance(type_.parent, namespace.namespace_t)
            if is_ns and type_.parent.name == "tr1":
                decl = cls_declaration.parent
        elif isinstance(type_, cpptypes.declarated_t):
            is_ns = isinstance(type_.declaration.parent, namespace.namespace_t)
            if is_ns and type_.declaration.parent.name == "tr1":
                decl = cls_declaration.parent

        for ns in std_namespaces:
            if traits_impl_details.impl_details.is_defined_in_xxx(ns, decl):
                utils.loggers.queries_engine.debug(
                    "Container traits: get_container_or_none() will return " +
                    cls_declaration.name)
                # The is_defined_in_xxx check is done on decl, but we return
                # the original declation so that the rest of the algorithm
                # is able to work with it.
                return cls_declaration

        # This should not happen
        utils.loggers.queries_engine.debug(
            "Container traits: get_container_or_none() will return None\n")

    def is_my_case(self, type_):
        """
        Checks, whether type is STD container or not.

        """

        return bool(self.get_container_or_none(type_))

    def class_declaration(self, type_):
        """
        Returns reference to the class declaration.

        """

        utils.loggers.queries_engine.debug(
            "Container traits: searching class declaration for %s", type_)

        cls_declaration = self.get_container_or_none(type_)
        if not cls_declaration:
            raise TypeError(
                'Type "%s" is not instantiation of std::%s' %
                (type_.decl_string, self.name()))
        return cls_declaration

    def is_sequence(self, type_):
        # raise exception if type is not container
        self.class_declaration(type_)
        return self.key_type_index is None

    def is_mapping(self, type_):
        return not self.is_sequence(type_)

    def __find_xxx_type(
            self,
            type_,
            xxx_index,
            xxx_typedef,
            cache_property_name):
        cls_declaration = self.class_declaration(type_)
        result = getattr(cls_declaration.cache, cache_property_name)
        if not result:
            if isinstance(cls_declaration, class_declaration.class_t):
                xxx_type = cls_declaration.typedef(
                    xxx_typedef, recursive=False).decl_type
                result = type_traits.remove_declarated(xxx_type)
            else:
                xxx_type_str = templates.args(cls_declaration.name)[xxx_index]
                result = traits_impl_details.impl_details.find_value_type(
                    cls_declaration.top_parent, xxx_type_str)
                if None is result:
                    raise RuntimeError(
                        "Unable to find out %s '%s' key\\value type." %
                        (self.name(), cls_declaration.decl_string))
            setattr(cls_declaration.cache, cache_property_name, result)
        return result

    def element_type(self, type_):
        """returns reference to the class value\\mapped type declaration"""
        return self.__find_xxx_type(
            type_,
            self.element_type_index,
            self.element_type_typedef,
            'container_element_type')

    def key_type(self, type_):
        """returns reference to the class key type declaration"""
        if not self.is_mapping(type_):
            raise TypeError(
                'Type "%s" is not "mapping" container' %
                str(type_))
        return self.__find_xxx_type(
            type_,
            self.key_type_index,
            self.key_type_typedef,
            'container_key_type')

    def remove_defaults(self, type_or_string):
        """
        Removes template defaults from a templated class instantiation.

        For example:
            .. code-block:: c++

               std::vector< int, std::allocator< int > >

        will become:
            .. code-block:: c++

               std::vector< int >

        """

        name = type_or_string
        if not isinstance(type_or_string, str):
            name = self.class_declaration(type_or_string).name
        if not self.remove_defaults_impl:
            return name
        no_defaults = self.remove_defaults_impl(name)
        if not no_defaults:
            return name
        return no_defaults


list_traits = container_traits_impl_t(
    'list',
    0,
    'value_type',
    'erase_allocator')

deque_traits = container_traits_impl_t(
    'deque',
    0,
    'value_type',
    'erase_allocator')

queue_traits = container_traits_impl_t(
    'queue',
    0,
    'value_type',
    'erase_container')

priority_queue_traits = container_traits_impl_t(
    'priority_queue',
    0,
    'value_type',
    'erase_container_compare')

vector_traits = container_traits_impl_t(
    'vector',
    0,
    'value_type',
    'erase_allocator')

stack_traits = container_traits_impl_t(
    'stack',
    0,
    'value_type',
    'erase_container')

map_traits = container_traits_impl_t(
    'map',
    1,
    'mapped_type',
    'erase_map_compare_allocator',
    key_type_index=0,
    key_type_typedef='key_type')

multimap_traits = container_traits_impl_t(
    'multimap',
    1,
    'mapped_type',
    'erase_map_compare_allocator',
    key_type_index=0,
    key_type_typedef='key_type')


hash_map_traits = container_traits_impl_t(
    'hash_map',
    1,
    'mapped_type',
    'erase_hashmap_compare_allocator',
    key_type_index=0,
    key_type_typedef='key_type')


hash_multimap_traits = container_traits_impl_t(
    'hash_multimap',
    1,
    'mapped_type',
    'erase_hashmap_compare_allocator',
    key_type_index=0,
    key_type_typedef='key_type')

set_traits = container_traits_impl_t(
    'set',
    0,
    'value_type',
    'erase_compare_allocator')

multiset_traits = container_traits_impl_t(
    'multiset',
    0,
    'value_type',
    'erase_compare_allocator')

hash_set_traits = container_traits_impl_t(
    'hash_set',
    0,
    'value_type',
    'erase_hash_allocator')

hash_multiset_traits = container_traits_impl_t(
    'hash_multiset',
    0,
    'value_type',
    'erase_hash_allocator')

# c++11 equivalents for clang
unordered_map_traits = container_traits_impl_t(
    'unordered_map',
    1,
    'mapped_type',
    'erase_hashmap_compare_allocator',
    key_type_index=0,
    key_type_typedef='key_type',
    unordered_maps_and_sets=True)

unordered_multimap_traits = container_traits_impl_t(
    'unordered_multimap',
    1,
    'mapped_type',
    'erase_hashmap_compare_allocator',
    key_type_index=0,
    key_type_typedef='key_type',
    unordered_maps_and_sets=True)

unordered_set_traits = container_traits_impl_t(
    'unordered_set',
    1,
    'value_type',
    'erase_hash_allocator',
    unordered_maps_and_sets=True)

unordered_multiset_traits = container_traits_impl_t(
    'unordered_multiset',
    1,
    'value_type',
    'erase_hash_allocator',
    unordered_maps_and_sets=True)

all_container_traits = (
    list_traits,
    deque_traits,
    queue_traits,
    priority_queue_traits,
    vector_traits,
    stack_traits,
    map_traits,
    multimap_traits,
    hash_map_traits,
    hash_multimap_traits,
    set_traits,
    hash_set_traits,
    multiset_traits,
    hash_multiset_traits,
    unordered_map_traits,
    unordered_multimap_traits,
    unordered_set_traits,
    unordered_multiset_traits)
"""tuple of all STD container traits classes"""

sequential_container_traits = [
    list_traits,
    deque_traits,
    queue_traits,
    priority_queue_traits,
    vector_traits,
    stack_traits,
    set_traits,
    hash_set_traits,
    multiset_traits,
    hash_multiset_traits]
"""list, that contains all STD container traits classes"""


def find_container_traits(cls_or_string):
    """
    Find the container traits type of a declaration.

    Args:
        cls_or_string (str | declarations.declaration_t): a string

    Returns:
        declarations.container_traits: a container traits
    """

    if isinstance(cls_or_string, str):
        if not templates.is_instantiation(cls_or_string):
            return None
        name = templates.name(cls_or_string)
        if name.startswith('std::'):
            name = name[len('std::'):]
        if name.startswith('std::tr1::'):
            name = name[len('std::tr1::'):]
        for cls_traits in all_container_traits:
            if cls_traits.name() == name:
                return cls_traits
    else:

        if isinstance(cls_or_string, class_declaration.class_types):
            # Look in the cache.
            if cls_or_string.cache.container_traits is not None:
                return cls_or_string.cache.container_traits

        # Look for a container traits
        for cls_traits in all_container_traits:
            if cls_traits.is_my_case(cls_or_string):
                # Store in the cache
                if isinstance(cls_or_string, class_declaration.class_types):
                    cls_or_string.cache.container_traits = cls_traits
                return cls_traits
