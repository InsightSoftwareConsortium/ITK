# Copyright 2014-2017 Insight Software Consortium.
# Copyright 2004-2009 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

from .. import declarations


def bind_aliases(decls):
    """
    This function binds between class and it's typedefs.

    :param decls: list of all declarations

    :rtype: None

    """

    visited = set()
    typedefs = [
        decl for decl in decls if isinstance(decl, declarations.typedef_t)]
    for decl in typedefs:
        type_ = declarations.remove_alias(decl.decl_type)
        if not isinstance(type_, declarations.declarated_t):
            continue
        cls_inst = type_.declaration
        if not isinstance(cls_inst, declarations.class_types):
            continue
        if id(cls_inst) not in visited:
            visited.add(id(cls_inst))
            del cls_inst.aliases[:]
        cls_inst.aliases.append(decl)


def join_declarations(namespace):
    _join_namespaces(namespace)
    for ns in namespace.declarations:
        if isinstance(ns, declarations.namespace_t):
            join_declarations(ns)


def _join_namespaces(namespace):
    ddhash = {}
    decls = []

    for decl in namespace.declarations:
        _fill_declarations(ddhash, decls, decl)

    class_t = declarations.class_t
    class_declaration_t = declarations.class_declaration_t
    if class_t in ddhash and class_declaration_t in ddhash:
        # If there is a class and its forward declaration in the namespace,
        # Remove the second one from the declaration tree
        _remove_second_class(ddhash, decls, class_t, class_declaration_t)

    namespace.declarations = decls


def _fill_declarations(ddhash, decls, decl):
    if decl.__class__ not in ddhash:
        ddhash[decl.__class__] = {decl.name: [decl]}
        decls.append(decl)
    else:
        joined_decls = ddhash[decl.__class__]
        if decl.name not in joined_decls:
            decls.append(decl)
            joined_decls[decl.name] = [decl]
        else:
            if isinstance(decl, declarations.calldef_t):
                if decl not in joined_decls[decl.name]:
                    # functions has overloading
                    decls.append(decl)
                    joined_decls[decl.name].append(decl)
            elif isinstance(decl, declarations.enumeration_t):
                # unnamed enums
                if not decl.name and decl not in \
                        joined_decls[decl.name]:
                    decls.append(decl)
                    joined_decls[decl.name].append(decl)
            elif isinstance(decl, declarations.class_t):
                # unnamed classes
                if not decl.name and decl not in \
                        joined_decls[decl.name]:
                    decls.append(decl)
                    joined_decls[decl.name].append(decl)
            elif isinstance(decl, declarations.namespace_t):
                joined_decls[decl.name][0].take_parenting(decl)


def _remove_second_class(ddhash, decls, class_t, class_declaration_t):
    class_names = set()
    for name, same_name_classes in ddhash[class_t].items():
        if not name:
            continue
        if same_name_classes[0].mangled:
            class_names.add(same_name_classes[0].mangled)
        else:
            class_names.add(same_name_classes[0].name)

    class_declarations = ddhash[class_declaration_t]
    for name, same_name_class_declarations in \
            class_declarations.items():
        if not name:
            continue
        for class_declaration in same_name_class_declarations:
            if class_declaration.mangled is not None:
                # gccxml
                if class_declaration.mangled in class_names:
                    decls.remove(class_declaration)
            elif class_declaration.name in class_names:
                # castxml
                decls.remove(class_declaration)
