import os
import re
import pdb
import sys
import ctypes
import pprint
import logging
import comtypes
import itertools
import comtypes.client

from . import enums
from . import impl_details

from ... import utils
from ... import declarations
from .. import config as msvc_cfg
from .. import common_utils as msvc_utils

msdia = comtypes.client.GetModule( msvc_cfg.msdia_path )

SymTagEnum = 12
msdia.SymTagEnum = 12

def iif( condition, true_value, false_value ):
    if condition:
        return true_value
    else:
        return false_value

def as_symbol( x ):
    return ctypes.cast( x, ctypes.POINTER( msdia.IDiaSymbol ) )

def as_table( x ):
    return ctypes.cast( x, ctypes.POINTER( msdia.IDiaTable ) )

def as_enum_symbols( x ):
    return ctypes.cast( x, ctypes.POINTER( msdia.IDiaEnumSymbols ) )

def as_enum_variant( x ):
    return ctypes.cast( x, ctypes.POINTER( comtypes.automation.IEnumVARIANT ) )

def print_files( session ):
    files = iter( session.findFile( None, '', 0 ) )
    for f in files:
        f = ctypes.cast( f, ctypes.POINTER(msdia.IDiaSourceFile) )
        print 'File: ', f.fileName

class decl_loader_t(object):
    COMPILER = declarations.compilers.MSVC_PDB_9
    def __init__(self, pdb_file_path ):
        self.logger = utils.loggers.pdb_reader
        self.logger.setLevel(logging.INFO)
        self.logger.debug( 'creating DiaSource object' )
        self.__dia_source = comtypes.client.CreateObject( msdia.DiaSource )
        self.logger.debug( 'loading pdb file: %s' % pdb_file_path )
        self.__dia_source.loadDataFromPdb(pdb_file_path)
        self.logger.debug( 'opening session' )
        self.__dia_session = self.__dia_source.openSession()
        self.logger.debug( 'opening session - done' )
        self.__global_ns = declarations.namespace_t( '::' )
        self.__global_ns.compiler = self.COMPILER
        self.__id2decl = {} #cache symIndexId : decl
        self.__types_cache = {} #smbl id : type

    def __find_table(self, name):
        valid_names = ( 'Symbols', 'SourceFiles', 'Sections'
                        , 'SegmentMap', 'InjectedSource', 'FrameData' )
        tables = self.__dia_session.getEnumTables()
        for table in itertools.imap(as_table, tables):
            if name == table.name:
                return table
        else:
            return None

    @utils.cached
    def symbols_table(self):
        return self.__find_table( "Symbols" )

    @utils.cached
    def symbols(self):
        self.logger.info( 'loading symbols from the file' )
        smbls = {}
        for smbl in itertools.imap( as_symbol, as_enum_variant( self.symbols_table._NewEnum ) ):
            smbl.uname = msvc_utils.undecorate_name( smbl.name, msvc_utils.UNDECORATE_NAME_OPTIONS.UNDNAME_SCOPES_ONLY )
            def smbl_undecorate_name( options = None ):
                return msvc_utils.undecorate_name( smbl.name, options )
            smbl.undecorate_name = smbl_undecorate_name
            smbls[ smbl.symIndexId ] = smbl
        self.logger.info( 'loading symbols(%d) from the file - done', len( smbls ) )
        return smbls

    def __load_nss(self):
        def ns_filter( smbl ):
            self.logger.debug( '__load_ns.ns_filter, %s', smbl.uname )
            tags = ( msdia.SymTagFunction
                     , msdia.SymTagBlock
                     , msdia.SymTagData
                     #~ , msdia.SymTagAnnotation
                     #~ , msdia.SymTagPublicSymbol
                     , msdia.SymTagUDT
                     , msdia.SymTagEnum
                     #~ , msdia.SymTagFunctionType
                     #~ , msdia.SymTagPointerType
                     , msdia.SymTagArrayType
                     , msdia.SymTagBaseType
                     , msdia.SymTagTypedef
                     , msdia.SymTagBaseClass
                     , msdia.SymTagFriend
                     #~ , msdia.SymTagFunctionArgType
                     #~ , msdia.SymTagUsingNamespace
                    )
            if smbl.symTag not in tags:
                self.logger.debug( 'smbl.symTag not in tags, %s', smbl.uname )
                return False
            elif smbl.symTag == msdia.SymTagData and not self.__is_my_var( smbl ):
                return False
            elif not smbl.name:
                self.logger.debug( 'not smbl.name, %s', smbl.uname )
                return False
            #~ elif '-' in smbl.name:
                #~ self.logger.debug( '"-" in smbl.name, %s', smbl.uname )
                #~ return False
            elif smbl.classParent:
                parent_smbl = self.symbols[ smbl.classParentId ]
                while parent_smbl:
                    if parent_smbl.symTag == msdia.SymTagUDT:
                        if parent_smbl.uname in smbl.uname:
                            #for some reason std::map is reported as parent of std::_Tree, in source code
                            #std::map derives from std::_Tree. In logical sense parent name is a subset of the child name
                            self.logger.debug( 'parent_smbl.symTag == msdia.SymTagUDT, %s', parent_smbl.uname )
                            return False
                        else:
                            return True
                    else:
                        parent_smbl = self.symbols[ parent_smbl.classParentId ]
            else:
                return True

        self.logger.debug( 'scanning symbols table' )

        self.logger.debug( 'looking for scopes' )
        names = set()
        for index, smbl in enumerate( itertools.ifilter( ns_filter, self.symbols.itervalues() ) ):
            if index and ( index % 10000 == 0 ):
                self.logger.debug( '%d symbols scanned', index )
            name_splitter = impl_details.get_name_splitter( smbl.uname )
            for sn in name_splitter.scope_names:
                if '<' in sn:
                    break
                else:
                    names.add( sn )
        names = list( names )
        names.sort()
        self.logger.debug( 'looking for scopes - done' )

        nss = {'': self.__global_ns}

        self.logger.debug( 'building namespace objects' )
        for ns_name in itertools.ifilterfalse( self.__find_udt, names ):
            self.logger.debug( 'inserting ns "%s" into declarations tree', ns_name )
            name_splitter = impl_details.get_name_splitter( ns_name )
            if not name_splitter.scope_names:
                parent_ns = self.global_ns
            else:
                parent_ns = nss.get( name_splitter.scope_names[-1], None )
                if not parent_ns:
                    continue #in this case the parent scope is UDT
            ns_decl = declarations.namespace_t( name_splitter.name )
            ns_decl.compiler = self.COMPILER
            ns_decl.mangled = ns_decl.name
            ns_decl.demangled = ns_decl.name
            parent_ns.adopt_declaration( ns_decl )
            nss[ ns_name ] = ns_decl
            self.logger.debug( 'inserting ns "%s" into declarations tree - done', ns_name )
        self.logger.debug( 'building namespace objects - done' )

        self.logger.debug( 'scanning symbols table - done' )

    def __update_decls_tree( self, decl ):
        #~ if decl.name == 'money_base' and isinstance( decl, declarations.class_t ):
            #~ pdb.set_trace()
        smbl = decl.dia_symbols[0]
        if smbl.classParentId in self.__id2decl:
            self.__adopt_declaration( self.__id2decl[smbl.classParentId], decl )
        else:
            name_splitter = impl_details.get_name_splitter( smbl.uname )
            if not name_splitter.scope_names:
                self.__adopt_declaration( self.global_ns, decl )
            else:
                parent_name = '::' + name_splitter.scope_names[-1]
                try:
                    parent = self.global_ns.decl( parent_name )
                except:
                    declarations.print_declarations( self.global_ns )
                    print 'identifiers:'
                    for index, identifier in enumerate(name_splitter.identifiers):
                        print index, ':', identifier
                    raise
                self.__adopt_declaration( parent, decl )

    def __adopt_declaration( self, parent, decl ):
        smbl = decl.dia_symbols[0]
        already_added = parent.decls( decl.name, decl_type=decl.__class__, recursive=False, allow_empty=True )
        if not already_added:
            if isinstance( parent, declarations.namespace_t ):
                parent.adopt_declaration( decl )
            else:
                parent.adopt_declaration( decl, self.__guess_access_type( smbl ) )
            self.__id2decl[ smbl.symIndexId ] = decl
        else:
            for other_decl in already_added:
                if self.__is_same_smbls( other_decl, decl ):
                    other_decl.dia_symbols.append( smbl )
                    self.__id2decl[ smbl.symIndexId ] = other_decl
                    return
            else:
                if isinstance( parent, declarations.namespace_t ):
                    parent.adopt_declaration( decl )
                else:
                    parent.adopt_declaration( decl, self.__guess_access_type( smbl )  )
                self.__id2decl[ smbl.symIndexId ] = decl

    def __guess_access_type( self, smbl ):
        if enums.CV_access_e.CV_private == smbl.access:
            return declarations.ACCESS_TYPES.PRIVATE
        elif enums.CV_access_e.CV_protected == smbl.access:
            return declarations.ACCESS_TYPES.PROTECTED
        else:
            fully_undecorated_name = smbl.undecorate_name()
            if fully_undecorated_name.startswith( 'private:' ):
                declarations.ACCESS_TYPES.PRIVATE
            elif fully_undecorated_name.startswith( 'protected:' ):
                declarations.ACCESS_TYPES.PROTECTED
            else:
                return declarations.ACCESS_TYPES.PUBLIC

    class parent_exists_t:
        def __init__( self, global_ns, classes, id2decl ):
            self.global_ns = global_ns
            self.classes = classes
            self.id2decl = id2decl
            self.__parent_exist = set()

        def __call__( self, decl ):
            smbl = decl.dia_symbols[0]
            if smbl.classParent:
                if smbl.classParentId in self.id2decl:
                    return True
                else:
                    return False
            if self.classes.has_key( smbl.classParentId ):
                return False
            name_splitter = impl_details.get_name_splitter( smbl.uname )
            if not name_splitter.scope_names:
                return True #global namespace
            else:
                #print "I am here " + '::' + name_splitter.scope_names[-1]
                parent_name = '::' + name_splitter.scope_names[-1]
                if parent_name in self.__parent_exist:
                    return True
                found = self.global_ns.decls( parent_name
                                              , decl_type=declarations.scopedef_t
                                              , allow_empty=True
                                              , recursive=True )
                if found:
                    self.__parent_exist.add( parent_name )
                return bool( found )

    def __clear_symbols(self):
        self.logger.info( 'clearing symbols' )
        to_be_deleted = []
        useless_tags = (
            msdia.SymTagAnnotation
            , msdia.SymTagPublicSymbol
            , msdia.SymTagBlock
            , msdia.SymTagFuncDebugStart
            , msdia.SymTagFuncDebugEnd
        )
        for smbl_id, smbl in self.symbols.iteritems():
            if smbl.symTag in useless_tags \
               or ( smbl.symTag == msdia.SymTagData and not self.__is_my_var( smbl ) ):
                to_be_deleted.append( smbl_id )

        map( lambda smbl_id: self.symbols.pop( smbl_id ), to_be_deleted )
        self.logger.info( 'clearing symbols(%d) - done', len( to_be_deleted ) )


    def __normalize_name( self, decl ):
        if decl.name == '<unnamed-tag>':
            decl.name = ''
        elif decl.name.startswith( '?' ):
            decl.name = ''
        elif isinstance( decl, declarations.namespace_t ) and 'anonymous-namespace' in decl.name:
            decl.name = ''
        else:
            pass

    def __join_unnamed_nss( self, ns_parent ):
        child_nss = ns_parent.namespaces( name='', recursive=False, allow_empty=True )
        if len(child_nss) > 1:
            alive_ns = child_nss[0]
            dead_nss = child_nss[1:]
            for dead_ns in dead_nss:
                decls = dead_ns.decls( recursive=False, allow_empty=True )
                map( dead_ns.remove_declaration, decls )
                map( alive_ns.adopt_declaration, decls )
            map( ns_parent.remove_declaration, dead_nss )
        map( self.__join_unnamed_nss
             , ns_parent.namespaces( recursive=False, allow_empty=True ) )


    def read(self):
        self.__clear_symbols()
        self.__load_nss()
        self.__load_classes()
        self.__load_base_classes()
        self.__load_enums()
        self.__load_vars()
        self.__load_typedefs()
        self.__load_calldefs()
        map( self.__normalize_name, self.global_ns.decls(recursive=True) )
        self.__join_unnamed_nss( self.global_ns )
        #join unnamed namespaces

    @property
    def dia_global_scope(self):
        return self.__dia_session.globalScope

    @property
    def global_ns(self):
        return self.__global_ns

    def __is_same_smbls( self, decl1, decl2 ):
        if not( decl1.__class__ is decl2.__class__ ):
            return False
        if decl1.name == decl2.name:
            if isinstance( decl1, declarations.calldef_t ):
                #TODO: well, I will have to fix this someday
                return False
            else:
                return True
        else:
            return False
            #~ return self.__dia_session.symsAreEquiv( decl1.dia_symbols[0], decl2.dia_symbols[0] )

    def __find_udt( self, name ):
        self.logger.debug( 'testing whether name( "%s" ) is UDT symbol' % name )
        flags = enums.NameSearchOptions.nsfCaseSensitive
        found = self.dia_global_scope.findChildren( msdia.SymTagUDT, name, flags )
        if found.Count == 1:
            self.logger.debug( 'name( "%s" ) is UDT symbol' % name )
            return as_symbol( found.Item(0) )
        elif 1 < found.Count:
            raise RuntimeError( "duplicated UDTs with name '%s', were found" % name )
            #~ self.logger.debug( 'name( "%s" ) is UDT symbol' % name )
            #~ return [as_symbol( s ) for s in  iter(found)]
            #~ for s in iter(found):
                #~ s =
                #~ print s.name
                #~ print impl_details.guess_class_type(s.udtKind)
        else:
            self.logger.debug( 'name( "%s" ) is **NOT** UDT symbol' % name )
            return None

    def __update_decl( self, decl, smbl ):
        decl.dia_symbols = [smbl]
        decl.compiler = self.COMPILER
        if not isinstance( decl, declarations.typedef_t ):
            decl.byte_size = smbl.length
            decl.byte_offset = smbl.offset
        decl.mangled = iif( smbl.get_undecoratedNameEx(0), smbl.name, '' )
        decl.demangled = iif( smbl.uname, smbl.uname, '' )
        decl.is_artificial = bool( smbl.compilerGenerated )
        decl.location = declarations.location_t( smbl.lexicalParent.sourceFileName )


    def __load_classes( self ):
        classes = {}#unique symbol id : class decl
        is_udt = lambda smbl: smbl.symTag == msdia.SymTagUDT
        self.logger.info( 'building udt objects' )
        for udt_smbl in itertools.ifilter( is_udt, self.symbols.itervalues() ):
            classes[udt_smbl.symIndexId] = self.__create_class(udt_smbl)
        self.logger.info( 'building udt objects(%d) - done', len(classes) )

        self.logger.info( 'integrating udt objects with namespaces' )
        does_parent_exists = self.parent_exists_t( self.global_ns, classes, self.__id2decl )
        while classes:
            to_be_integrated = len( classes )
            self.logger.info( 'there are %d classes to go', len( classes ) )
            to_be_deleted = filter( does_parent_exists, classes.itervalues() )
            map( self.__update_decls_tree, to_be_deleted )
            map( lambda decl: classes.pop( decl.dia_symbols[0].symIndexId )
                 , to_be_deleted )
            if not ( to_be_integrated - len( classes ) ):
                for cls in classes.itervalues():
                    self.logger.warning( 'unable to integrate class "%s" into declarations tree', cls.dia_symbols[0].uname )
                break
        self.logger.info( 'integrating udt objects with namespaces - done' )

    def __load_base_classes( self ):
        make_hi = declarations.hierarchy_info_t
        is_base_class = lambda smbl: smbl.symTag == msdia.SymTagBaseClass \
                                     and False == smbl.indirectVirtualBaseClass
        self.logger.info( 'building class hierarchies' )
        for count, smbl in enumerate( itertools.ifilter( is_base_class, self.symbols.itervalues() ) ):
            base_id = smbl.type.symIndexId
            derived_id = smbl.classParentId

            hi_base = make_hi( self.__id2decl[base_id]
                               , self.__guess_access_type( smbl )
                               , bool( smbl.virtualBaseClass ) )
            self.__id2decl[ derived_id ].bases.append( hi_base )

            hi_derived = make_hi( self.__id2decl[derived_id]
                                  , self.__guess_access_type( smbl )
                                  , bool( smbl.virtualBaseClass ) )
            self.__id2decl[ base_id ].derived.append( hi_derived )

        self.logger.info( 'building class hierarchies(%d) - done', count )

    def __load_enums( self ):
        is_enum = lambda smbl: smbl.symTag == msdia.SymTagEnum
        self.logger.info( 'building enum objects' )
        for enums_count, enum_smbl in enumerate( itertools.ifilter( is_enum, self.symbols.itervalues() ) ):
            enum_decl = self.__create_enum(enum_smbl)
            if not enum_decl:
                continue
            self.__update_decls_tree( enum_decl )
        self.logger.info( 'building enum objects(%d) - done', enums_count )

    def __create_enum( self, enum_smbl ):
        name_splitter = impl_details.get_name_splitter( enum_smbl.uname )
        self.logger.debug( 'working on enum %s', enum_smbl.uname )
        enum_decl = declarations.enumeration_t( name_splitter.name )
        self.__update_decl( enum_decl, enum_smbl )

        values = enum_smbl.findChildren( msdia.SymTagData, None, 0 )
        for v in itertools.imap(as_symbol, values):
            if v.classParent.symIndexId != enum_smbl.symIndexId:
                continue
            enum_decl.append_value( v.name, v.value )
        if enum_decl.values:
            return enum_decl
        else:
            #for some reason same enum could appear under global namespace and
            #under the class, it was defined in. This is a criteria I use to distinguish
            #between those cases
            return None

    def __create_class( self, class_smbl ):
        name_splitter = impl_details.get_name_splitter( class_smbl.uname )
        class_decl = declarations.class_t( name_splitter.name )
        self.__update_decl( class_decl, class_smbl )
        class_decl.class_type = impl_details.guess_class_type(class_smbl.udtKind)
        return class_decl

    def __is_my_var( self, smbl ):
        #I am only interested in global and class variables
        if smbl.symTag != msdia.SymTagData:
            return False
        if not smbl.uname:
            return False
        if smbl.classParentId not in self.symbols:
            return True #global scope
        parent_smbl = self.symbols[ smbl.classParentId ]
        return bool( parent_smbl.symTag == msdia.SymTagUDT )

    def __load_vars( self ):
        self.logger.info( 'building variable objects' )

        for vars_count, var_smbl in enumerate( itertools.ifilter( self.__is_my_var, self.symbols.itervalues() ) ):
            var_decl = self.__create_var(var_smbl)
            if var_decl:
                self.__update_decls_tree( var_decl )
        self.logger.info( 'building variable objects(%d) - done', vars_count )

    def __create_var( self, smbl ):
        self.logger.debug( 'creating variable "%s"', smbl.uname )
        name_splitter = impl_details.get_name_splitter( smbl.uname )
        decl = declarations.variable_t( name_splitter.name )
        self.__update_decl( decl, smbl )
        decl.type = self.create_type( smbl.type )
        decl.value = str(smbl.value)
        self.logger.debug( 'creating variable "%s" - done', smbl.uname )
        return decl

    def __load_typedefs( self ):
        self.logger.info( 'building typedef objects' )
        is_typedef = lambda smbl: smbl.symTag == msdia.SymTagTypedef
        for typedefs_count, typedef_smbl in enumerate( itertools.ifilter( is_typedef, self.symbols.itervalues() ) ):
            typedef_decl = self.__create_typedef(typedef_smbl)
            self.__update_decls_tree( typedef_decl )
        self.logger.info( 'building typedef objects(%d) - done', typedefs_count )

    def __create_typedef( self, smbl ):
        self.logger.debug( 'creating typedef "%s"', smbl.uname )
        name_splitter = impl_details.get_name_splitter( smbl.uname )
        decl = declarations.typedef_t( name_splitter.name
                                       , self.create_type( smbl.type ) )
        self.__update_decl( decl, smbl )
        self.logger.debug( 'creating typedef "%s" - done', smbl.uname )
        return decl

    def __load_calldefs( self ):
        self.logger.info( 'building function objects' )
        is_function = lambda smbl: smbl.symTag == msdia.SymTagFunction
        for functions_count, function_smbl in enumerate( itertools.ifilter( is_function, self.symbols.itervalues() ) ):
            function_decl = self.__create_calldef(function_smbl)
            if function_decl:
                self.__update_decls_tree( function_decl )
        self.logger.info( 'building function objects(%d) - done', functions_count )

    def __guess_operator_type( self, smbl, operator_type ):
        #assumption: the code deals with correct code
        if not smbl.uname.startswith( 'operator' ) or smbl.uname == 'operator':
            return None
        oper_smbls = ('!', ' ', '*', '%', '&', '(', '+', '-', ',', '/', '|', '~', '[', '^', '=', '<')
        if smbl.uname[ len( 'operator' ) ] not in oper_smbls:
            return None
        if smbl.uname[ len( 'operator' ) ] == ' ' \
           and smbl.uname not in ['operator new', 'operator delete']:
            #we have casting operator
            return declarations.casting_operator_t()
        if isinstance( operator_type, declarations.member_function_type_t ):
            return declarations.member_operator_t()
        else:
            return declarations.free_operator_t()

    def __guess_constructor( self, smbl, calldef_type ):
        tmpls = declarations.templates
        class_ = declarations.remove_declarated( calldef_type.class_inst )
        if class_.name == smbl.uname \
           or ( tmpls.is_instantiation( class_.name )
                and tmpls.name( class_.name ) == smbl.uname ):
            calldef_type.return_type = None
            return declarations.constructor_t()

    def __create_calldef( self, smbl ):
        self.logger.debug( 'creating calldef "%s"', smbl.uname )
        #~ if smbl.uname == 'some_function':
            #~ pdb.set_trace()
        name_splitter = impl_details.get_name_splitter( smbl.uname )
        calldef_type = self.create_type( smbl.type ) #what does happen with constructor?
        decl = None
        if isinstance( calldef_type, declarations.member_function_type_t ):
            could_be_static = False
            could_be_const = False
            if smbl.uname.startswith( '~' ):
                decl = declarations.destructor_t()
            if not decl: #may be operator
                decl = self.__guess_operator_type(smbl, calldef_type)
                could_be_static = True
                could_be_const = True
            if not decl: #may be constructor
                decl = self.__guess_constructor( smbl, calldef_type )
            if not decl:
                decl = declarations.member_function_t()
                could_be_static = True
                could_be_const = True
            if smbl.virtual:
                decl.virtuality = iif( smbl.pure
                                       , declarations.VIRTUALITY_TYPES.PURE_VIRTUAL
                                       , declarations.VIRTUALITY_TYPES.VIRTUAL )
            decl.has_const = bool( could_be_const and smbl.constType )
            decl.has_static = bool( could_be_static and smbl.isStatic )
        else:
            decl = self.__guess_operator_type(smbl, calldef_type)
            if not decl:
                if 'instantiate::`dynamic initializer for' in smbl.uname:
                    return #in this case we deal with initializer of some global variable
                decl = declarations.free_function_t()
        decl.name = smbl.uname
        decl.arguments = map( lambda t: declarations.argument_t( type=t )
                              , calldef_type.arguments_types )

        args_smbls = map( as_symbol, smbl.findChildren( msdia.SymTagData, None, 0 ) )
        args_smbls = filter( lambda smbl: smbl.dataKind == enums.DataKind.DataIsParam, args_smbls )

        for index, arg_smbl in enumerate( args_smbls ):
            arg_decl = decl.arguments[index]
            arg_decl.name = arg_smbl.name
            arg_decl.default_value = arg_smbl.value
        decl.return_type = calldef_type.return_type

        self.__update_decl( decl, smbl )
        self.logger.debug( 'creating calldef "%s" - done', smbl.uname )
        return decl

    def __create_function_type( self, smbl ):
        return_type = self.create_type( smbl.type )
        args_smbls = smbl.findChildren( msdia.SymTagFunctionArgType, None, 0 )
        args_types = map( self.create_type, itertools.imap(as_symbol, args_smbls) )
        if smbl.objectPointerType:
            try:
                class_ = self.create_type( smbl.objectPointerType )
                class_ = declarations.base_type( class_ )
                #~ pdb.set_trace()
                return declarations.member_function_type_t( class_, return_type, args_types )
            except:
                self.logger.warning( 'unable to find out the type of the object pointer for a class method.' )
                return declarations.unknown_t()
        else:
            return declarations.free_function_type_t( return_type, args_types )

    def create_type( self, smbl ):
        #http://msdn2.microsoft.com/en-us/library/s3f49ktz(VS.80).aspx
        if smbl.symIndexId in self.__types_cache:
            return self.__types_cache[smbl.symIndexId]
        my_type = None
        if msdia.SymTagBaseType == smbl.symTag:
            if enums.BasicType.btNoType == smbl.baseType:
                my_type = declarations.unknown_t()
            elif enums.BasicType.btVoid == smbl.baseType:
                my_type = declarations.void_t()
            elif enums.BasicType.btChar == smbl.baseType:
                my_type = declarations.char_t()
            elif enums.BasicType.btWChar == smbl.baseType:
                my_type = declarations.wchar_t()
            elif smbl.baseType in ( enums.BasicType.btInt, enums.BasicType.btLong ):
                if 8 == smbl.length:
                    my_type = declarations.long_long_int_t()
                elif 4 == smbl.length:
                    my_type = declarations.int_t() #long_int_t
                elif 2 == smbl.length:
                    my_type = declarations.short_int_t()
                else:
                    my_type = declarations.int_t()
            elif enums.BasicType.btUInt == smbl.baseType:
                if 2 == smbl.length:
                    my_type = declarations.short_unsigned_int_t()
                else:
                    my_type = declarations.unsigned_int_t()
            elif enums.BasicType.btFloat == smbl.baseType:
                if 8 == smbl.length:
                    my_type = declarations.double_t()
                else:
                    my_type = declarations.float_t()
            elif enums.BasicType.btBCD == smbl.baseType:
                my_type = declarations.unknown_t()
            elif enums.BasicType.btBool == smbl.baseType:
                my_type = declarations.bool_t()
            elif enums.BasicType.btULong == smbl.baseType:
                if 8 == smbl.length:
                    my_type = declarations.long_long_unsigned_int_t()
                else:
                    my_type = declarations.long_unsigned_int_t()
            elif enums.BasicType.btCurrency == smbl.baseType:
                my_type = declarations.unknown_t()
            elif enums.BasicType.btDate == smbl.baseType:
                my_type = declarations.double_t()
            elif enums.BasicType.btVariant == smbl.baseType:
                my_type = declarations.unknown_t()
            elif enums.BasicType.btComplex == smbl.baseType:
                my_type = declarations.complex_double_t()
            elif enums.BasicType.btBit == smbl.baseType:
                my_type = declarations.unknown_t()
            elif enums.BasicType.btBSTR == smbl.baseType:
                my_type = declarations.unknown_t()
            elif enums.BasicType.btHresult == smbl.baseType:
                my_type = declarations.long_int_t()
            else:
                my_type = declarations.unknown_t()
        elif msdia.SymTagPointerType == smbl.symTag:
            base_type = self.create_type( smbl.type )
            if smbl.reference:
                my_type = declarations.reference_t( base_type )
            else:
                my_type = declarations.pointer_t( base_type )
        elif msdia.SymTagArrayType == smbl.symTag:
            element_type = self.create_type( smbl.type )
            size = declarations.array_t.SIZE_UNKNOWN
            if smbl.count:
                size = smbl.count
            #~ if smbl.length and element_type.byte_size:
                #~ size = smbl.length / element_type.byte_size
            my_type = declarations.array_t( element_type, size )
        elif smbl.symTag in ( msdia.SymTagUDT, msdia.SymTagTypedef, msdia.SymTagEnum ):
            if smbl.symIndexId in self.__id2decl:
                decl = self.__id2decl[ smbl.symIndexId ]
                my_type = declarations.declarated_t( decl )
            else:
                my_type = declarations.unknown_t()
        elif msdia.SymTagFunctionArgType == smbl.symTag:
            my_type = self.create_type( smbl.type )
        elif msdia.SymTagFunctionType == smbl.symTag:
            my_type = self.__create_function_type( smbl )
        else:
            my_type = declarations.unknown_t()
        try:
            my_type.byte_size = smbl.length
        except AttributeError:
            pass
        if smbl.constType:
            if isinstance( my_type, declarations.member_function_type_t ):
                my_type.has_const = True
            else:
                if not isinstance( my_type, declarations.const_t ):
                    my_type = declarations.const_t( my_type )
        if smbl.volatileType:
            if not isinstance( my_type, declarations.volatile_t ):
                my_type = declarations.volatile_t( my_type )
        return my_type
