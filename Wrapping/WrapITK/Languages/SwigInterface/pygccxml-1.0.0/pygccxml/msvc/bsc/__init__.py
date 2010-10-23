import os
import sys
import logging
from c_wrapper import *
from .. import common_utils as msvc_utils

class definition_t(object):
    #represents some other symbol
    def __init__( self, def_id, bsc ):
        self.__bsc = bsc
        self.__def_id = def_id

    @property
    def def_id(self):
        return self.__def_id

    @utils.cached
    def location( self ):
        module = STRING()
        line = LINE()
        if not BSCIdefInfo( self.__bsc, self.def_id, byref( module ), byref( line ) ):
            raise RuntimeError( "Unable to load information about instance(%s)" % str( self.__def_id ) )
        return (module, line)

    @utils.cached
    def file_name(self):
        return self.location[0].value

    @utils.cached
    def line(self):
        return self.location[1].value

    def __str__( self ):
        return self.file_name + ': %d' % self.line + ' name: %s' % self.as_instance.name

    @utils.cached
    def as_instance(self):
        return self.__bsc.create_instance( BSCIinstFrIdef( self.__bsc, self.def_id) )

class instance_t(object):
    #represents some symbol
    def __init__( self, inst_id, bsc ):
        self.__bsc = bsc
        self.__inst_id = inst_id

    @property
    def inst_id(self):
        return self.__inst_id

    @utils.cached
    def name_type_attribute_mangled_name( self ):
        name = STRING()
        typ = TYP()
        attribute = ATR()
        if not BSCIinstInfo( self.__bsc, self.inst_id, byref( name ), byref( typ ), byref( attribute ) ):
            raise RuntimeError( "Unable to load information about instance(%s)" % str( self.__inst_id ) )
        undecorated_name = msvc_utils.undecorate_name( name.value )
        if undecorated_name.startswith( ' ?? ' ):
            undecorated_name = undecorated_name[4:]
        #BSCFormatDname( self.__bsc, name )
        return undecorated_name, typ, attribute, name.value

    @utils.cached
    def mangled_name(self):
        return self.name_type_attribute_mangled_name[3]

    @utils.cached
    def name(self):
        return self.name_type_attribute_mangled_name[0]

    @utils.cached
    def type(self):
        return self.name_type_attribute_mangled_name[1].value

    @utils.cached
    def attribute(self):
        return self.name_type_attribute_mangled_name[2].value

    @utils.cached
    def is_class(self):
        return self.type in [ enums.TYPES.STRUCNAM
                              , enums.TYPES.UNIONNAM
                              , enums.TYPES.CLASSNAM  ]

    def __str__( self ):
        tmp = []
        if enums.TYPES.has_value( self.type ):
            tmp.append( 'type( "%s" )' % enums.TYPES.name_of( self.type ) )
        if enums.ATTRIBUTES.has_value( self.attribute ):
            tmp.append( 'attribute( "%s" )' % enums.ATTRIBUTES.name_of( self.attribute ) )
        tmp.append( 'name( "%s" )' % self.name )
        tmp.append( 'mangled name( "%s" )' % self.mangled_name )
        return ', '.join( tmp )


    @utils.cached
    def definitions( self ):
        definitions_len = ULONG(0)
        definitions_ids = pointer( IDEF() )

        if not BSCGetDefArray( self.__bsc, self.inst_id, byref( definitions_ids ), byref( definitions_len ) ):
            raise RuntimeError( "Unable to call BSCGetDefArray" )

        definitions = map( lambda i: definition_t( definitions_ids[i], self.__bsc )
                           , range( definitions_len.value ) )

        BSCDisposeArray( self.__bsc, definitions_ids )
        return definitions

    @utils.cached
    def members( self ):
        instances_len = ULONG(0)
        instances_ids = pointer( IINST() )

        if not BSCGetMembersArray( self.__bsc, self.inst_id, enums.MBF.ALL, byref( instances_ids ), byref( instances_len ) ):
            raise RuntimeError( "Unable to call BSCGetMembersArray" )

        instances = map( lambda i: self.__bsc.create_instance( instances_ids[i] )
                         , range( instances_len.value ) )

        BSCDisposeArray( self.__bsc, instances_ids )
        return instances

    @utils.cached
    def used_symbols(self):
        instances_len = ULONG(0)
        instances_ids = pointer( IINST() )

        if not BSCGetUsesArray( self.__bsc, self.inst_id, enums.MBF.ALL, byref( instances_ids ), byref( instances_len ) ):
            raise RuntimeError( "Unable to call BSCGetUsesArray" )

        instances = map( lambda i: self.__bsc.create_instance( instances_ids[i] )
                         , range( instances_len.value ) )

        BSCDisposeArray( self.__bsc, instances_ids )
        return instances

    @utils.cached
    def base_classes(self):
        instances_len = ULONG(0)
        instances_ids = pointer( IINST() )

        if not BSCGetBaseArray( self.__bsc, self.inst_id, byref( instances_ids ), byref( instances_len ) ):
            raise RuntimeError( "Unable to call BSCGetBaseArray" )

        instances = map( lambda i: self.__bsc.create_instance( instances_ids[i] )
                         , range( instances_len.value ) )

        BSCDisposeArray( self.__bsc, instances_ids )
        return instances

    @utils.cached
    def derived_classes(self):
        instances_len = ULONG(0)
        instances_ids = pointer( IINST() )

        if not BSCGetDervArray( self.__bsc, self.inst_id, byref( instances_ids ), byref( instances_len ) ):
            raise RuntimeError( "Unable to call BSCGetDervArray" )

        instances = map( lambda i: self.__bsc.create_instance( instances_ids[i] )
                         , range( instances_len.value ) )

        BSCDisposeArray( self.__bsc, instances_ids )
        return instances

class module_t(object):
    #represents file
    def __init__( self, mod_id, bsc ):
        self.__bsc = bsc
        self.__mod_id = mod_id

    @property
    def mod_id( self ):
        return self.__mod_id

    @utils.cached
    def path( self ):
        name = STRING()
        BSCImodInfo(self.__bsc, self.__mod_id, byref(name))
        return name.value

    @utils.cached
    def instances( self ):
        instances_len = ULONG(0)
        instances_ids = pointer( IINST() )

        if not BSCGetModuleContents( self.__bsc, self.mod_id, enums.MBF.ALL, byref( instances_ids ), byref( instances_len ) ):
            raise RuntimeError( "Unable to call BSCGetModuleContents" )

        instances = map( lambda i: self.__bsc.create_instance( instances_ids[i] )
                         , range( instances_len.value ) )

        BSCDisposeArray( self.__bsc, instances_ids )
        return instances

class reader_t( object ):
    def __init__( self, bsc_file ):
        self.logger = utils.loggers.pdb_reader
        self.logger.setLevel(logging.INFO)

        self.__bsc_file = bsc_file
        self.__bsc = pointer( Bsc() )
        if not BSCOpen( self.__bsc_file, byref( self.__bsc ) ):
            raise RuntimeError( "Unable to open bsc file '%s'" % self.__bsc_file )

        self.__instances_cache = {} #inst id : instance_t
        self.__bsc.create_instance = lambda inst_id: self.__create_instance( inst_id )

    @utils.cached
    def instances(self):
        return self.__instances_cache.values()

    def __create_instance( self, inst_id ):
        try:
            return self.__instances_cache[ inst_id ]
        except KeyError:
            inst = instance_t( inst_id, self.__bsc )
            self.__instances_cache[ inst_id ] = inst
            return inst

    def load_instances( self ):
        instances_len = ULONG(0)
        instances_ids = pointer( IINST() )

        if not BSCGetAllGlobalsArray( self.__bsc, enums.MBF.ALL, byref( instances_ids ), byref( instances_len ) ):
            raise RuntimeError( "Unable to load all globals symbols" )

        for i in range( instances_len.value ):
            self.__create_instance( instances_ids[i] )

        BSCDisposeArray( self.__bsc, instances_ids )

    @utils.cached
    def is_case_sensitive( self ):
        return bool( BSCFCaseSensitive( self.__bsc ) )

    @utils.cached
    def files(self):
        module_ids = pointer( IMOD() )
        module_len = ULONG()
        bs = BSC_STAT()

        if not BSCGetAllModulesArray( self.__bsc, module_ids, byref(module_len) ):
            raise RuntimeError( "Unable to load all modules" )

        modules = map( lambda i: module_t( module_ids[i], self.__bsc )
                       , range( module_len.value ) )

        BSCDisposeArray( self.__bsc, module_ids )

        return modules

    def print_stat( self ):
        stat = BSC_STAT()
        BSCGetStatistics( self.__bsc, byref( stat ) )
        for f, t in stat._fields_:
            print '%s: %s' % ( f, str( getattr( stat, f) ) )

    def print_classes(self, file_name=None):
        for m in self.files:
            if file_name and m.path != file_name:
                continue
            print 'File: ', m.path
            if m.instances:
                print '\tInstances:'
                for inst in m.instances:
                    print '\t\t', str(inst)
                    if inst.definitions:
                        print '\t\t\tDefinitions:'
                        for definition in inst.definitions:
                            print '\t\t\t\t', str( definition )
                    if inst.members:
                        print '\t\t\tMembers:'
                        for member in inst.members:
                            print '\t\t\t\t', str( member )
                    if inst.used_symbols:
                        print '\t\t\tUsed symbols:'
                        for used_symbol in inst.used_symbols:
                            print '\t\t\t\t', str( used_symbol )
                    if inst.base_classes:
                        print '\t\t\tBase classes:'
                        for base_class in inst.base_classes:
                            print '\t\t\t\t', str( base_class )
                    if inst.derived_classes:
                        print '\t\t\tDerived classes:'
                        for derived_class in inst.derived_classes:
                            print '\t\t\t\t', str( derived_class )

    def __del__( self ):
        if self.__bsc:
            BSCClose( self.__bsc )
