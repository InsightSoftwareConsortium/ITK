from __future__ import print_function

import os
import sys
import ctypes
import logging
import msvc_details
from ctypes import *
from ctypes.wintypes import ULONG
from ctypes.wintypes import DWORD
from ctypes.wintypes import BOOL
from ctypes.wintypes import BYTE
from ctypes.wintypes import WORD
from ctypes.wintypes import UINT

sys.path.append( r'../..' )

from pygccxml import utils
from pygccxml import declarations


STRING = c_char_p
_libraries = {}
_libraries['msvcr70.dll'] = CDLL(msvc_details.msvcr_path, mode=RTLD_GLOBAL)
_libraries['msbsc70.dll'] = CDLL(msvc_details.msbsc_path, mode=RTLD_GLOBAL)


qyMac = 9
refreshAllOp = 4
qyDervOf = 7
delOp = 1
qyImpMembers = 8
changeOp = 2
qyRefs = 4
qyCalls = 2
changeIinstOp = 3
qyContains = 1
qyCalledBy = 3
noOp = 5
qyBaseOf = 6
qyNil = 0
addOp = 0
qyDefs = 5
PULONG = POINTER(ULONG)
USHORT = c_ushort
PUSHORT = POINTER(USHORT)
UCHAR = c_ubyte
PUCHAR = POINTER(UCHAR)
PSZ = STRING
FLOAT = c_float
PFLOAT = POINTER(FLOAT)
PBOOL = POINTER(BOOL)
LPBOOL = POINTER(BOOL)
PBYTE = POINTER(BYTE)
LPBYTE = POINTER(BYTE)
PINT = POINTER(c_int)
LPINT = POINTER(c_int)
PWORD = POINTER(WORD)
LPWORD = POINTER(WORD)
LPLONG = POINTER(c_long)
PDWORD = POINTER(DWORD)
LPDWORD = POINTER(DWORD)
LPVOID = c_void_p
LPCVOID = c_void_p
INT = c_int
PUINT = POINTER(c_uint)
ULONG_PTR = POINTER(ULONG)
NI = ULONG
IINST = ULONG
IREF = ULONG
IDEF = ULONG
IMOD = USHORT
LINE = USHORT
TYP = BYTE
ATR = USHORT
ATR32 = ULONG
MBF = ULONG
SZ = STRING
SZ_CONST = STRING

class Bsc(Structure):
    pass

# values for enumeration 'OPERATION'
OPERATION = c_int # enum
class IinstInfo(Structure):
    pass
IinstInfo._fields_ = [
    ('m_iinst', IINST),
    ('m_szName', SZ_CONST),
    ('m_ni', NI),
]
class BSC_STAT(Structure):
    pass
BSC_STAT._fields_ = [
    ('cDef', ULONG),
    ('cRef', ULONG),
    ('cInst', ULONG),
    ('cMod', ULONG),
    ('cUseLink', ULONG),
    ('cBaseLink', ULONG),
]
class NiQ(Structure):
    pass
NiQ._fields_ = [
    ('m_iinstOld', IINST),
    ('m_iInfoNew', IinstInfo),
    ('m_op', OPERATION),
    ('m_typ', TYP),
]
pfnNotifyChange = CFUNCTYPE(BOOL, POINTER(NiQ), ULONG, ULONG_PTR)

# values for enumeration '_qy_'
_qy_ = c_int # enum
QY = _qy_
Bsc._fields_ = [
]
BSCOpen = _libraries['msbsc70.dll'].BSCOpen
BSCOpen.restype = BOOL
BSCOpen.argtypes = [SZ_CONST, POINTER(POINTER(Bsc))]
BSCClose = _libraries['msbsc70.dll'].BSCClose
BSCClose.restype = BOOL
BSCClose.argtypes = [POINTER(Bsc)]
BSCIinstInfo = _libraries['msbsc70.dll'].BSCIinstInfo
BSCIinstInfo.restype = BOOL
BSCIinstInfo.argtypes = [POINTER(Bsc), IINST, POINTER(SZ), POINTER(TYP), POINTER(ATR)]
BSCIrefInfo = _libraries['msbsc70.dll'].BSCIrefInfo
BSCIrefInfo.restype = BOOL
BSCIrefInfo.argtypes = [POINTER(Bsc), IREF, POINTER(SZ), POINTER(LINE)]
BSCIdefInfo = _libraries['msbsc70.dll'].BSCIdefInfo
BSCIdefInfo.restype = BOOL
BSCIdefInfo.argtypes = [POINTER(Bsc), IDEF, POINTER(SZ), POINTER(LINE)]
BSCImodInfo = _libraries['msbsc70.dll'].BSCImodInfo
BSCImodInfo.restype = BOOL
BSCImodInfo.argtypes = [POINTER(Bsc), IMOD, POINTER(SZ)]
BSCSzFrTyp = _libraries['msbsc70.dll'].BSCSzFrTyp
BSCSzFrTyp.restype = SZ
BSCSzFrTyp.argtypes = [POINTER(Bsc), TYP]
BSCSzFrAtr = _libraries['msbsc70.dll'].BSCSzFrAtr
BSCSzFrAtr.restype = SZ
BSCSzFrAtr.argtypes = [POINTER(Bsc), ATR]
BSCGetIinstByvalue = _libraries['msbsc70.dll'].BSCGetIinstByvalue
BSCGetIinstByvalue.restype = BOOL
BSCGetIinstByvalue.argtypes = [POINTER(Bsc), SZ, TYP, ATR, POINTER(IINST)]
BSCGetOverloadArray = _libraries['msbsc70.dll'].BSCGetOverloadArray
BSCGetOverloadArray.restype = BOOL
BSCGetOverloadArray.argtypes = [POINTER(Bsc), SZ, MBF, POINTER(POINTER(IINST)), POINTER(ULONG)]
BSCGetUsedByArray = _libraries['msbsc70.dll'].BSCGetUsedByArray
BSCGetUsedByArray.restype = BOOL
BSCGetUsedByArray.argtypes = [POINTER(Bsc), IINST, MBF, POINTER(POINTER(IINST)), POINTER(ULONG)]
BSCGetUsesArray = _libraries['msbsc70.dll'].BSCGetUsesArray
BSCGetUsesArray.restype = BOOL
BSCGetUsesArray.argtypes = [POINTER(Bsc), IINST, MBF, POINTER(POINTER(IINST)), POINTER(ULONG)]
BSCGetBaseArray = _libraries['msbsc70.dll'].BSCGetBaseArray
BSCGetBaseArray.restype = BOOL
BSCGetBaseArray.argtypes = [POINTER(Bsc), IINST, POINTER(POINTER(IINST)), POINTER(ULONG)]
BSCGetDervArray = _libraries['msbsc70.dll'].BSCGetDervArray
BSCGetDervArray.restype = BOOL
BSCGetDervArray.argtypes = [POINTER(Bsc), IINST, POINTER(POINTER(IINST)), POINTER(ULONG)]
BSCGetMembersArray = _libraries['msbsc70.dll'].BSCGetMembersArray
BSCGetMembersArray.restype = BOOL
BSCGetMembersArray.argtypes = [POINTER(Bsc), IINST, MBF, POINTER(POINTER(IINST)), POINTER(ULONG)]
BSCGetDefArray = _libraries['msbsc70.dll'].BSCGetDefArray
BSCGetDefArray.restype = BOOL
BSCGetDefArray.argtypes = [POINTER(Bsc), IINST, POINTER(POINTER(IREF)), POINTER(ULONG)]
BSCGetRefArray = _libraries['msbsc70.dll'].BSCGetRefArray
BSCGetRefArray.restype = BOOL
BSCGetRefArray.argtypes = [POINTER(Bsc), IINST, POINTER(POINTER(IREF)), POINTER(ULONG)]
BSCGetModuleContents = _libraries['msbsc70.dll'].BSCGetModuleContents
BSCGetModuleContents.restype = BOOL
BSCGetModuleContents.argtypes = [POINTER(Bsc), IMOD, MBF, POINTER(POINTER(IINST)), POINTER(ULONG)]
BSCGetModuleByName = _libraries['msbsc70.dll'].BSCGetModuleByName
BSCGetModuleByName.restype = BOOL
BSCGetModuleByName.argtypes = [POINTER(Bsc), SZ, POINTER(IMOD)]
BSCGetAllModulesArray = _libraries['msbsc70.dll'].BSCGetAllModulesArray
BSCGetAllModulesArray.restype = BOOL
BSCGetAllModulesArray.argtypes = [POINTER(Bsc), POINTER(POINTER(IMOD)), POINTER(ULONG)]
BSCDisposeArray = _libraries['msbsc70.dll'].BSCDisposeArray
BSCDisposeArray.restype = None
BSCDisposeArray.argtypes = [POINTER(Bsc), c_void_p]
BSCFormatDname = _libraries['msbsc70.dll'].BSCFormatDname
BSCFormatDname.restype = SZ
BSCFormatDname.argtypes = [POINTER(Bsc), SZ]
BSCFInstFilter = _libraries['msbsc70.dll'].BSCFInstFilter
BSCFInstFilter.restype = BOOL
BSCFInstFilter.argtypes = [POINTER(Bsc), IINST, MBF]
BSCIinstFrIref = _libraries['msbsc70.dll'].BSCIinstFrIref
BSCIinstFrIref.restype = IINST
BSCIinstFrIref.argtypes = [POINTER(Bsc), IREF]
BSCIinstFrIdef = _libraries['msbsc70.dll'].BSCIinstFrIdef
BSCIinstFrIdef.restype = IINST
BSCIinstFrIdef.argtypes = [POINTER(Bsc), IDEF]
BSCIinstContextIref = _libraries['msbsc70.dll'].BSCIinstContextIref
BSCIinstContextIref.restype = IINST
BSCIinstContextIref.argtypes = [POINTER(Bsc), IREF]
BSCGetStatistics = _libraries['msbsc70.dll'].BSCGetStatistics
BSCGetStatistics.restype = BOOL
BSCGetStatistics.argtypes = [POINTER(Bsc), POINTER(BSC_STAT)]
BSCGetModuleStatistics = _libraries['msbsc70.dll'].BSCGetModuleStatistics
BSCGetModuleStatistics.restype = BOOL
BSCGetModuleStatistics.argtypes = [POINTER(Bsc), IMOD, POINTER(BSC_STAT)]
BSCFCaseSensitive = _libraries['msbsc70.dll'].BSCFCaseSensitive
BSCFCaseSensitive.restype = BOOL
BSCFCaseSensitive.argtypes = [POINTER(Bsc)]
BSCSetCaseSensitivity = _libraries['msbsc70.dll'].BSCSetCaseSensitivity
BSCSetCaseSensitivity.restype = BOOL
BSCSetCaseSensitivity.argtypes = [POINTER(Bsc), BOOL]
BSCGetAllGlobalsArray = _libraries['msbsc70.dll'].BSCGetAllGlobalsArray
BSCGetAllGlobalsArray.restype = BOOL
BSCGetAllGlobalsArray.argtypes = [POINTER(Bsc), MBF, POINTER(POINTER(IINST)), POINTER(ULONG)]
BSCSzFrAtr2 = _libraries['msbsc70.dll'].BSCSzFrAtr2
BSCSzFrAtr2.restype = SZ
BSCSzFrAtr2.argtypes = [POINTER(Bsc), ATR32]
BSCIinstInfo2 = _libraries['msbsc70.dll'].BSCIinstInfo2
BSCIinstInfo2.restype = BOOL
BSCIinstInfo2.argtypes = [POINTER(Bsc), IINST, POINTER(SZ), POINTER(TYP), POINTER(ATR32)]
BSCGetIinstByvalue2 = _libraries['msbsc70.dll'].BSCGetIinstByvalue2
BSCGetIinstByvalue2.restype = BOOL
BSCGetIinstByvalue2.argtypes = [POINTER(Bsc), SZ, TYP, ATR32, POINTER(IINST)]
OpenBSCQuery = _libraries['msbsc70.dll'].OpenBSCQuery
OpenBSCQuery.restype = BOOL
OpenBSCQuery.argtypes = [POINTER(Bsc)]
CloseBSCQuery = _libraries['msbsc70.dll'].CloseBSCQuery
CloseBSCQuery.restype = BOOL
CloseBSCQuery.argtypes = []
BOB = ULONG
InitBSCQuery = _libraries['msbsc70.dll'].InitBSCQuery
InitBSCQuery.restype = BOOL
InitBSCQuery.argtypes = [QY, BOB]
BobNext = _libraries['msbsc70.dll'].BobNext
BobNext.restype = BOB
BobNext.argtypes = []
BobFrName = _libraries['msbsc70.dll'].BobFrName
BobFrName.restype = BOB
BobFrName.argtypes = [SZ]
LszNameFrBob = _libraries['msbsc70.dll'].LszNameFrBob
LszNameFrBob.restype = SZ
LszNameFrBob.argtypes = [BOB]
CLS = USHORT

class enums:
    class MBF(utils.enum):
        NIL       = 0x000
        VARS      = 0x001
        FUNCS     = 0x002
        MACROS    = 0x004
        TYPES     = 0x008
        CLASS     = 0x010
        INCL      = 0x020
        MSGMAP    = 0x040
        DIALOGID  = 0x080
        LIBRARY   = 0x100
        IMPORT    = 0x200
        TEMPLATE  = 0x400
        NAMESPACE = 0x800
        ALL       = 0xFFF

    class TYPES(utils.enum):
        FUNCTION  = 0x01
        LABEL     = 0x02
        PARAMETER = 0x03
        VARIABLE  = 0x04
        CONSTANT  = 0x05
        MACRO     = 0x06
        TYPEDEF   = 0x07
        STRUCNAM  = 0x08
        ENUMNAM   = 0x09
        ENUMMEM   = 0x0A
        UNIONNAM  = 0x0B
        SEGMENT   = 0x0C
        GROUP     = 0x0D
        PROGRAM   = 0x0E
        CLASSNAM  = 0x0F
        MEMFUNC   = 0x10
        MEMVAR    = 0x11

    class ATTRIBUTES(utils.enum):
        LOCAL     = 0x001
        STATIC    = 0x002
        SHARED    = 0x004
        NEAR      = 0x008
        COMMON    = 0x010
        DECL_ONLY = 0x020
        PUBLIC    = 0x040
        NAMED     = 0x080
        MODULE    = 0x100
        VIRTUAL   = 0x200
        PRIVATE   = 0x400
        PROTECT   = 0x800

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
        undecorated_name = BSCFormatDname( self.__bsc, name )
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

class bsc_reader_t( object ):
    def __init__( self, bsc_file ):
        self.logger = utils.loggers.pdb_reader
        self.logger.setLevel(logging.INFO)

        self.__bsc_file = bsc_file
        self.__bsc = pointer( Bsc() )
        if not BSCOpen( self.__bsc_file, byref( self.__bsc ) ):
            raise RuntimeError( "Unable to open bsc file '%s'" % self.__bsc_file )

        self.__instances_cache = {} #inst id : instance_t
        self.__bsc.create_instance = lambda inst_id: self.__create_instance( inst_id )

    def __create_instance( self, inst_id ):
        try:
            return self.__instances_cache[ inst_id ]
        except KeyError:
            inst = instance_t( inst_id, self.__bsc )
            self.__instances_cache[ inst_id ] = inst
            return inst

    def query_all_instances( self ):
        instances_len = ULONG(0)
        instances = pointer( IINST() )

        if not BSCGetAllGlobalsArray( self.__bsc, enums.MBF.ALL, byref( instances ), byref( instances_len ) ):
            raise RuntimeError( "Unable to load all globals symbols" )
        for i in range( instances_len.value ):
            self.__instances.append( instances[i] )
        BSCDisposeArray( self.__bsc, instances )

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
            print('%s: %s' % ( f, str( getattr( stat, f) ) ))

    def print_classes(self, file_name=None):
        for m in self.files:
            if file_name and m.path != file_name:
                continue
            print('File: ', m.path)
            if m.instances:
                print('\tInstances:')
                for inst in m.instances:
                    print('\t\t', str(inst))
                    if inst.definitions:
                        print('\t\t\tDefinitions:')
                        for definition in inst.definitions:
                            print('\t\t\t\t', str( definition ))
                    if inst.members:
                        print('\t\t\tMembers:')
                        for member in inst.members:
                            print('\t\t\t\t', str( member ))
                    if inst.used_symbols:
                        print('\t\t\tUsed symbols:')
                        for used_symbol in inst.used_symbols:
                            print('\t\t\t\t', str( used_symbol ))
                    if inst.base_classes:
                        print('\t\t\tBase classes:')
                        for base_class in inst.base_classes:
                            print('\t\t\t\t', str( base_class ))
                    if inst.derived_classes:
                        print('\t\t\tDerived classes:')
                        for derived_class in inst.derived_classes:
                            print('\t\t\t\t', str( derived_class ))

    def __del__( self ):
        if self.__bsc:
            BSCClose( self.__bsc )

if __name__ == '__main__':
    control_bsc = r'xxx.bsc'
    reader = bsc_reader_t( control_bsc )
    reader.print_stat()
    print('is_case_sensitive', reader.is_case_sensitive)
    #~ reader.query_all_instances()
    #reader.files
    reader.print_classes( )#r'c:\dev\produce_pdb\produce_pdb.cpp')
