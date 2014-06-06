from ... import utils #import utils from pygccxml package

class BasicType(utils.enum):
    btNoType   = 0
    btVoid     = 1
    btChar     = 2
    btWChar    = 3
    btInt      = 6
    btUInt     = 7
    btFloat    = 8
    btBCD      = 9
    btBool     = 10
    btLong     = 13
    btULong    = 14
    btCurrency = 25
    btDate     = 26
    btVariant  = 27
    btComplex  = 28
    btBit      = 29
    btBSTR     = 30
    btHresult  = 31




#Adding code, that was not generated for some reason.
class UdtKind(utils.enum):
   UdtStruct, UdtClass, UdtUnion = (0, 1, 2)

class CV_access_e(utils.enum):
   CV_private, CV_protected, CV_public = (1, 2, 3)

class NameSearchOptions(utils.enum):
   nsNone               = 0
   nsfCaseSensitive     = 0x1
   nsfCaseInsensitive   = 0x2
   nsfFNameExt          = 0x4
   nsfRegularExpression = 0x8
   nsfUndecoratedName   = 0x10

   # For backward compabibility:
   nsCaseSensitive           = nsfCaseSensitive
   nsCaseInsensitive         = nsfCaseInsensitive
   nsFNameExt = nsfFNameExt
   nsRegularExpression       = nsfRegularExpression | nsfCaseSensitive
   nsCaseInRegularExpression = nsfRegularExpression | nsfCaseInsensitive


class DataKind( utils.enum ):
   DataIsUnknown        = 0
   DataIsLocal          = 1
   DataIsStaticLocal    = 2
   DataIsParam          = 3
   DataIsObjectPtr      = 4
   DataIsFileStatic     = 5
   DataIsGlobal         = 6
   DataIsMember         = 7
   DataIsStaticMember   = 8
   DataIsConstant       = 9

