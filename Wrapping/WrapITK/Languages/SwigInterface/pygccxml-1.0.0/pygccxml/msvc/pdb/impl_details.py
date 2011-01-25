from . import enums
from pygccxml import declarations

def guess_class_type( udt_kind ):
    if enums.UdtKind.UdtStruct == udt_kind:
        return declarations.CLASS_TYPES.STRUCT
    elif enums.UdtKind.UdtClass == udt_kind:
        return declarations.CLASS_TYPES.CLASS
    else:
        return declarations.CLASS_TYPES.UNION

class full_name_splitter_t( object ):
    def __init__( self, full_name ):
        self.__full_name = full_name
        self.__identifiers = self.__split_scope_identifiers()
        self.__scope_identifiers = None

    @property
    def name( self ):
        return self.__identifiers[-1]

    @property
    def scope_names( self ):
        if None is self.__scope_identifiers:
            self.__scope_identifiers = []# ['::']
            for i in range( len(self.__identifiers) - 1):
                self.__scope_identifiers.append( '::'.join( self.__identifiers[0:i+1] ) )
        return self.__scope_identifiers

    @property
    def identifiers( self ):
        return self.__identifiers

    def __split_scope_identifiers( self ):
        try:
            result = []
            tmp = self.__full_name.split( '::' )
            tmp.reverse()
            while tmp:
                token = tmp.pop()
                less_count = token.count( '<' )
                greater_count = token.count( '>' )
                if less_count != greater_count:
                    while less_count != greater_count and tmp:
                        next_token = tmp.pop()
                        token = token + '::' + next_token
                        less_count += next_token.count( '<' )
                        greater_count += next_token.count( '>' )
                result.append( token )
            return result
        except Exception, err:
            msg = 'Unable to split scope for identifiers. The full scope name is: "%s". Error: %s'
            msg = msg % ( self.__full_name, str(err) )
            raise RuntimeError( msg )

__name_splitters = {}
def get_name_splitter( full_name ):
    try:
        return __name_splitters[full_name]
    except KeyError:
        splitter = full_name_splitter_t( full_name )
        __name_splitters[full_name] = splitter
        return splitter

if '__main__' == __name__:
    name = "boost::detail::is_base_and_derived_impl2<engine_objects::universal_base_t,engine_objects::erroneous_transactions_file_configuration_t>::Host"
    fnsp = full_name_splitter_t( name )
    for x in fnsp.scope_names:
        print x

    fnsp = full_name_splitter_t( 'x' )
    for x in fnsp.scope_names:
        print x
