#!/usr/bin/env python

from __future__ import print_function

import sys
import re
import httplib
import os

# compile regular expression to pull out URLs in ITK
# The ignored characters an common delineators, and not strick to the standard
http_re = re.compile("(http://[^\s<>\{\}\|\]\[\)\(\"]*)")
http_dict = dict()

for arg in sys.argv[1:]:
    if not os.path.isfile( arg ):
        continue
    f = open( arg, "r" )
    for l in  f.readlines():
        mo =  http_re.search( l )
        if mo is not None:
            http_dict[mo.group(1)] = arg

    f.close()

if len( http_dict ) > 1:
    print("Found ", len( http_dict ), " unique URLS.")

# compile regular expression to pull out the server address and path
server_re = re.compile( "http://([^/]+)(/?[^\s]*)" )

for url,filename in http_dict.items():
    mo = server_re.search( url )
    server = mo.group( 1 )
    path = mo.group( 2 )

    try:
        #print("Verifying URL: ", url,)

        # connect to server and get the path
        conn = httplib.HTTPConnection( server )
        conn.request("GET", path )
        r1 = conn.getresponse()


        if  r1.status == httplib.OK:
            # URL is OK do nothing
            #print("   URL: ", url, r1.status, r1.reason)
            pass
        elif r1.status == httplib.MOVED_PERMANENTLY:
            print(filename,": ERROR (URL needs update): ", url)
            print(r1.status, r1.reason, " to: ", r1.getheader("location"))
        elif r1.status == httplib.FOUND:
            print(filename,": INFO URL: ", url, r1.status, r1.reason, " to: ", r1.getheader("location"))
            pass
        elif r1.status == httplib.FORBIDDEN:
            print(filename,": INFO URL: ", url, r1.status, r1.reason)
            pass
        elif r1.status == httplib.NOT_FOUND:
            print(filename,": ERROR URL: ", url, r1.status, r1.reason)
        else:
            print(filename, ": UNKNOWN URL: ", url, "\"", r1.status, "\"", r1.reason)
            pass


    except Exception as e:
        print()
        print(filename,": ERROR (exception): ", url)
        print(e)
    except:
        print(filename,": ERROR (exception): ", url)
        print("Unexpected error:", sys.exc_info()[0])
        raise
    finally:
        conn.close()
