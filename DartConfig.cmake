# Dart server to submit results (used by client)
SET (DROP_SITE "public.kitware.com")
SET (DROP_LOCATION "/incoming")
SET (DROP_SITE_USER "anonymous")
SET (DROP_SITE_PASSWORD "insight-tester@somewhere.com")
SET (TRIGGER_SITE 
       "http://${DROP_SITE}/cgi-bin/Submit-Insight-TestingResults.pl")

# Dart server configuration 
SET (CVS_WEB_URL "http://${DROP_SITE}/cgi-bin/itkcvsweb.cgi/Insight/")
SET (CVS_WEB_CVSROOT "Insight")
SET (DOXYGEN_URL "http://${DROP_SITE}/Insight/Doxygen/html/" )
SET (GNATS_WEB_URL "http://${DROP_SITE}/cgi-bin/gnatsweb.pl/Insight/")
