# Dashboard is opened for submissions for a 24 hour period starting at
# the specified NIGHLY_START_TIME. Time is specified in 24 hour format.
SET (NIGHTLY_START_TIME "1:00:00 EDT")

# Dart server to submit results (used by client)
SET (DROP_SITE "www.itk.org")
SET (DROP_LOCATION "/incoming")
SET (DROP_SITE_USER "ftpuser")
SET (DROP_SITE_PASSWORD "public")
SET (DROP_METHOD "ftp")
SET (TRIGGER_SITE 
       "http://${DROP_SITE}/cgi-bin/Submit-Insight-TestingResults.pl")

# Project Home Page
SET (PROJECT_URL "http://www.itk.org/")

# Dart server configuration 
SET (ROLLUP_URL "http://${DROP_SITE}/cgi-bin/insight-rollup-dashboard.sh")
SET (CVS_WEB_URL "http://${DROP_SITE}/cgi-bin/cvsweb.cgi/Insight/")
SET (CVS_WEB_CVSROOT "Insight")

OPTION(BUILD_DOXYGEN "Build source documentation using doxygen" "Off")
SET (DOXYGEN_CONFIG "${PROJECT_BINARY_DIR}/doxygen.config" )
SET (USE_DOXYGEN "On")
SET (DOXYGEN_URL "http://${DROP_SITE}/Insight/Doxygen/html/" )

SET (USE_GNATS "On")
SET (GNATS_WEB_URL "http://${DROP_SITE}/cgi-bin/gnatsweb.pl/Insight/")

# Continuous email delivery variables
SET (CONTINUOUS_FROM "lorensen@crd.ge.com")
SET (SMTP_MAILHOST "public.kitware.com")
SET (CONTINUOUS_MONITOR_LIST "lorensen@crd.ge.com millerjv@crd.ge.com")
SET (CONTINUOUS_BASE_URL "http://www.itk.org/Testing")

MARK_AS_ADVANCED(BUILD_DOXYGEN)
