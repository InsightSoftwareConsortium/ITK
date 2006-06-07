# Dashboard is opened for submissions for a 24 hour period starting at
# the specified NIGHLY_START_TIME. Time is specified in 24 hour format.
SET (NIGHTLY_START_TIME "21:00:00 EDT")

# Dart server to submit results (used by client)
IF(NOT DROP_METHOD)
  SET (DROP_METHOD "http")
ENDIF(NOT DROP_METHOD)
IF(DROP_METHOD MATCHES http)
  SET (DROP_SITE "public.kitware.com")
  SET (DROP_LOCATION "/cgi-bin/HTTPUploadDartFile.cgi")
ELSE(DROP_METHOD MATCHES http)
  SET (DROP_SITE "www.itk.org")
  SET (DROP_LOCATION "/incoming")
  SET (DROP_SITE_USER "ftpuser")
  SET (DROP_SITE_PASSWORD "public")
ENDIF(DROP_METHOD MATCHES http)

SET (TRIGGER_SITE 
  "http://${DROP_SITE}/cgi-bin/Submit-Insight-TestingResults.cgi")

# Project Home Page
SET (PROJECT_URL "http://www.itk.org/")

# Dart server configuration 
SET (ROLLUP_URL "http://${DROP_SITE}/cgi-bin/insight-rollup-dashboard.sh")
SET (CVS_WEB_URL "http://${DROP_SITE}/cgi-bin/viewcvs.cgi/")
SET (CVS_WEB_CVSROOT "Insight")

OPTION(BUILD_DOXYGEN "Build source documentation using doxygen" "Off")
SET (DOXYGEN_CONFIG "${PROJECT_BINARY_DIR}/doxygen.config" )
SET (USE_DOXYGEN "On")
SET (DOXYGEN_URL "http://${DROP_SITE}/Insight/Doxygen/html/" )

SET (USE_GNATS "On")
SET (GNATS_WEB_URL "http://${DROP_SITE}/Bug/query.php?projects=6&status%5B%5D=1&status%5B%5D=2&status%5B%5D=3&status%5B%5D=4&status%5B%5D=6&op=doquery")

# Continuous email delivery variables
SET (CONTINUOUS_FROM "lorensen@crd.ge.com")
SET (SMTP_MAILHOST "public.kitware.com")
SET (CONTINUOUS_MONITOR_LIST "lorensen@crd.ge.com millerjv@crd.ge.com lorensen@nycap.rr.com")
SET (CONTINUOUS_BASE_URL "http://www.itk.org/Testing")

MARK_AS_ADVANCED(BUILD_DOXYGEN)
