WRAP_INCLUDE("<vector>")
WRAP_INCLUDE("<string>")
WRAP_INCLUDE("<list>")
# Now inlcude the wrap_SwigExtras.h file, which has a CSWIG directive in it
# ("renames") that aren't accomodated in the normal cableswig wrapper input 
# generation process.
WRAP_INCLUDE("wrap_SwigExtras.h")
