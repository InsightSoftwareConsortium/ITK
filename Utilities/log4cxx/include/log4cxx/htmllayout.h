/*
 * Copyright 2003,2004 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
 
#ifndef _LOG4CXX_HTML_LAYOUT_H
#define _LOG4CXX_HTML_LAYOUT_H

#include <log4cxx/layout.h>
#include <log4cxx/helpers/iso8601dateformat.h>

namespace log4cxx
{
  class HTMLLayout;
  typedef helpers::ObjectPtrT<HTMLLayout> HTMLLayoutPtr;
  
  /**
  This layout outputs events in a HTML table.
  */
  class LOG4CXX_EXPORT HTMLLayout : public Layout
  {
  protected:
    static String TRACE_PREFIX;

  private:
    /**
    A string constant used in naming the option for setting the the
    location information flag.  Current value of this string
    constant is <b>LocationInfo</b>.
    */
    static String LOCATION_INFO_OPTION;

    /**
    A string constant used in naming the option for setting the the
    HTML document title.  Current value of this string
    constant is <b>Title</b>.
    */
    static String TITLE_OPTION;

    // Print no location info by default
    bool locationInfo; //= false

    String title;

    helpers::ISO8601DateFormat dateFormat;

  public:
    DECLARE_LOG4CXX_OBJECT(HTMLLayout)
    BEGIN_LOG4CXX_CAST_MAP()
      LOG4CXX_CAST_ENTRY(HTMLLayout)
      LOG4CXX_CAST_ENTRY_CHAIN(Layout)
    END_LOG4CXX_CAST_MAP()

    HTMLLayout();

    /**
    The <b>LocationInfo</b> option takes a boolean value. By
    default, it is set to false which means there will be no location
    information output by this layout. If the the option is set to
    true, then the file name and line number of the statement
    at the origin of the log statement will be output.

    <p>If you are embedding this layout within an
    {@link net::SMTPAppender SMTPAppender} then make sure
    to set the <b>LocationInfo</b> option of that appender as well.
    */
    inline void setLocationInfo(bool flocationInfoag)
      { this->locationInfo = locationInfo; }

    /**
    Returns the current value of the <b>LocationInfo</b> option.
    */
    inline bool getLocationInfo() const
      { return locationInfo; }

    /**
    The <b>Title</b> option takes a String value. This option sets the
    document title of the generated HTML document.
    <p>Defaults to 'Log4cxx Log Messages'.
    */
    inline void setTitle(const String& title)
      { this->title = title; }

    /**
    Returns the current value of the <b>Title</b> option.
    */
    inline const String& getTitle() const
      { return title; }

    /**
    Returns the content type output by this layout, i.e "text/html".
    */
    virtual String getContentType() const { return _T("text/html"); }

    /**
    No options to activate.
    */
    virtual void activateOptions() {}

    /**
    Set options
    */
    virtual void setOption(const String& option, const String& value);

    virtual void format(ostream& output, const spi::LoggingEventPtr& event) const;

    /**
    Append appropriate HTML headers.
    */
    virtual void appendHeader(ostream& output);

    /**
    Append the appropriate HTML footers.
    */
    virtual void appendFooter(ostream& output);

    /**
    The HTML layout handles the throwable contained in logging
    events. Hence, this method return <code>false</code>.  */
    virtual bool ignoresThrowable() const
      { return false; }
      
  }; // class HtmlLayout
}  // namespace log4cxx

#endif // _LOG4CXX_HTML_LAYOUT_H
