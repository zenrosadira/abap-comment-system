interface ZIF_CMT_THEME
  public .


  methods RENDER_COMMENTS
    returning
      value(RESULT) type ZIF_CMT_MANAGER=>TT_HTML_DATA .
  methods RENDER_REPLY_TEXTAREA
    importing
      !PARENT_COMMENT type CLIKE
    returning
      value(RESULT) type ZIF_CMT_MANAGER=>TT_HTML_DATA .
  methods RENDER_TEXTAREA
    returning
      value(RESULT) type ZIF_CMT_MANAGER=>TT_HTML_DATA .
  methods RENDER_TEXTAREA_CARD
    returning
      value(RESULT) type ZIF_CMT_MANAGER=>TT_HTML_DATA .
endinterface.
