interface ZIF_CMT_SYSTEM
  public .


  types:
    BEGIN OF ts_display,
      start_col TYPE i,
      start_row TYPE i,
      end_col   TYPE i,
      end_row   TYPE i,
      title     TYPE string,
    END OF ts_display .

  methods RENDER_TEXTAREA
    returning
      value(RESULT) type ZIF_CMT_MANAGER=>TT_HTML_DATA .
  methods RENDER_COMMENTS
    returning
      value(RESULT) type ZIF_CMT_MANAGER=>TT_HTML_DATA .
  methods ADD_COMMENT
    importing
      !TEXT type CLIKE
      !PARENT_COMMENT type CLIKE optional .
  methods DISLPLAY_IN_CONTAINERS
    importing
      !SECTION_CONTAINER type CLIKE
      !TEXTAREA_CONTAINER type CLIKE .
  methods DISPLAY
    importing
      !SETTINGS type TS_DISPLAY optional .
  methods EXPLODE_SUBCOMMENTS
    importing
      !COMMENT_ID type SYSUUID_C26 .
  methods LIKE_COMMENT
    importing
      !COMMENT_ID type SYSUUID_C26 .
  methods DISLIKE_COMMENT
    importing
      !COMMENT_ID type SYSUUID_C26 .
  methods DELETE_COMMENT
    importing
      !COMMENT_ID type SYSUUID_C26 .
  methods FREE .
  methods LOAD_THEME .
  methods MARK_AS_READ
    changing
      !COMMENTS type ZIF_CMT_MANAGER=>TT_COMMENTS .
  methods NOTIFY_TO_READ
    returning
      value(RESULT) type I .
  methods OPEN_REPLY_AREA
    importing
      !COMMENT_ID type SYSUUID_C26 .
  methods REPLY
    importing
      !PARENT_COMMENT type CLIKE
      !COMMENT_TEXT type CLIKE .
  methods GET_COMMENTS
    returning
      value(RESULT) type ZIF_CMT_MANAGER=>TT_COMMENTS .
endinterface.
