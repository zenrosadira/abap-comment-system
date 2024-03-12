class ZCL_CMT_THEME definition
  public
  final
  create private

  global friends ZCL_CMT_SYSTEM .

public section.

  interfaces ZIF_CMT_THEME .

  methods CONSTRUCTOR
    importing
      !SYSTEM type ref to ZIF_CMT_SYSTEM .
protected section.
private section.

  types:
    BEGIN OF ts_instance,
      theme_name TYPE c LENGTH 20,
      instance   TYPE REF TO zif_cmt_theme,
    END OF ts_instance .
  types:
    tt_instances TYPE TABLE OF ts_instance WITH KEY theme_name .
  types:
    BEGIN OF ts_template,
      section_template  TYPE zif_cmt_manager=>tt_html_data,
      comment_template  TYPE zif_cmt_manager=>tt_html_data,
      textarea_template TYPE zif_cmt_manager=>tt_html_data,
      textarea_card     TYPE zif_cmt_manager=>tt_html_data,
    END OF ts_template .

  constants:
    BEGIN OF c_placeholders,
               comments          TYPE string VALUE `<!--ABAP:comments:-->`,
               sub_comment_style TYPE string VALUE `/*ABAP:sub_comment:*/`,
               avatar_name       TYPE string VALUE `/*ABAP:avatar_name:*/`,
               avatar_dim        TYPE string VALUE `/*ABAP:avatar_dim:*/`,
               comment_author    TYPE string VALUE `<!--ABAP:comment_author:-->`,
               comment_subtitle  TYPE string VALUE `<!--ABAP:comment_subtitle:-->`,
               comment_content   TYPE string VALUE `<!--ABAP:comment_content:-->`,
               like_event        TYPE string VALUE `/*ABAP:like_event:*/`,
               comment_id        TYPE string VALUE `/*ABAP:comment_id:*/`,
               thumb_style       TYPE string VALUE `/*ABAP:thumb_style:*/`,
               disp_delete       TYPE string VALUE `/*ABAP:disp_delete:*/`,
               disp_reply        TYPE string VALUE `/*ABAP:disp_reply:*/`,
               disp_exp          TYPE string VALUE `/*ABAP:disp_exp:*/`,
               disp_sub          TYPE string VALUE `/*ABAP:disp_sub:*/`,
               explode_icon      TYPE string VALUE `/*ABAP:explode_icon:*/`,
               sub_comments      TYPE string VALUE `<!--ABAP:sub_comments:-->`,
               rep_text_area     TYPE string VALUE `<!--ABAP:reply_text_area:-->`,
               text_area         TYPE string VALUE `<!--ABAP:text_area:-->`,
               upvotes           TYPE string VALUE `<!--ABAP:upvotes:-->`,
               replies           TYPE string VALUE `<!--ABAP:replies:-->`,
               parent_comment    TYPE string VALUE `/*ABAP:parent_comment:*/`,
               text_rows         TYPE string VALUE `/*ABAP:text_rows:*/`,
               label_send        TYPE string VALUE `/*ABAP:label_send:*/`,
               label_cancel      TYPE string VALUE `<!--ABAP:label_cancel:-->`,
             END OF c_placeholders .
  data M_SYSTEM type ref to ZIF_CMT_SYSTEM .
  data M_TEMPLATE type TS_TEMPLATE .

  class-methods GET_MANAGER
    returning
      value(RESULT) type ref to ZIF_CMT_MANAGER .
  methods _LOAD_TEMPLATES .
  methods _RENDER_COMMENT
    importing
      !COMMENT_DATA type ZIF_CMT_MANAGER=>TS_COMMENT
    returning
      value(RESULT) type ZIF_CMT_MANAGER=>TT_HTML_DATA .
  methods _INJECT_CONTENTS
    importing
      !PLACEHOLDER type CLIKE
      !CONTENT type ZIF_CMT_MANAGER=>TT_HTML_DATA
      !KEEP_PLACEHOLDER type ABAP_BOOL optional
    changing
      !HTML_DATA type ZIF_CMT_MANAGER=>TT_HTML_DATA .
  methods _INJECT_ELEMENT
    importing
      !PLACEHOLDER type CLIKE
      !CONTENT type CLIKE
    changing
      !HTML_DATA type ZIF_CMT_MANAGER=>TT_HTML_DATA .
ENDCLASS.



CLASS ZCL_CMT_THEME IMPLEMENTATION.


  METHOD constructor.

    m_system = system.

    _load_templates( ).

  ENDMETHOD.


  METHOD get_manager.

    result = zcl_cmt_manager=>get_manager( ).

  ENDMETHOD.


  METHOD zif_cmt_theme~render_comments.

    result = m_template-section_template.

    DATA(comments) = m_system->get_comments( ).
    DELETE comments WHERE comment_parent IS NOT INITIAL.

    DATA(comments_html) = VALUE zif_cmt_manager=>tt_html_data(
      FOR _comment IN comments
      LET _comment_rendered = _render_comment( _comment ) IN
        FOR _lines IN _comment_rendered
        ( html = _lines ) ).

    _inject_contents(
      EXPORTING
        placeholder = c_placeholders-comments
        content     = comments_html
      CHANGING
        html_data   = result ).

  ENDMETHOD.


  METHOD zif_cmt_theme~render_reply_textarea.
    result = m_template-textarea_template.

    DATA(textarea_card) = zif_cmt_theme~render_textarea_card( ).

    _inject_contents(
      EXPORTING
        placeholder = c_placeholders-text_area
        content     = textarea_card
      CHANGING
        html_data   = result ).

    _inject_element(
      EXPORTING
        placeholder = c_placeholders-text_rows
        content     = `1`
      CHANGING
        html_data   = result ).

    _inject_element(
      EXPORTING
          placeholder = c_placeholders-parent_comment
          content     = parent_comment
      CHANGING
          html_data   = result ).

    _inject_element(
      EXPORTING
        placeholder = c_placeholders-sub_comment_style
        content     = `sub-comment`
      CHANGING
        html_data   = result ).

  ENDMETHOD.


  METHOD zif_cmt_theme~render_textarea.

    result = m_template-textarea_template.

    DATA(textarea_card) = zif_cmt_theme~render_textarea_card( ).

    _inject_contents(
      EXPORTING
        placeholder = c_placeholders-text_area
        content     = textarea_card
      CHANGING
        html_data   = result ).

    _inject_element(
      EXPORTING
        placeholder = c_placeholders-text_rows
        content     = `4`
      CHANGING
        html_data   = result ).

    _inject_element(
      EXPORTING
          placeholder = c_placeholders-parent_comment
          content     = ``
      CHANGING
          html_data   = result ).

    _inject_element(
      EXPORTING
        placeholder = c_placeholders-sub_comment_style
        content     = ``
      CHANGING
        html_data   = result ).

  ENDMETHOD.


  METHOD zif_cmt_theme~render_textarea_card.

    result = m_template-textarea_card.

    DATA(user_full_name) = get_manager( )->get_user_full_name( sy-uname ).
    DATA(avatar_name)    = replace( val = user_full_name sub = ` ` with = `+` ).

    _inject_element( EXPORTING placeholder = c_placeholders-avatar_name   content = avatar_name CHANGING html_data = result ).
    _inject_element( EXPORTING placeholder = c_placeholders-label_cancel  content = TEXT-l02    CHANGING html_data = result ).
    _inject_element( EXPORTING placeholder = c_placeholders-label_send    content = TEXT-l01    CHANGING html_data = result ).

  ENDMETHOD.


  METHOD _inject_contents.

    DATA result LIKE html_data.

    LOOP AT html_data INTO DATA(html_line).

      CASE html_line-html.

        WHEN placeholder.

          APPEND LINES OF content TO result.
          IF keep_placeholder = abap_true.
            APPEND html_line TO result.
          ENDIF.

        WHEN OTHERS.

          APPEND html_line TO result.

      ENDCASE.

    ENDLOOP.

    html_data = result.

  ENDMETHOD.


  METHOD _inject_element.

    REPLACE ALL OCCURRENCES OF placeholder IN TABLE html_data WITH content.

  ENDMETHOD.


  METHOD _load_templates.

    IMPORT html = m_template-section_template   FROM DATABASE wwwdata(ht) ID 'ZCMT_SECTION'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN TABLE m_template-section_template WITH ``.

    IMPORT html = m_template-comment_template   FROM DATABASE wwwdata(ht) ID 'ZCMT_COMMENT'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN TABLE m_template-comment_template WITH ``.

    IMPORT html = m_template-textarea_template  FROM DATABASE wwwdata(ht) ID 'ZCMT_TEXTAREA'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN TABLE m_template-textarea_template WITH ``.

    IMPORT html = m_template-textarea_card      FROM DATABASE wwwdata(ht) ID 'ZCMT_TEXTAREA_CARD'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN TABLE m_template-textarea_card WITH ``.

  ENDMETHOD.


  METHOD _render_comment.

    DATA(comments) = m_system->get_comments( ).

    DATA(comment_text)      = cl_abap_codepage=>convert_from( comment_data-comment_text ).
    DATA(comment_date)      = |{ comment_data-comment_date DATE = USER }|.
    DATA(comment_time)      = |{ comment_data-comment_time TIME = USER }|.
    DATA(comment_subtitle)  = |{ comment_date } - { comment_time }|.
    DATA(comment_id)        = comment_data-comment_id.
    DATA(author_name)       = get_manager( )->get_user_full_name( comment_data-comment_author ).
    DATA(upvotes)           = CONV string( lines( comment_data-user_upvoting ) ).
    DATA(disp_delete)       = COND #( WHEN comment_data-comment_author = sy-uname THEN `inherit` ELSE `none` ).
    DATA(disp_reply)        = COND #( WHEN comment_data-comment_parent IS INITIAL THEN `inherit` ELSE `none` ).
    DATA(already_upvoted)   = xsdbool( line_exists( comment_data-user_upvoting[ table_line = sy-uname ] ) ).
    DATA(like_event)        = COND #( WHEN already_upvoted = abap_true THEN `DISLIKE` ELSE `LIKE` ).
    DATA(thumb_style)       = COND #( WHEN already_upvoted = abap_true THEN `fas` ELSE `far` ).
    DATA(avatar_name)       = replace( val = author_name sub = ` ` with = `+` ).
    DATA(comment_rendered)  = VALUE zif_cmt_manager=>tt_html_data( ).
    DATA(avatar_dim)        = COND #( WHEN comment_data-comment_parent IS INITIAL THEN `50` ELSE `30` ).
    DATA(sub_comment)       = COND #( WHEN comment_data-comment_parent IS INITIAL THEN `` ELSE `sub-comment` ).
    DATA(num_replies)       = CONV string( REDUCE #(
      INIT i = 0 FOR _c IN comments WHERE ( comment_parent = comment_data-comment_id ) NEXT i = i + 1 ) ).
    DATA(disp_exp)          = COND #( WHEN num_replies > 0 THEN `inherit` ELSE `none` ).
    DATA(exp_icon)          = COND #( WHEN comment_data-sub_closed = abap_false THEN `fa-solid fa-square-minus` ELSE `fa-solid fa-square-plus` ).
    DATA(disp_sub)          = COND #( WHEN comment_data-sub_closed = abap_false THEN `inherit` ELSE `none` ).

    result = m_template-comment_template.

    _inject_element( EXPORTING placeholder = c_placeholders-avatar_name       content = avatar_name       CHANGING html_data = result ).
    _inject_element( EXPORTING placeholder = c_placeholders-comment_author    content = author_name       CHANGING html_data = result ).
    _inject_element( EXPORTING placeholder = c_placeholders-comment_subtitle  content = comment_subtitle  CHANGING html_data = result ).
    _inject_element( EXPORTING placeholder = c_placeholders-comment_content   content = comment_text      CHANGING html_data = result ).
    _inject_element( EXPORTING placeholder = c_placeholders-like_event        content = like_event        CHANGING html_data = result ).
    _inject_element( EXPORTING placeholder = c_placeholders-comment_id        content = comment_id        CHANGING html_data = result ).
    _inject_element( EXPORTING placeholder = c_placeholders-upvotes           content = upvotes           CHANGING html_data = result ).
    _inject_element( EXPORTING placeholder = c_placeholders-replies           content = num_replies       CHANGING html_data = result ).
    _inject_element( EXPORTING placeholder = c_placeholders-disp_delete       content = disp_delete       CHANGING html_data = result ).
    _inject_element( EXPORTING placeholder = c_placeholders-disp_reply        content = disp_reply        CHANGING html_data = result ).
    _inject_element( EXPORTING placeholder = c_placeholders-avatar_dim        content = avatar_dim        CHANGING html_data = result ).
    _inject_element( EXPORTING placeholder = c_placeholders-sub_comment_style content = sub_comment       CHANGING html_data = result ).
    _inject_element( EXPORTING placeholder = c_placeholders-thumb_style       content = thumb_style       CHANGING html_data = result ).
    _inject_element( EXPORTING placeholder = c_placeholders-disp_exp          content = disp_exp          CHANGING html_data = result ).
    _inject_element( EXPORTING placeholder = c_placeholders-explode_icon      content = exp_icon          CHANGING html_data = result ).
    _inject_element( EXPORTING placeholder = c_placeholders-disp_sub          content = disp_sub          CHANGING html_data = result ).

    CHECK comment_data-comment_parent IS INITIAL.

    DATA(reply_textarea) = zif_cmt_theme~render_reply_textarea( comment_data-comment_id ).
    _inject_contents( EXPORTING placeholder = c_placeholders-rep_text_area content = reply_textarea CHANGING html_data = result ).

    DELETE comments WHERE comment_parent <> comment_data-comment_id.

    DATA(sub_comments) = VALUE zif_cmt_manager=>tt_html_data(
      FOR _comment IN comments
      LET _comment_rendered = _render_comment( _comment  ) IN
        FOR _lines IN _comment_rendered
        ( html = _lines ) ).

    _inject_contents(
        EXPORTING placeholder       = c_placeholders-sub_comments
                  content           = sub_comments
                  keep_placeholder  = abap_true
        CHANGING  html_data         = result ).

  ENDMETHOD.
ENDCLASS.
