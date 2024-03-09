CLASS zcl_cmt_theme DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_cmt_theme.

    CLASS-METHODS get
      RETURNING VALUE(result) TYPE REF TO zif_cmt_theme.

    METHODS constructor.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ts_instance,
        theme_name TYPE c LENGTH 20,
        instance   TYPE REF TO zif_cmt_theme,
      END OF ts_instance.
    TYPES tt_instances TYPE TABLE OF ts_instance WITH KEY theme_name.
    TYPES:
      BEGIN OF ts_template,
        section_template  TYPE zif_cmt_manager=>tt_html_data,
        comment_template  TYPE zif_cmt_manager=>tt_html_data,
        textarea_template TYPE zif_cmt_manager=>tt_html_data,
      END OF ts_template.

    CLASS-DATA m_instance TYPE REF TO zif_cmt_theme.

    DATA m_template TYPE ts_template.

    CLASS-METHODS get_manager
      RETURNING VALUE(result) TYPE REF TO zif_cmt_manager.

    METHODS _load_templates.

    METHODS _render_comment
      IMPORTING comment_data     TYPE zif_cmt_manager=>ts_comment
                comment_template TYPE string_table
      RETURNING VALUE(result)    TYPE zif_cmt_manager=>tt_html_data.
ENDCLASS.



CLASS ZCL_CMT_THEME IMPLEMENTATION.


  METHOD constructor.
    _load_templates( ).
  ENDMETHOD.


  METHOD get.
    IF m_instance IS INITIAL.

      m_instance = NEW zcl_cmt_theme( ).

    ENDIF.

    result = m_instance.
  ENDMETHOD.


  METHOD get_manager.
    result = zcl_cmt_manager=>get_manager( ).
  ENDMETHOD.


  METHOD zif_cmt_theme~render_comments.
    DATA(comment_template_lines) = VALUE string_table( FOR _ctemp IN m_template-comment_template
                                                       ( CONV string( _ctemp-html ) ) ).

    DATA(section_template_lines) = VALUE string_table( FOR _stemp IN m_template-section_template
                                                       ( CONV string( _stemp-html ) ) ).

    DATA(comments_html) = VALUE zif_cmt_manager=>tt_html_data( FOR _comment IN comments
                                                               LET _comment_rendered = _render_comment(
                                                                   comment_data     = _comment
                                                                   comment_template = comment_template_lines ) IN
                                                               FOR _lines IN _comment_rendered
                                                               ( html = _lines ) ).

    LOOP AT section_template_lines INTO DATA(temp_line).

      IF temp_line = `#comments#`.
        APPEND LINES OF comments_html TO result.
      ELSE.
        APPEND VALUE #( html = temp_line ) TO result.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD zif_cmt_theme~render_textarea.
    result = m_template-textarea_template.

    DATA(user_full_name) = get_manager( )->get_user_full_name( sy-uname ).
    DATA(avatar_name)    = replace( val  = user_full_name
                                    sub  = ` `
                                    with = `+` ).

    REPLACE ALL OCCURRENCES OF `#avatar_name#`  IN TABLE result WITH avatar_name.
    REPLACE ALL OCCURRENCES OF `#label_send#`   IN TABLE result WITH TEXT-l01.
    REPLACE ALL OCCURRENCES OF `#label_cancel#` IN TABLE result WITH TEXT-l02.
  ENDMETHOD.


  METHOD _load_templates.
    IMPORT html = m_template-section_template FROM DATABASE wwwdata(ht) ID 'ZCMT_SECTION'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN TABLE m_template-section_template WITH ``.

    IMPORT html = m_template-comment_template FROM DATABASE wwwdata(ht) ID 'ZCMT_COMMENT'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN TABLE m_template-comment_template WITH ``.

    IMPORT html = m_template-textarea_template FROM DATABASE wwwdata(ht) ID 'ZCMT_TEXTAREA'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN TABLE m_template-textarea_template WITH ``.
  ENDMETHOD.


  METHOD _render_comment.
    DATA(comment_text)    = cl_abap_codepage=>convert_from( comment_data-comment_text ).
    DATA(comment_date)    = |{ comment_data-comment_date DATE = USER }|.
    DATA(comment_time)    = |{ comment_data-comment_time TIME = USER }|.
    DATA(author_name)     = get_manager( )->get_user_full_name( comment_data-comment_author ).
    DATA(upvotes)         = CONV string( lines( comment_data-user_upvoting ) ).
    DATA(own_comment)     = xsdbool( comment_data-comment_author = sy-uname ).
    DATA(already_upvoted) = xsdbool( line_exists( comment_data-user_upvoting[ table_line = sy-uname ] ) ).
    DATA(to_upvote)       = xsdbool( NOT already_upvoted = abap_true ).
    DATA(like_event)      = COND #( WHEN already_upvoted = abap_true THEN `DISLIKE` ELSE `LIKE` ).
    DATA(avatar_name)     = replace( val  = author_name
                                     sub  = ` `
                                     with = `+` ).

    LOOP AT comment_template INTO DATA(temp).

      DATA(html_line) = temp.

      DATA(condition) = substring_before( val = substring_after( val = html_line
                                                                 sub = `#if[` )
                                          sub = `]` ).
      IF condition IS NOT INITIAL.
        ASSIGN (condition) TO FIELD-SYMBOL(<condition>).
        IF sy-subrc = 0 AND <condition> = abap_false.
          CONTINUE.
        ELSE.
          html_line = replace( val  = html_line
                               sub  = |#if[{ condition }]|
                               with = `` ).
        ENDIF.
      ENDIF.

      html_line = replace( val  = html_line
                           sub  = `#avatar_name#`
                           with = avatar_name ).
      html_line = replace( val  = html_line
                           sub  = `#comment_author#`
                           with = author_name ).
      html_line = replace( val  = html_line
                           sub  = `#comment_subtitle#`
                           with = |{ comment_date } - { comment_time }| ).
      html_line = replace( val  = html_line
                           sub  = `#comment_text#`
                           with = comment_text ).
      html_line = replace( val  = html_line
                           sub  = `#like_event#`
                           with = like_event ).
      html_line = replace( val  = html_line
                           sub  = `#like_param#`
                           with = comment_data-comment_id ).
      html_line = replace( val  = html_line
                           sub  = `#upvotes#`
                           with = upvotes ).
      html_line = replace( val  = html_line
                           sub  = `#delete_param#`
                           with = comment_data-comment_id ).

      APPEND VALUE #( html = html_line ) TO result.

    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
