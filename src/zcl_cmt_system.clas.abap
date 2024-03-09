CLASS zcl_cmt_system DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zif_cmt_theme.

  PUBLIC SECTION.
    INTERFACES zif_cmt_system.

    CLASS-METHODS create_comment_system
      IMPORTING !object       TYPE data
      RETURNING VALUE(result) TYPE REF TO zif_cmt_system.

    METHODS constructor
      IMPORTING object_id  TYPE clike
                section_id TYPE clike.

  PRIVATE SECTION.
    DATA m_object_id     TYPE char40.
    DATA m_section_id    TYPE sysuuid_c26.
    DATA m_comments      TYPE zif_cmt_manager=>tt_comments.
    DATA m_sec_viewer    TYPE REF TO cl_gui_html_viewer.
    DATA m_mes_viewer    TYPE REF TO cl_gui_html_viewer.
    DATA m_theme         TYPE REF TO zif_cmt_theme.
    DATA m_sec_container TYPE REF TO cl_gui_custom_container.
    DATA m_mes_container TYPE REF TO cl_gui_custom_container.

    METHODS _display_textarea
      IMPORTING textarea_container TYPE clike.

    METHODS _display_comment_section
      IMPORTING section_container TYPE clike.

    METHODS _load_comments.

    METHODS _section_events
      FOR EVENT sapevent OF cl_gui_html_viewer
      IMPORTING !action
                !frame
                getdata
                postdata
                query_table.

    METHODS _text_area_events
      FOR EVENT sapevent OF cl_gui_html_viewer
      IMPORTING !action
                !frame
                getdata
                postdata
                query_table.

    CLASS-METHODS get_manager
      RETURNING VALUE(result) TYPE REF TO zif_cmt_manager.

    CLASS-METHODS _hash
      IMPORTING !data         TYPE data
      RETURNING VALUE(result) TYPE hash160.
ENDCLASS.


CLASS zcl_cmt_system IMPLEMENTATION.
  METHOD constructor.
    m_object_id  = object_id.
    m_section_id = section_id.

    _load_comments( ).

    zif_cmt_system~load_theme( ).
  ENDMETHOD.

  METHOD create_comment_system.
    DATA(object_id) = _hash( object ).

    SELECT SINGLE section_id FROM zcmt_object_t
      WHERE object_id = @object_id
      INTO @DATA(section_id).

    IF sy-subrc <> 0.

      section_id = get_manager( )->create_id( ).

      INSERT zcmt_object_t FROM @( VALUE #(
        object_id     = object_id
        section_id    = section_id
        creation_date = sy-datum
        creation_time = sy-uzeit ) ).

    ENDIF.

    result = NEW zcl_cmt_system( object_id  = object_id
                                 section_id = section_id ).
  ENDMETHOD.

  METHOD get_manager.
    result = zcl_cmt_manager=>get_manager( ).
  ENDMETHOD.

  METHOD zif_cmt_system~add_comment.
    DATA(comment_text) = cl_abap_codepage=>convert_to( CONV #( text ) ).
    DATA(comment_id)   = get_manager( )->create_id( ).

    INSERT zcmt_comment_t FROM @( VALUE #(
      section_id      = m_section_id
      comment_id      = comment_id
      comment_author  = sy-uname
      comment_date    = sy-datum
      comment_time    = sy-uzeit
      comment_text    = comment_text ) ).

    INSERT VALUE #( comment_id     = comment_id
                    comment_author = sy-uname
                    comment_date   = sy-datum
                    comment_time   = sy-uzeit
                    comment_text   = comment_text ) INTO m_comments INDEX 1.
  ENDMETHOD.

  METHOD zif_cmt_system~delete_comment.
    DELETE FROM zcmt_comment_t WHERE comment_id = @comment_id.
    DELETE FROM zcmt_upvotes_t WHERE comment_id = @comment_id.
    DELETE FROM zcmt_comread_t WHERE comment_id = @comment_id.

    DELETE m_comments WHERE comment_id = comment_id.
  ENDMETHOD.

  METHOD zif_cmt_system~dislike_comment.
    READ TABLE m_comments ASSIGNING FIELD-SYMBOL(<com>) WITH KEY comment_id = comment_id.
    CHECK sy-subrc = 0.

    DELETE <com>-user_upvoting WHERE table_line = sy-uname.

    DELETE FROM zcmt_upvotes_t WHERE comment_id = comment_id AND user_upvoting = sy-uname.
  ENDMETHOD.

  METHOD zif_cmt_system~dislplay_in_containers.
    _display_comment_section( section_container ).

    _display_textarea( textarea_container ).
  ENDMETHOD.

  METHOD zif_cmt_system~display.
    DATA(popup_settings) = settings.

    IF popup_settings-start_col IS INITIAL.
      popup_settings-start_col = 50.
    ENDIF.

    IF popup_settings-start_row IS INITIAL.
      popup_settings-start_row = 1.
    ENDIF.

    IF popup_settings-end_col IS INITIAL.
      popup_settings-end_col = 120.
    ENDIF.

    IF popup_settings-end_row IS INITIAL.
      popup_settings-end_row = 25.
    ENDIF.

    IF popup_settings-title IS INITIAL.
      popup_settings-title = TEXT-t01.
    ENDIF.

    CALL FUNCTION 'ZFM_COMM_SECT_DISPLAY'
      EXPORTING i_comment_system = me
                i_settings       = popup_settings.
  ENDMETHOD.

  METHOD zif_cmt_system~free.
    m_sec_viewer->free( ).
    CLEAR m_sec_viewer.

    m_sec_container->free( ).
    CLEAR m_sec_container.

    m_mes_viewer->free( ).
    CLEAR m_mes_viewer.

    m_mes_container->free( ).
    CLEAR m_mes_container.
  ENDMETHOD.

  METHOD zif_cmt_system~like_comment.
    READ TABLE m_comments ASSIGNING FIELD-SYMBOL(<com>) WITH KEY comment_id = comment_id.
    CHECK sy-subrc = 0.

    INSERT sy-uname INTO TABLE <com>-user_upvoting.

    INSERT zcmt_upvotes_t FROM @( VALUE #( comment_id = comment_id user_upvoting = sy-uname ) ).
  ENDMETHOD.

  METHOD zif_cmt_system~load_theme.
    m_theme = zcl_cmt_theme=>get( ).
  ENDMETHOD.

  METHOD zif_cmt_system~mark_as_read.
    DATA read_by TYPE TABLE OF zcmt_comread_t.

    LOOP AT comments ASSIGNING FIELD-SYMBOL(<comment>) WHERE comment_author <> sy-uname.

      CHECK NOT line_exists( <comment>-read_by[ table_line = sy-uname ] ).

      INSERT sy-uname INTO TABLE <comment>-read_by.
      INSERT VALUE #( comment_id = <comment>-comment_id
                      user_read  = sy-uname ) INTO TABLE read_by.

    ENDLOOP.

    CHECK read_by IS NOT INITIAL.

    INSERT zcmt_comread_t FROM TABLE @read_by.
  ENDMETHOD.

  METHOD zif_cmt_system~notify_to_read.
    result = REDUCE #(
      INIT i = 0
      FOR _c IN m_comments WHERE ( comment_author <> sy-uname )
      LET user_read = xsdbool( line_exists( _c-read_by[ table_line = sy-uname ] ) ) IN
      NEXT i = COND #( WHEN user_read = abap_true THEN i ELSE i + 1 ) ).
  ENDMETHOD.

  METHOD zif_cmt_system~render_comments.
    zif_cmt_system~mark_as_read( CHANGING comments = m_comments ).

    result = m_theme->render_comments( m_comments ).
  ENDMETHOD.

  METHOD zif_cmt_system~render_textarea.
    result = m_theme->render_textarea( ).
  ENDMETHOD.

  METHOD _display_comment_section.
    DATA sec_url TYPE c LENGTH 1024.

    DATA(sec_html) = zif_cmt_system~render_comments( ).

    IF m_sec_viewer IS BOUND.

      m_sec_viewer->load_data( IMPORTING assigned_url = sec_url
                               CHANGING  data_table   = sec_html ).
      m_sec_viewer->show_url( sec_url ).

      RETURN.

    ENDIF.

    m_sec_container = NEW cl_gui_custom_container( section_container ).

    m_sec_viewer    = NEW cl_gui_html_viewer( m_sec_container ).
    m_sec_viewer->set_registered_events( VALUE #( ( eventid = m_sec_viewer->m_id_sapevent appl_event = abap_true ) ) ).
    SET HANDLER _section_events FOR m_sec_viewer.

    m_sec_viewer->load_data( IMPORTING assigned_url = sec_url
                             CHANGING  data_table   = sec_html ).

    m_sec_viewer->show_url( sec_url ).
  ENDMETHOD.

  METHOD _display_textarea.
    DATA mes_url TYPE c LENGTH 1024.

    DATA(mes_html) = zif_cmt_system~render_textarea( ).

    IF m_mes_viewer IS BOUND.

      m_mes_viewer->load_data( IMPORTING assigned_url = mes_url
                               CHANGING  data_table   = mes_html ).
      m_mes_viewer->show_url( mes_url ).

      RETURN.

    ENDIF.

    m_mes_container = NEW cl_gui_custom_container( textarea_container ).

    m_mes_viewer = NEW cl_gui_html_viewer( m_mes_container ).
    m_mes_viewer->set_registered_events( VALUE #( ( eventid = m_mes_viewer->m_id_sapevent appl_event = abap_true ) ) ).
    SET HANDLER _text_area_events FOR m_mes_viewer.

    m_mes_viewer->load_data( IMPORTING assigned_url = mes_url
                             CHANGING  data_table   = mes_html ).

    m_mes_viewer->show_url( mes_url ).
  ENDMETHOD.

  METHOD _hash.
    DATA xml TYPE string.

    CALL TRANSFORMATION id
         OPTIONS data_refs = 'heap-or-create' technical_types = 'ignore'
         SOURCE result = data
         RESULT XML xml.

    CALL FUNCTION 'CALCULATE_HASH_FOR_CHAR'
      EXPORTING  data           = xml
      IMPORTING  hash           = result
      EXCEPTIONS unknown_alg    = 1
                 param_error    = 2
                 internal_error = 3
                 OTHERS         = 4.
  ENDMETHOD.

  METHOD _load_comments.
    DATA(db_comments) = get_manager( )->load_comments( m_section_id ).
    DATA(db_upvotes)  = get_manager( )->load_upvotes( db_comments ).
    DATA(db_read_by)  = get_manager( )->load_readby( db_comments ).

    CLEAR m_comments.
    MOVE-CORRESPONDING db_comments TO m_comments.
    SORT m_comments BY comment_date DESCENDING
                       comment_time DESCENDING.

    LOOP AT db_upvotes INTO DATA(upvote).

      READ TABLE m_comments ASSIGNING FIELD-SYMBOL(<com>) WITH KEY comment_id = upvote-comment_id.
      CHECK sy-subrc = 0.

      INSERT upvote-user_upvoting INTO TABLE <com>-user_upvoting.

    ENDLOOP.

    LOOP AT db_read_by INTO DATA(rby).

      READ TABLE m_comments ASSIGNING <com> WITH KEY comment_id = rby-comment_id.
      CHECK sy-subrc = 0.

      INSERT rby-user_read INTO TABLE <com>-read_by.

    ENDLOOP.
  ENDMETHOD.

  METHOD _section_events.
    CASE action.

      WHEN `DELETE`.

        zif_cmt_system~delete_comment( CONV #( getdata ) ).

      WHEN `LIKE`.

        zif_cmt_system~like_comment( CONV #( getdata ) ).

      WHEN `DISLIKE`.

        zif_cmt_system~dislike_comment( CONV #( getdata ) ).

    ENDCASE.
  ENDMETHOD.

  METHOD _text_area_events.
    DATA(comment) = VALUE #( query_table[ name = `comment` ]-value OPTIONAL ).
    CHECK comment IS NOT INITIAL.

    zif_cmt_system~add_comment( comment ).
  ENDMETHOD.
ENDCLASS.
