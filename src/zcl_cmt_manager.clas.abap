CLASS zcl_cmt_manager DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_cmt_manager.

    CLASS-METHODS get_manager
      RETURNING VALUE(result) TYPE REF TO zif_cmt_manager.

  PRIVATE SECTION.
    CLASS-DATA m_manager TYPE REF TO zif_cmt_manager.

    DATA m_users_full_name TYPE zif_cmt_manager=>tt_users_name.
ENDCLASS.


CLASS zcl_cmt_manager IMPLEMENTATION.
  METHOD get_manager.
    IF m_manager IS NOT BOUND.
      m_manager = NEW zcl_cmt_manager( ).
    ENDIF.

    result = m_manager.
  ENDMETHOD.

  METHOD zif_cmt_manager~create_id.
    TRY.

        result = cl_system_uuid=>create_uuid_c26_static( ).

      CATCH cx_uuid_error.

    ENDTRY.
  ENDMETHOD.

  METHOD zif_cmt_manager~get_user_full_name.
    DATA address TYPE bapiaddr3.
    DATA ret     TYPE bapiret2_t.

    READ TABLE m_users_full_name ASSIGNING FIELD-SYMBOL(<user>) WITH KEY user_id = user_id.

    IF sy-subrc <> 0.

      APPEND INITIAL LINE TO m_users_full_name ASSIGNING <user>.
      <user>-user_id = user_id.

      CALL FUNCTION 'BAPI_USER_GET_DETAIL'
        EXPORTING
          username = user_id
        IMPORTING
          address  = address
        TABLES
          return   = ret.

      <user>-user_name = |{ address-firstname } { address-lastname }|.

    ENDIF.

    result = <user>-user_name.
  ENDMETHOD.

  METHOD zif_cmt_manager~load_comments.
    SELECT * FROM zcmt_comment_t
      WHERE section_id = @section_id
      INTO TABLE @result.
  ENDMETHOD.

  METHOD zif_cmt_manager~load_readby.
    CHECK comments IS NOT INITIAL.

    SELECT * FROM zcmt_comread_t
      FOR ALL ENTRIES IN @comments
      WHERE comment_id = @comments-comment_id
      INTO TABLE @result.
  ENDMETHOD.

  METHOD zif_cmt_manager~load_upvotes.
    CHECK comments IS NOT INITIAL.

    SELECT * FROM zcmt_upvotes_t
      FOR ALL ENTRIES IN @comments
      WHERE comment_id = @comments-comment_id
      INTO TABLE @result.
  ENDMETHOD.
ENDCLASS.
